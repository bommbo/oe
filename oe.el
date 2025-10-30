;;; oe.el --- oil.nvim-like editable directory buffer -*- lexical-binding: t; -*-

(require 'dired)
(require 'cl-lib)

;;; ============================================================================
;;; Customs
;;; ============================================================================

(defgroup oe nil
  "Oe file manager settings."
  :group 'files
  :prefix "oe-")

(defcustom oe-default-tree-mode nil
  "Non-nil means Oe buffers start in tree mode by default."
  :type 'boolean
  :group 'oe)

(defcustom oe-history-limit 100
  "Maximum number of directories to keep in history."
  :type 'integer
  :group 'oe)

(defcustom oe-history-deduplicate 'adjacent
  "How to deduplicate history.
- nil: no deduplication
- 'adjacent: remove adjacent duplicates only
- 'all: remove all duplicates (keep latest)"
  :type '(choice (const :tag "No deduplication" nil)
				 (const :tag "Adjacent only" adjacent)
				 (const :tag "All duplicates" all))
  :group 'oe)

(defcustom oe-preview-size 5000
  "Maximum number of bytes to preview from files."
  :type 'integer
  :group 'oe)

;;; ============================================================================
;;; Variables and Faces
;;; ============================================================================

(defvar oe-marks-file (expand-file-name "oe-marks.el" user-emacs-directory)
  "File to persist Oe marks across sessions.")

(defvar oe--marks nil
  "Alist of (NAME . PATH) for marked directories and files.")

(defvar oe--clipboard nil
  "List of file paths in clipboard for copy/cut operations.")

(defvar oe--clipboard-mode nil
  "Mode for clipboard: 'copy or 'cut.")

(defvar oe--directory-states (make-hash-table :test 'equal)
  "Per-directory state storage.
Key: directory path (string)
Value: plist with :folded-dirs :selected-files :tree-mode :cursor-path :cursor-line :cursor-column")

(defvar-local oe--dir nil
  "The directory this Oe buffer represents.")

(defvar-local oe--orig-buf nil
  "The Dired buffer that opened this Oe buffer.")

(defvar-local oe--orig-entries nil
  "List of original entries when buffer was opened.")

(defvar-local oe--tree-mode nil
  "Whether tree mode is enabled.")

(defvar-local oe--folded-dirs nil
  "Hash table of folded directory paths in tree mode.")

(defvar-local oe--selected-files nil
  "List of selected files for operations.")

(defvar-local oe--history nil
  "Navigation history: list of directory paths.")

(defvar-local oe--history-index -1
  "Current position in navigation history.")

(defface oe-directory-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for directory names.")

(defface oe-file-face
  '((t :inherit default))
  "Face for file names.")

(defface oe-tree-indent-face
  '((t :inherit font-lock-comment-face))
  "Face for tree indentation lines.")

(defface oe-marked-face
  '((t :inherit warning :weight bold))
  "Face for marked files/directories.")

(defface oe-selected-face
  '((t :inherit region))
  "Face for selected files.")

;;; ============================================================================
;;; Keymap
;;; ============================================================================

(defvar oe-mode-map
  (let ((map (make-sparse-keymap)))
	;; Common keys (always available)
	(define-key map (kbd "C-c C-j") #'oe-jump)
	(define-key map (kbd "C-c C-t") #'oe-toggle-tree-mode)
	(define-key map (kbd "C-c C-k") #'oe-cancel)
	(define-key map (kbd "q") #'oe-quit)
	;; History navigation
	(define-key map (kbd "C-c C-b") #'oe-history-back)
	(define-key map (kbd "C-c C-f") #'oe-history-forward)
	map)
  "Keymap for `oe-mode'.")

(defvar oe-tree-mode-map
  (let ((map (make-sparse-keymap)))
	;; Navigation
	(define-key map (kbd "RET") #'oe-enter)
	(define-key map (kbd "^") #'oe-up)
	(define-key map (kbd "TAB") #'oe-toggle-fold)
	;; Selection
	(define-key map (kbd "SPC") #'oe-toggle-selection)
	(define-key map (kbd "*") #'oe-selecte-all)
	(define-key map (kbd "t") #'oe-toggle-all-selections)
	;; File operations
	(define-key map (kbd "C") #'oe-copy)
	(define-key map (kbd "X") #'oe-cut)
	(define-key map (kbd "P") #'oe-paste)
	(define-key map (kbd "D") #'oe-delete-selected)
	;; Marks
	(define-key map (kbd "m") #'oe-mark)
	(define-key map (kbd "u") #'oe-unmark)
	(define-key map (kbd "U") #'oe-unmark-all)
	(define-key map (kbd "C-c m") #'oe-show-marks)
	(define-key map (kbd "C-c g") #'oe-goto-mark)
	;; Preview
	(define-key map (kbd "p") #'oe-preview)
	map)
  "Keymap for tree mode operations.")

(defvar oe-edit-mode-map
  (let ((map (make-sparse-keymap)))
	;; Edit-specific keys
	(define-key map (kbd "C-c C-c") #'oe-save)
	map)
  "Keymap for edit mode operations.")

;;; ============================================================================
;;; State Management
;;; ============================================================================

(defun oe--save-current-state ()
  "Save current directory state to global state table."
  (when (and oe--dir (file-directory-p oe--dir))
	(puthash oe--dir
			 (list :folded-dirs (when oe--folded-dirs
								  ;; Convert hash table to alist for storage
								  (let ((alist nil))
									(maphash (lambda (k v) (push (cons k v) alist))
											 oe--folded-dirs)
									alist))
				   :selected-files oe--selected-files
				   :tree-mode oe--tree-mode
				   :cursor-path (when (fboundp 'oe--get-path-at-point)
								  (oe--get-path-at-point))
				   :cursor-line (line-number-at-pos)
				   :cursor-column (current-column))
			 oe--directory-states)))

(defun oe--restore-state (dir)
  "Restore state for DIR from global state table."
  (let ((state (gethash dir oe--directory-states)))
	(when state
	  ;; Restore fold states
	  (let ((fold-alist (plist-get state :folded-dirs)))
		(if fold-alist
			(progn
			  (setq oe--folded-dirs (make-hash-table :test 'equal))
			  (dolist (pair fold-alist)
				(puthash (car pair) (cdr pair) oe--folded-dirs)))
		  (setq oe--folded-dirs (make-hash-table :test 'equal))))

	  ;; Restore selections
	  (setq oe--selected-files (plist-get state :selected-files))

	  ;; Restore tree mode
	  (setq oe--tree-mode (plist-get state :tree-mode))

	  ;; Return cursor position for later restoration
	  (list :cursor-path (plist-get state :cursor-path)
			:cursor-line (plist-get state :cursor-line)
			:cursor-column (plist-get state :cursor-column)))))

(defun oe--restore-cursor (cursor-info)
  "Restore cursor position from CURSOR-INFO."
  (when cursor-info
	(let ((cursor-path (plist-get cursor-info :cursor-path))
		  (cursor-line (plist-get cursor-info :cursor-line))
		  (cursor-col (plist-get cursor-info :cursor-column)))
	  (cond
	   ;; Try to find the exact path
	   ((and cursor-path (file-exists-p cursor-path))
		(goto-char (point-min))
		(let ((found nil))
		  (while (and (not found) (not (eobp)))
			(when (and (oe--get-path-at-point)
					   (string= (oe--get-path-at-point) cursor-path))
			  (setq found t))
			(unless found (forward-line 1)))
		  (when found
			(move-to-column (or cursor-col 0)))))
	   ;; Fallback to line number
	   (cursor-line
		(goto-char (point-min))
		(forward-line (1- cursor-line))
		(move-to-column (or cursor-col 0)))
	   ;; Default to first entry
	   (t
		(goto-char (point-min))
		(forward-line 1))))))

(defun oe--add-to-history (dir)
  "Add DIR to navigation history with deduplication."
  (setq dir (expand-file-name dir))

  ;; Skip if same as current position
  (unless (and (>= oe--history-index 0)
			   (< oe--history-index (length oe--history))
			   (string= dir (nth oe--history-index oe--history)))

	;; Truncate history after current position
	(when (>= oe--history-index 0)
	  (setq oe--history (seq-take oe--history (1+ oe--history-index))))

	;; Deduplicate based on setting
	(cond
	 ((eq oe-history-deduplicate 'all)
	  ;; Remove all occurrences of this dir
	  (setq oe--history (delete dir oe--history)))

	 ((eq oe-history-deduplicate 'adjacent)
	  ;; Remove if last item is the same
	  (when (and oe--history
				 (string= dir (car (last oe--history))))
		(setq oe--history (butlast oe--history)))))

	;; Add new directory
	(setq oe--history (append oe--history (list dir)))

	;; Limit history size
	(when (> (length oe--history) oe-history-limit)
	  (let ((overflow (- (length oe--history) oe-history-limit)))
		(setq oe--history (seq-drop oe--history overflow))))

	;; Update index
	(setq oe--history-index (1- (length oe--history)))))

;;; ============================================================================
;;; Mark Persistence
;;; ============================================================================

(defun oe--load-marks ()
  "Load marks from persistent storage."
  (when (file-exists-p oe-marks-file)
	(condition-case err
		(with-temp-buffer
		  (insert-file-contents oe-marks-file)
		  (let ((marks (read (buffer-string))))
			(when (listp marks)
			  (setq oe--marks marks))))
	  (error
	   (message "Error loading marks: %s" (error-message-string err))
	   (setq oe--marks nil))))
  (unless oe--marks
	(setq oe--marks nil)))

(defun oe--save-marks ()
  "Save marks to persistent storage."
  (condition-case err
	  (with-temp-file oe-marks-file
		(prin1 oe--marks (current-buffer)))
	(error
	 (message "Error saving marks: %s" (error-message-string err)))))

(defun oe--is-marked-p (path)
  "Check if PATH is marked."
  (and path (cl-find path oe--marks :key #'cdr :test #'string=)))

(defun oe--is-selected-p (path)
  "Check if PATH is selected."
  (and path (member path oe--selected-files)))

;;; ============================================================================
;;; Icons and Display
;;; ============================================================================

(defun oe--icon (path is-dir folded)
  "Return icon for PATH. IS-DIR indicates directory. FOLDED for collapsed state."
  (cond
   ((and is-dir folded) "▶")
   (is-dir "▼")
   ((string-match-p "\\.el$" path) "📜")
   ((string-match-p "\\.org$" path) "📋")
   ((string-match-p "\\.md$" path) "📝")
   ((string-match-p "\\.txt$" path) "📄")
   ((string-match-p "\\.[ch]\\(pp\\)?$" path) "🔧")
   ((string-match-p "\\.\\(py\\|rb\\|js\\|ts\\)$" path) "🐍")
   ((string-match-p "\\.\\(jpg\\|png\\|gif\\|svg\\)$" path) "🖼️")
   ((string-match-p "\\.\\(mp3\\|wav\\|flac\\)$" path) "🎵")
   ((string-match-p "\\.\\(mp4\\|avi\\|mkv\\)$" path) "🎬")
   ((string-match-p "\\.\\(zip\\|tar\\|gz\\|7z\\)$" path) "📦")
   ((string-match-p "\\.pdf$" path) "📕")
   (t "📄")))

;;; ============================================================================
;;; Tree View
;;; ============================================================================

(defun oe--tree-insert-entry (path depth is-last parent-path)
  "Insert tree entry for PATH at DEPTH. IS-LAST indicates last child."
  (when (and path (file-exists-p path))
	(let* ((name (file-name-nondirectory path))
		   (is-dir (file-directory-p path))
		   (fold-state (gethash path oe--folded-dirs))
		   (folded (not (eq fold-state t)))
		   (icon (oe--icon name is-dir folded))
		   (marked (oe--is-marked-p path))
		   (selected (oe--is-selected-p path))
		   (face (cond
				  (marked 'oe-marked-face)
				  (selected 'oe-selected-face)
				  (is-dir 'oe-directory-face)
				  (t 'oe-file-face)))
		   (mark-indicator (if marked "★ " ""))
		   (select-indicator (if selected "• " ""))
		   (prefix (if (> depth 0)
					   (concat (make-string (* depth 2) ?\s)
							   (propertize (if is-last "└─ " "├─ ")
										 'face 'oe-tree-indent-face))
					 ""))
		   (line-content (format "%s%s%s%s %s\n" prefix select-indicator mark-indicator icon name))
		   (start (point)))
	  (insert line-content)
	  (put-text-property start (point) 'oe-path path)
	  (put-text-property start (point) 'oe-depth depth)
	  (put-text-property start (point) 'oe-is-dir is-dir)
	  (put-text-property start (point) 'oe-parent parent-path)
	  (put-text-property start (point) 'face face)
	  (when (and is-dir (eq fold-state t))
		(let* ((entries (condition-case nil
							(directory-files path t directory-files-no-dot-files-regexp)
						  (error nil)))
			   (sorted (sort (copy-sequence entries) #'string<))
			   (count (length sorted)))
		  (cl-loop for entry in sorted
				   for idx from 0
				   do (oe--tree-insert-entry entry (1+ depth) (= idx (1- count)) path)))))))

(defun oe--insert-dir-tree (dir)
  "Insert directory tree for DIR."
  (when (and dir (file-directory-p dir))
	(let ((old-point (point))
		  (inhibit-read-only t))
	  (erase-buffer)
	  (unless oe--folded-dirs
		(setq oe--folded-dirs (make-hash-table :test 'equal)))
	  (let ((entries (condition-case nil
						 (directory-files dir t directory-files-no-dot-files-regexp)
					   (error nil))))
		(dolist (entry entries)
		  (when (and (file-directory-p entry)
					 (not (gethash entry oe--folded-dirs)))
			(puthash entry 'default-folded oe--folded-dirs))))
	  (insert (propertize (format "# Oe Tree: %s\n" dir) 'face 'font-lock-comment-face))
	  (let* ((entries (condition-case nil
						  (directory-files dir t directory-files-no-dot-files-regexp)
						(error nil)))
			 (sorted (sort (copy-sequence entries) #'string<))
			 (count (length sorted)))
		(cl-loop for entry in sorted
				 for idx from 0
				 do (oe--tree-insert-entry entry 0 (= idx (1- count)) dir)))
	  (goto-char (point-max))
	  (insert (propertize "\n# RET=open ^=up SPC=select t=toggle C=copy X=cut P=paste\n"
						  'face 'font-lock-comment-face))
	  (goto-char (min old-point (point-max)))
	  (when (< (point) (point-min))
		(goto-char (point-min))
		(forward-line 1)))))

;;; ============================================================================
;;; Flat View
;;; ============================================================================

(defun oe--insert-dir-flat (dir)
  "Insert flat directory listing for DIR."
  (when (and dir (file-directory-p dir))
	(let ((old-point (point))
		  (inhibit-read-only t))
	  (erase-buffer)
	  (insert (propertize (format "# Oe Edit: %s\n" dir) 'face 'font-lock-comment-face))
	  (let ((entries (condition-case nil
						 (directory-files dir nil directory-files-no-dot-files-regexp)
					   (error nil)))
			(formatted-entries nil))
		(dolist (entry entries)
		  (let* ((path (expand-file-name entry dir))
				 (is-dir (file-directory-p path))
				 (display-name (if is-dir (concat entry "/") entry))
				 (icon (oe--icon entry is-dir nil))
				 (marked (oe--is-marked-p path))
				 (face (if marked 'oe-marked-face
						 (if is-dir 'oe-directory-face 'oe-file-face)))
				 (mark-indicator (if marked "★ " "")))
			(push display-name formatted-entries)
			(insert (propertize (format "%s%s %s\n" mark-indicator icon display-name) 'face face))))
		(setq oe--orig-entries (nreverse formatted-entries)))
	  (goto-char (point-max))
	  (insert (propertize "\n# Edit file names above, then C-c C-c to save changes\n"
						  'face 'font-lock-comment-face))
	  (insert (propertize "# Add lines to create files/dirs (end with / for dirs)\n"
						  'face 'font-lock-comment-face))
	  (insert (propertize "# Delete lines to remove files/dirs, rename by editing names\n"
						  'face 'font-lock-comment-face))
	  (goto-char (min old-point (point-max)))
	  (when (< (point) (point-min))
		(goto-char (point-min))
		(forward-line 1)))))

(defun oe--insert-dir (dir)
  "Insert directory listing for DIR based on current mode."
  (when (and dir (file-directory-p dir))
	(if oe--tree-mode
		(oe--insert-dir-tree dir)
	  (oe--insert-dir-flat dir))))

;;; ============================================================================
;;; Entry Point
;;; ============================================================================

;;;###autoload
(defun oe-open (dir &optional keep-mode)
  "Open DIR in editable Oe buffer.
If KEEP-MODE is non-nil, preserve current tree/edit mode."
  (interactive "DDirectory: ")
  (setq dir (expand-file-name dir))
  (unless (file-directory-p dir)
	(user-error "Not a directory: %s" dir))
  (unless oe--marks
	(oe--load-marks))

  ;; Use single buffer
  (let ((buf (get-buffer-create "*oe*"))
		(was-same-dir (and (buffer-live-p (get-buffer "*oe*"))
						   (with-current-buffer "*oe*"
							 (and (boundp 'oe--dir)
								  (string= oe--dir dir))))))

	;; Don't save state if we're already viewing this directory
	(unless was-same-dir
	  ;; Save current state if in Oe buffer
	  (when (and (buffer-live-p (get-buffer "*oe*"))
				 (with-current-buffer "*oe*"
				   (and (boundp 'oe--dir) oe--dir)))
		(with-current-buffer "*oe*"
		  (oe--save-current-state))))

	(with-current-buffer buf
	  (oe-mode)

	  ;; Add to history (unless we're already viewing this directory)
	  (unless was-same-dir
		(oe--add-to-history dir))

	  ;; Restore or initialize state
	  (let ((cursor-info (oe--restore-state dir)))
		;; If keep-mode is nil and we have saved state, use saved tree-mode
		;; Otherwise use keep-mode or default
		(unless keep-mode
		  (unless cursor-info
			(setq oe--tree-mode oe-default-tree-mode)
			(setq oe--folded-dirs (make-hash-table :test 'equal))
			(setq oe--selected-files nil)))

		(setq oe--dir dir
			  oe--orig-buf (current-buffer))

		(oe--update-keymap)
		(oe--insert-dir dir)

		;; Restore cursor position
		(when cursor-info
		  (oe--restore-cursor cursor-info)))

	  ;; Track in session if available
	  (when (fboundp 'oe-session-add-recent)
		(oe-session-add-recent dir)))

	(switch-to-buffer buf)))

;;;###autoload
(defun oe-dired-here ()
  "Open current dired directory in Oe."
  (interactive)
  (if (eq major-mode 'dired-mode)
	  (oe-open default-directory)
	(user-error "Not in dired buffer")))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun oe--get-path-at-point ()
  "Get file path at point."
  (if oe--tree-mode
	  (get-text-property (point) 'oe-path)
	(save-excursion
	  (beginning-of-line)
	  (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
			 (name-raw (replace-regexp-in-string "^[★ ]*[^ ]+ " "" line))
			 (name (string-trim-left name-raw)))
		(when (and (not (string-empty-p name))
				   (not (string-prefix-p "#" name)))
		  ;; Remove trailing / for path construction
		  (let ((clean-name (if (string-suffix-p "/" name)
								(substring name 0 -1)
							  name)))
			(expand-file-name clean-name oe--dir)))))))

(defun oe--get-line-info ()
  "Get line information at point."
  (save-excursion
	(beginning-of-line)
	(let ((path (get-text-property (point) 'oe-path)))
	  (when (not path)
		(let ((line-end (line-end-position)))
		  (while (and (< (point) line-end) (not path))
			(forward-char 1)
			(setq path (get-text-property (point) 'oe-path)))))
	  (list :path path
			:depth (get-text-property (point) 'oe-depth)
			:is-dir (get-text-property (point) 'oe-is-dir)
			:parent (get-text-property (point) 'oe-parent)))))

(defun oe--lines ()
  "Return all non-empty, non-comment lines, stripped of decorations but preserving trailing /."
  (let ((lines (split-string (buffer-string) "\n" t)))
	(delq nil
		  (mapcar (lambda (line)
					(let ((trimmed (string-trim line)))
					  ;; Skip comments and empty lines
					  (when (and (not (string-empty-p trimmed))
								 (not (string-prefix-p "#" trimmed)))
						;; Remove decorations but keep trailing /
						(let* ((no-marks (replace-regexp-in-string "^[★• ]+" "" trimmed))
							   (no-tree (replace-regexp-in-string "^[│├└─ ]+" "" no-marks))
							   (no-icon (replace-regexp-in-string "^[^ ]+ " "" no-tree)))
						  ;; Only trim left side to preserve trailing /
						  (string-trim-left no-icon)))))
				  lines))))

;;; ============================================================================
;;; Selection Commands (Tree Mode Only)
;;; ============================================================================

(defun oe-toggle-selection ()
  "Toggle selection of file/directory at point."
  (interactive)
  (let ((path (oe--get-path-at-point))
		(line (line-number-at-pos)))
	(if (not path)
		(message "No file at point")
	  (if (oe--is-selected-p path)
		  (setq oe--selected-files (delete path oe--selected-files))
		(push path oe--selected-files))
	  (let ((inhibit-read-only t))
		(oe--insert-dir oe--dir))
	  ;; Restore position and move down
	  (goto-char (point-min))
	  (forward-line line)
	  (recenter))))

(defun oe-selecte-all ()
  "Select all files in current directory."
  (interactive)
  (let ((count 0)
		(current-line (line-number-at-pos)))
	(save-excursion
	  (goto-char (point-min))
	  (while (not (eobp))
		(let ((path (oe--get-path-at-point)))
		  (when (and path (not (oe--is-selected-p path)))
			(push path oe--selected-files)
			(setq count (1+ count))))
		(forward-line 1)))
	(when (> count 0)
	  (let ((inhibit-read-only t))
		(oe--insert-dir oe--dir))
	  (goto-char (point-min))
	  (forward-line (1- current-line))
	  (recenter)
	  (message "Selected %d items" count))))

(defun oe-toggle-all-selections ()
  "Toggle all selections."
  (interactive)
  (let ((current-line (line-number-at-pos)))
	(if oe--selected-files
		(progn
		  (setq oe--selected-files nil)
		  (let ((inhibit-read-only t))
			(oe--insert-dir oe--dir))
		  (goto-char (point-min))
		  (forward-line (1- current-line))
		  (recenter)
		  (message "Cleared all selections"))
	  (oe-selecte-all))))

;;; ============================================================================
;;; Copy/Cut/Paste Commands (Tree Mode Only)
;;; ============================================================================

(defun oe-copy ()
  "Copy selected files to clipboard."
  (interactive)
  (let ((files (or oe--selected-files
				   (let ((p (oe--get-path-at-point)))
					 (when p (list p))))))
	(if (not files)
		(message "No files to copy")
	  (setq oe--clipboard files
			oe--clipboard-mode 'copy)
	  (message "Copied %d item(s)" (length files)))))

(defun oe-cut ()
  "Cut selected files to clipboard."
  (interactive)
  (let ((files (or oe--selected-files
				   (let ((p (oe--get-path-at-point)))
					 (when p (list p))))))
	(if (not files)
		(message "No files to cut")
	  (setq oe--clipboard files
			oe--clipboard-mode 'cut)
	  (message "Cut %d item(s)" (length files)))))

(defun oe-paste ()
  "Paste files from clipboard to current directory."
  (interactive)
  (unless oe--clipboard
	(user-error "Clipboard is empty"))
  (let ((target-dir oe--dir)
		(count 0)
		(total (length oe--clipboard))
		(errors nil)
		(inhibit-read-only t)
		(current-line (line-number-at-pos)))
	(dolist (src oe--clipboard)
	  (when (file-exists-p src)
		(let* ((name (file-name-nondirectory src))
			   (dest (expand-file-name name target-dir)))
		  ;; Show progress for each file
		  (message "Pasting %d/%d: %s" (1+ count) total name)
		  (condition-case err
			  (progn
				(if (eq oe--clipboard-mode 'cut)
					(rename-file src dest)
				  (if (file-directory-p src)
					  (copy-directory src dest nil t t)
					(copy-file src dest)))
				(setq count (1+ count)))
			(error
			 (push (format "%s: %s" name (error-message-string err)) errors))))))
	(when (eq oe--clipboard-mode 'cut)
	  (setq oe--clipboard nil
			oe--clipboard-mode nil))
	(setq oe--selected-files nil)
	(oe--insert-dir target-dir)
	(goto-char (point-min))
	(forward-line (1- current-line))
	(recenter)
	(if errors
		(message "Pasted %d/%d items (%d errors)" count total (length errors))
	  (message "Pasted %d items" count))))

(defun oe-delete-selected ()
  "Delete selected files."
  (interactive)
  (let ((files (or oe--selected-files
				   (let ((p (oe--get-path-at-point)))
					 (when p (list p)))))
		(inhibit-read-only t)
		(current-line (line-number-at-pos)))
	(unless files
	  (user-error "No files selected"))
	(when (yes-or-no-p (format "Delete %d item(s)? " (length files)))
	  (let ((count 0)
			(total (length files))
			(errors nil))
		(dolist (path files)
		  ;; Show progress
		  (message "Deleting %d/%d: %s" (1+ count) total (file-name-nondirectory path))
		  (condition-case err
			  (progn
				(if (file-directory-p path)
					(delete-directory path t)
				  (delete-file path))
				(setq count (1+ count)))
			(error
			 (push (format "%s: %s" path (error-message-string err)) errors))))
		(setq oe--selected-files nil)
		(oe--insert-dir oe--dir)
		(goto-char (point-min))
		(forward-line (1- current-line))
		(recenter)
		(if errors
			(message "Deleted %d/%d items (%d errors)" count total (length errors))
		  (message "Deleted %d items" count))))))

;;; ============================================================================
;;; Navigation Commands (Tree Mode Only)
;;; ============================================================================

(defun oe-enter ()
  "Open file or directory at point."
  (interactive)
  (let* ((info (oe--get-line-info))
		 (path (plist-get info :path))
		 (is-dir (plist-get info :is-dir))
		 (inhibit-read-only t))
	(when path
	  (if is-dir
		  (oe-open path t)  ; Keep current mode
		(find-file path)))))

(defun oe-up ()
  "Go up to parent directory."
  (interactive)
  (let ((parent (file-name-directory (directory-file-name oe--dir)))
		(inhibit-read-only t))
	(if (not parent)
		(message "Already at root directory")
	  (oe-open parent t))))  ; Keep current mode

(defun oe--zoxide-query ()
  "Query zoxide for directory list."
  (when (executable-find "zoxide")
	(condition-case nil
		(let ((output (shell-command-to-string "zoxide query -l 2>/dev/null")))
		  (split-string output "\n" t))
	  (error nil))))

(defun oe-jump ()
  "Jump to directory using zoxide or read-directory-name."
  (interactive)
  (let* ((zoxide-paths (oe--zoxide-query))
		 (selected-dir
		  (if zoxide-paths
			  (completing-read "Jump to directory: " zoxide-paths nil nil)
			(read-directory-name "Jump to directory: " oe--dir))))
	(when (and selected-dir (not (string-empty-p selected-dir)))
	  (if (file-directory-p selected-dir)
		  (progn
			(oe-open selected-dir t)  ; Keep current mode
			(message "Jumped to: %s" selected-dir))
		(message "Not a directory: %s" selected-dir)))))

(defun oe-quit ()
  "Quit Oe buffer."
  (interactive)
  ;; Save state before quitting
  (oe--save-current-state)
  (kill-buffer (current-buffer)))

;;; ============================================================================
;;; History Navigation
;;; ============================================================================

(defun oe-history-back ()
  "Go back in navigation history."
  (interactive)
  (if (or (not oe--history) (<= oe--history-index 0))
	  (message "No previous directory in history")
	(setq oe--history-index (1- oe--history-index))
	(let ((dir (nth oe--history-index oe--history)))
	  (if (not (file-directory-p dir))
		  (progn
			(message "Directory no longer exists: %s" dir)
			(oe-history-back))
		;; Save current state
		(oe--save-current-state)
		;; Navigate without adding to history
		(let ((cursor-info (oe--restore-state dir)))
		  (setq oe--dir dir)
		  (oe--update-keymap)
		  (let ((inhibit-read-only t))
			(oe--insert-dir dir))
		  (when cursor-info
			(oe--restore-cursor cursor-info))
		  (message "Back: %s [%d/%d]" dir (1+ oe--history-index) (length oe--history)))))))

(defun oe-history-forward ()
  "Go forward in navigation history."
  (interactive)
  (if (or (not oe--history) (>= oe--history-index (1- (length oe--history))))
	  (message "No next directory in history")
	(setq oe--history-index (1+ oe--history-index))
	(let ((dir (nth oe--history-index oe--history)))
	  (if (not (file-directory-p dir))
		  (progn
			(message "Directory no longer exists: %s" dir)
			(oe-history-forward))
		;; Save current state
		(oe--save-current-state)
		;; Navigate without adding to history
		(let ((cursor-info (oe--restore-state dir)))
		  (setq oe--dir dir)
		  (oe--update-keymap)
		  (let ((inhibit-read-only t))
			(oe--insert-dir dir))
		  (when cursor-info
			(oe--restore-cursor cursor-info))
		  (message "Forward: %s [%d/%d]" dir (1+ oe--history-index) (length oe--history)))))))

;;; ============================================================================
;;; History Navigation
;;; ============================================================================

(defun oe-toggle-fold ()
  "Toggle fold state of directory at point."
  (interactive)
  (if (not oe--tree-mode)
	  (message "Fold only works in tree mode")
	(let* ((info (oe--get-line-info))
		   (path (plist-get info :path))
		   (is-dir (plist-get info :is-dir))
		   (inhibit-read-only t)
		   (current-line (line-number-at-pos)))
	  (cond
	   ((not path)
		(message "No path on this line"))
	   ((not is-dir)
		(message "Not a directory"))
	   (t
		(unless oe--folded-dirs
		  (setq oe--folded-dirs (make-hash-table :test 'equal)))
		(let ((current-state (gethash path oe--folded-dirs)))
		  (if (eq current-state t)
			  (puthash path 'default-folded oe--folded-dirs)
			(puthash path t oe--folded-dirs))
		  (oe--insert-dir oe--dir)
		  (goto-char (point-min))
		  (forward-line (1- current-line))
		  (recenter)))))))

(defun oe-toggle-tree-mode ()
  "Toggle between tree and flat view modes."
  (interactive)
  (setq oe--tree-mode (not oe--tree-mode))
  (when oe--tree-mode
	(unless oe--folded-dirs
	  (setq oe--folded-dirs (make-hash-table :test 'equal))))
  (oe--update-keymap)
  (let ((inhibit-read-only t)
		(line (line-number-at-pos))
		(col (current-column)))
	(oe--insert-dir oe--dir)
	(goto-char (point-min))
	(forward-line (1- line))
	(move-to-column col))
  (message "%s mode" (if oe--tree-mode "Tree" "Edit")))

;;; ============================================================================
;;; Mark Commands (Persistent Bookmarks)
;;; ============================================================================

(defun oe-mark ()
  "Mark file or directory at point as persistent bookmark."
  (interactive)
  (let ((path (oe--get-path-at-point))
		(inhibit-read-only t)
		(current-line (line-number-at-pos)))
	(if (not path)
		(message "No file at point")
	  (let* ((default-name (file-name-nondirectory path))
			 (name (read-string (format "Bookmark name (default: %s): " default-name)
								nil nil default-name)))
		(when (not (string-empty-p name))
		  (setq oe--marks (cl-remove name oe--marks :key #'car :test #'string=))
		  (push (cons name path) oe--marks)
		  (oe--save-marks)
		  (oe--insert-dir oe--dir)
		  (goto-char (point-min))
		  (forward-line (1- current-line))
		  (recenter)
		  (message "Bookmarked: %s → %s" name path))))))

(defun oe-unmark ()
  "Remove bookmark from file or directory at point."
  (interactive)
  (let ((path (oe--get-path-at-point))
		(inhibit-read-only t)
		(current-line (line-number-at-pos)))
	(if (not path)
		(message "No file at point")
	  (if (not (oe--is-marked-p path))
		  (message "Not bookmarked")
		(setq oe--marks (cl-remove path oe--marks :key #'cdr :test #'string=))
		(oe--save-marks)
		(oe--insert-dir oe--dir)
		(goto-char (point-min))
		(forward-line (1- current-line))
		(recenter)
		(message "Bookmark removed")))))

(defun oe-unmark-all ()
  "Clear all bookmarks."
  (interactive)
  (when (yes-or-no-p (format "Clear all %d bookmarks? " (length oe--marks)))
	(let ((current-line (line-number-at-pos)))
	  (setq oe--marks nil)
	  (oe--save-marks)
	  (let ((inhibit-read-only t))
		(oe--insert-dir oe--dir))
	  (goto-char (point-min))
	  (forward-line (1- current-line))
	  (recenter)
	  (message "All bookmarks cleared"))))

(defun oe-show-marks ()
  "Show all bookmarks in temporary buffer."
  (interactive)
  (oe--load-marks)
  (if (not oe--marks)
	  (message "No bookmarks set")
	(let ((seen-names (make-hash-table :test 'equal))
		  (unique-marks nil))
	  (dolist (mark oe--marks)
		(unless (gethash (car mark) seen-names)
		  (puthash (car mark) t seen-names)
		  (push mark unique-marks)))
	  (setq oe--marks (nreverse unique-marks))
	  (oe--save-marks)
	  (with-current-buffer (get-buffer-create "*Oe Bookmarks*")
		(let ((inhibit-read-only t))
		  (erase-buffer)
		  (insert (propertize "Oe Bookmarks\n=============\n\n" 'face 'bold))
		  (dolist (mark oe--marks)
			(insert (format "★ %s\n  → %s\n\n" (car mark) (cdr mark))))
		  (goto-char (point-min))
		  (special-mode))
		(display-buffer (current-buffer))))))

(defun oe-goto-mark ()
  "Jump to bookmarked location."
  (interactive)
  (oe--load-marks)
  (if (not oe--marks)
	  (message "No bookmarks set")
	(let* ((choices (mapcar (lambda (m)
							  (format "%s → %s" (car m) (cdr m)))
							oe--marks))
		   (selected (completing-read "Go to bookmark: " choices nil t)))
	  (when selected
		(string-match "^\\(.*?\\) → " selected)
		(let* ((mark-name (match-string 1 selected))
			   (mark-entry (assoc mark-name oe--marks))
			   (path (cdr mark-entry)))
		  (when path
			(if (file-directory-p path)
				(oe-open path t)  ; Keep current mode
			  (let ((dir (file-name-directory path)))
				(oe-open dir t)
				(goto-char (point-min))
				(search-forward (file-name-nondirectory path) nil t)
				(beginning-of-line)))))))))

;;; ============================================================================
;;; Edit Commands (Edit Mode Only)
;;; ============================================================================

(defun oe-save ()
  "Apply changes: create, delete, rename files/directories."
  (interactive)
  (when oe--tree-mode
	(user-error "Cannot edit in tree mode. Press C-c C-t for edit mode"))
  (let* ((dir oe--dir)
		 (original oe--orig-entries)
		 (current (oe--lines))
		 (inhibit-read-only t)
		 (renamed-pairs nil)
		 (added nil)
		 (deleted nil))

	;; Build sets for quick lookup
	(let ((orig-set (make-hash-table :test 'equal))
		  (curr-set (make-hash-table :test 'equal)))
	  (dolist (name original)
		(puthash name t orig-set))
	  (dolist (name current)
		(puthash name t curr-set))

	  ;; Step 1: Find pure additions (in current, not in original)
	  (dolist (name current)
		(unless (gethash name orig-set)
		  (push name added)))

	  ;; Step 2: Find pure deletions (in original, not in current)
	  (dolist (name original)
		(unless (gethash name curr-set)
		  (push name deleted)))

	  ;; Step 3: Detect renames by position (only if counts match after add/del)
	  ;; If we have equal number of additions and deletions, try to match them as renames
	  (when (and (= (length added) (length deleted))
				 (> (length added) 0))
		;; Try to match by position in the difference
		(let ((orig-list original)
			  (curr-list current)
			  (temp-renames nil))
		  (while (and orig-list curr-list)
			(let ((orig-name (car orig-list))
				  (curr-name (car curr-list)))
			  (when (and (member orig-name deleted)
						 (member curr-name added)
						 (not (string= orig-name curr-name)))
				;; This looks like a rename
				(push (cons orig-name curr-name) temp-renames)
				(setq deleted (delete orig-name deleted))
				(setq added (delete curr-name added))))
			(setq orig-list (cdr orig-list)
				  curr-list (cdr curr-list)))
		  (setq renamed-pairs temp-renames))))

	(setq added (nreverse added)
		  deleted (nreverse deleted)
		  renamed-pairs (nreverse renamed-pairs))

	(when (or added deleted renamed-pairs)
	  (let ((msg (format "Changes:\n- Create: %d\n- Delete: %d\n- Rename: %d\n\nConfirm? "
						 (length added) (length deleted) (length renamed-pairs))))
		(when (yes-or-no-p msg)
		  ;; Execute renames first
		  (dolist (pair renamed-pairs)
			(let* ((old-name (car pair))
				   (new-name (cdr pair))
				   (old-is-dir (string-suffix-p "/" old-name))
				   (new-is-dir (string-suffix-p "/" new-name))
				   (old-clean (if old-is-dir (substring old-name 0 -1) old-name))
				   (new-clean (if new-is-dir (substring new-name 0 -1) new-name))
				   (old-path (expand-file-name old-clean dir))
				   (new-path (expand-file-name new-clean dir)))
			  (condition-case err
				  (progn
					(rename-file old-path new-path)
					(when (oe--is-marked-p old-path)
					  (let ((mark-entry (cl-find old-path oe--marks :key #'cdr :test #'string=)))
						(when mark-entry
						  (setcdr mark-entry new-path)
						  (oe--save-marks))))
					(message "Renamed: %s → %s" old-name new-name))
				(error (message "Error renaming %s: %s" old-name (error-message-string err))))))

		  ;; Then create new files
		  (dolist (f added)
			(let* ((is-dir (string-suffix-p "/" f))
				   (clean-name (if is-dir (substring f 0 -1) f))
				   (path (expand-file-name clean-name dir)))
			  (condition-case err
				  (if is-dir
					  (progn
						(make-directory path t)
						(message "Created directory: %s" path))
					(make-empty-file path)
					(message "Created file: %s" path))
				(error (message "Error creating %s: %s" f (error-message-string err))))))

		  ;; Finally delete files
		  (dolist (f deleted)
			(let* ((is-dir (string-suffix-p "/" f))
				   (clean-name (if is-dir (substring f 0 -1) f))
				   (path (expand-file-name clean-name dir)))
			  (condition-case err
				  (progn
					(if is-dir
						(progn
						  (delete-directory path t)
						  (message "Deleted directory: %s" path))
					  (delete-file path)
					  (message "Deleted file: %s" path))
					(when (oe--is-marked-p path)
					  (setq oe--marks (cl-remove path oe--marks :key #'cdr :test #'string=))
					  (oe--save-marks)))
				(error (message "Error deleting %s: %s" f (error-message-string err))))))

		  (oe--insert-dir dir)
		  (message "Oe: %d created, %d deleted, %d renamed"
				   (length added) (length deleted) (length renamed-pairs)))))
	(unless (or added deleted renamed-pairs)
	  (message "No changes detected"))))

(defun oe-cancel ()
  "Abort and kill Oe buffer."
  (interactive)
  (when (yes-or-no-p "Discard all changes? ")
	(kill-buffer (current-buffer))
	(when (buffer-live-p oe--orig-buf)
	  (switch-to-buffer oe--orig-buf))))

;;; ============================================================================
;;; preview
;;; ============================================================================

(defun oe-preview ()
  "Show preview of file at point in a bottom window and switch to it."
  (interactive)
  (let ((path (oe--get-path-at-point)))
	(when path
	  (if (file-directory-p path)
		  (message "Directory: %d items"
				   (length (directory-files path nil nil t))) ; t to skip . and ..
		(let ((preview-buffer (get-buffer-create "*Oe Preview*")))
		  (with-current-buffer preview-buffer
			(let ((inhibit-read-only t))
			  (erase-buffer)
			  (condition-case err
				  (insert-file-contents path nil 0 10000)
				(file-too-short
				 (insert-file-contents path nil))
				(file-error
				 (insert (format "Error reading file: %s" (error-message-string err))))))
			(goto-char (point-min))
			(special-mode))
		  ;; Display in bottom window and switch to it
		  (let ((preview-window (display-buffer preview-buffer
												'((display-buffer-at-bottom)
												  (window-height . 10)))))
			(select-window preview-window)))))))

;;; ============================================================================
;;; Mode Definition
;;; ============================================================================

(defun oe--update-keymap ()
  "Update active keymap based on current mode."
  (if oe--tree-mode
	  (progn
		(use-local-map (make-composed-keymap
						(list oe-tree-mode-map oe-mode-map)))
		(setq buffer-read-only t)
		(setq-local header-line-format
					"Tree: RET=open ^=up SPC=select C=copy X=cut P=paste q=quit"))
	(use-local-map (make-composed-keymap
					(list oe-edit-mode-map oe-mode-map)))
	(setq buffer-read-only nil)
	(setq-local header-line-format
				"Edit: C-c C-c=save C-c C-t=tree q=quit")))

(define-minor-mode oe-mode
  "Minimal editable directory buffer, inspired by oe.nvim."
  :lighter " Oe"
  :keymap oe-mode-map
  (oe--update-keymap))

;;; ============================================================================
;;; Integration
;;; ============================================================================

;;;###autoload
(defun oe-dired-here ()
  "Open current dired directory in Oe."
  (interactive)
  (if (eq major-mode 'dired-mode)
	  (oe-open default-directory)
	(user-error "Not in dired buffer")))

;;;###autoload
(defun oe-to-dired ()
  "Open current Oe directory in dired."
  (interactive)
  (if (and (boundp 'oe--dir) oe--dir)
	  (dired oe--dir)
	(user-error "Not in Oe buffer")))

;;;###autoload
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c o") #'oe-dired-here))

;; Add dired shortcut in oe-mode
(define-key oe-mode-map (kbd "C-c o") #'oe-to-dired)

(provide 'oe)
;;; oe.el ends here
