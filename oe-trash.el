;;; oe-trash.el --- Trash-cli integration for Oe -*- lexical-binding: t; -*-

(require 'oe)

;;; Customs

(defcustom oe-use-trash t
  "Non-nil means use trash-cli for deletions instead of permanent deletion.
Requires trash-put command to be available in PATH."
  :type 'boolean
  :group 'oe)

(defcustom oe-trash-command "trash-put"
  "Command to use for trashing files."
  :type 'string
  :group 'oe)

;;; Core Functions

(defun oe-trash-available-p ()
  "Check if trash-put command is available."
  (and oe-use-trash
	   (executable-find oe-trash-command)))

(defun oe-trash--delete-file-or-directory (path)
  "Delete file or directory at PATH.
Use trash-cli if available and enabled, otherwise permanent deletion."
  (if (oe-trash-available-p)
	  (let ((exit-code (call-process oe-trash-command nil nil nil path)))
		(if (zerop exit-code)
			(message "Moved to trash: %s" path)
		  (error "Failed to trash %s (exit code %d)" path exit-code)))
	;; Fallback to permanent deletion
	(if (file-directory-p path)
		(progn
		  (delete-directory path t)
		  (message "Permanently deleted directory: %s" path))
	  (delete-file path)
	  (message "Permanently deleted file: %s" path))))

(defun oe-trash--parse-list ()
  "Parse trash-list output and return list of items.
Each item is a plist with :date :time :path :name."
  (when (executable-find "trash-list")
	(let ((output (shell-command-to-string "trash-list"))
		  (items nil))
	  (dolist (line (split-string output "\n" t))
		(when (string-match "^\\([0-9-]+\\) \\([0-9:]+\\) \\(.+\\)$" line)
		  (let* ((date (match-string 1 line))
				 (time (match-string 2 line))
				 (path (match-string 3 line))
				 (name (file-name-nondirectory path)))
			(push (list :date date
						:time time
						:path path
						:name name
						:display (format "%-12s %-10s %s" date time name))
				  items))))
	  (nreverse items))))

(defun oe-trash--restore-file (path)
  "Restore file from trash by PATH using trash-restore automation."
  (let* ((name (file-name-nondirectory path))
		 ;; Get list of trashed files matching this name
		 (items (oe-trash--parse-list))
		 (matches (cl-remove-if-not
				   (lambda (item) (string= (plist-get item :path) path))
				   items)))
	(if (not matches)
		(progn
		  (message "File not found in trash: %s" name)
		  nil)
	  ;; Find the index of this item in trash-restore's list
	  (let* ((all-names (mapcar (lambda (item) (plist-get item :name)) items))
			 (target-idx (cl-position name all-names :test #'string=)))
		(if (not target-idx)
			(progn
			  (message "Could not determine restore index")
			  nil)
		  ;; Restore using index
		  (let ((cmd (format "echo '%d' | trash-restore 2>&1" target-idx)))
			(let ((result (shell-command-to-string cmd)))
			  (if (string-match-p "restored" result)
				  (progn
					(message "Restored: %s" name)
					t)
				(progn
				  (message "Restore may have succeeded, check: %s" name)
				  t)))))))))

;;; Commands

(defun oe-delete-selected-with-trash ()
  "Delete selected files using trash-cli if available."
  (interactive)
  (let ((files (or oe--selected-files
				   (let ((p (oe--get-path-at-point)))
					 (when p (list p)))))
		(inhibit-read-only t)
		(current-line (line-number-at-pos))
		(use-trash (oe-trash-available-p)))
	(unless files
	  (user-error "No files selected"))
	(let ((prompt (if use-trash
					  (format "Move %d item(s) to trash? " (length files))
					(format "Permanently delete %d item(s)? " (length files)))))
	  (when (yes-or-no-p prompt)
		(let ((count 0)
			  (errors nil))
		  (dolist (path files)
			(condition-case err
				(progn
				  (oe-trash--delete-file-or-directory path)
				  (setq count (1+ count)))
			  (error
			   (push (format "%s: %s" path (error-message-string err)) errors))))
		  (setq oe--selected-files nil)
		  (oe--insert-dir oe--dir)
		  (goto-char (point-min))
		  (forward-line (1- current-line))
		  (recenter)
		  (if errors
			  (message "%s %d items (%d errors)"
					   (if use-trash "Trashed" "Deleted")
					   count (length errors))
			(message "%s %d items"
					 (if use-trash "Trashed" "Deleted")
					 count)))))))

;;;###autoload
(defun oe-trash-restore ()
  "Restore a file from trash with completion selection."
  (interactive)
  (if (not (executable-find "trash-restore"))
	  (message "trash-restore command not found")
	(let ((items (oe-trash--parse-list)))
	  (if (not items)
		  (message "Trash is empty")
		(let* ((choices (mapcar (lambda (item)
								  (cons (plist-get item :display) item))
								items))
			   (selected (completing-read "Restore file: "
										  (mapcar #'car choices)
										  nil t)))
		  (when selected
			(let* ((item (cdr (assoc selected choices)))
				   (path (plist-get item :path)))
			  (when (oe-trash--restore-file path)
				;; Refresh current Oe buffer if in one
				(when (and (boundp 'oe--dir) oe--dir)
				  (let ((inhibit-read-only t))
					(oe--insert-dir oe--dir)))))))))))

;;;###autoload
(defun oe-trash-list ()
  "List files in trash with interactive selection.
In the trash list buffer:
  RET - Restore file at point
  r   - Restore file at point
  d   - Delete file permanently (not implemented for safety)
  e   - Empty entire trash
  g   - Refresh list"
  (interactive)
  (if (not (executable-find "trash-list"))
	  (message "trash-list command not found")
	(let ((items (oe-trash--parse-list)))
	  (if (not items)
		  (message "Trash is empty")
		(with-current-buffer (get-buffer-create "*Trash List*")
		  (let ((inhibit-read-only t))
			(erase-buffer)
			(insert (propertize "Trash Contents\n" 'face 'bold))
			(insert (propertize "==============\n\n" 'face 'bold))
			(insert (propertize "Press RET or 'r' to restore, 'e' to empty trash, 'g' to refresh\n\n"
								'face 'font-lock-comment-face))
			;; Header
			(insert (propertize (format "%-12s %-10s %s\n" "Date" "Time" "File")
								'face 'header-line))
			(insert (propertize (make-string 80 ?-) 'face 'shadow))
			(insert "\n")
			;; Items
			(dolist (item items)
			  (let ((start (point)))
				(insert (format "%-12s %-10s %s\n"
								(plist-get item :date)
								(plist-get item :time)
								(plist-get item :path)))
				;; Store item data as text property
				(put-text-property start (point) 'trash-item item)))
			(insert "\n")
			(insert (propertize (format "Total: %d item(s)\n" (length items))
								'face 'shadow))
			(goto-char (point-min))
			(forward-line 4) ; Skip header lines
			(special-mode)
			;; Add keybindings
			(local-set-key (kbd "RET") #'oe-trash-list-restore-at-point)
			(local-set-key (kbd "r") #'oe-trash-list-restore-at-point)
			(local-set-key (kbd "e") #'oe-trash-empty)
			(local-set-key (kbd "g") #'oe-trash-list))
		  (display-buffer (current-buffer)))))))

(defun oe-trash-list-restore-at-point ()
  "Restore the trash item at point."
  (interactive)
  (let ((item (get-text-property (point) 'trash-item)))
	(if (not item)
		(message "No trash item at point")
	  (let ((path (plist-get item :path))
			(name (plist-get item :name)))
		(when (yes-or-no-p (format "Restore '%s'? " name))
		  (if (oe-trash--restore-file path)
			  (progn
				(message "Restored: %s" name)
				;; Refresh trash list
				(oe-trash-list))
			(message "Failed to restore: %s" name)))))))

;;;###autoload
(defun oe-trash-empty ()
  "Empty the trash."
  (interactive)
  (if (not (executable-find "trash-empty"))
	  (message "trash-empty command not found")
	(let ((items (oe-trash--parse-list)))
	  (when (yes-or-no-p (format "Empty trash permanently (%d items)? This cannot be undone! "
								 (length items)))
		(if (zerop (call-process "trash-empty" nil nil nil))
			(progn
			  (message "Trash emptied")
			  ;; Refresh trash list buffer if it exists
			  (when (get-buffer "*Trash List*")
				(kill-buffer "*Trash List*")
				(message "Trash is now empty")))
		  (message "Failed to empty trash"))))))

;;; Integration

;;;###autoload
(defun oe-trash-init ()
  "Initialize oe-trash integration."
  (interactive)
  (with-eval-after-load 'oe
	;; Override the delete command
	(define-key oe-tree-mode-map (kbd "D") #'oe-delete-selected-with-trash)
	;; Add trash-specific commands
	(define-key oe-mode-map (kbd "C-c t l") #'oe-trash-list)
	(define-key oe-mode-map (kbd "C-c t r") #'oe-trash-restore)
	(define-key oe-mode-map (kbd "C-c t e") #'oe-trash-empty))
  (when (not (oe-trash-available-p))
	(message "Warning: trash-put not found. Install trash-cli for trash support.")))

;;;###autoload
(oe-trash-init)

(provide 'oe-trash)
;;; oe-trash.el ends here
