;;; oe-session.el --- Session management for oe.el -*- lexical-binding: t; -*-

(require 'cl-lib)

;;; ============================================================================
;;; Variables
;;; ============================================================================

(defvar oe-session-file (expand-file-name "oe-sessions.el" user-emacs-directory)
  "File to persist Oe sessions.")

(defvar oe-session-recent-limit 50
  "Maximum number of recent directories to keep.")

(defvar oe--sessions nil
  "Alist of (SESSION-NAME . SESSION-DATA).
SESSION-DATA is a plist with keys:
  :current-dir - current directory path
  :history - navigation history
  :directory-states - hash table of per-directory states
  :timestamp - creation time")

(defvar oe--recent-dirs nil
  "List of recently visited directories in Oe.")

;;; ============================================================================
;;; Persistence
;;; ============================================================================

(defun oe-session--load ()
  "Load sessions from persistent storage."
  (when (file-exists-p oe-session-file)
	(condition-case err
		(with-temp-buffer
		  (insert-file-contents oe-session-file)
		  (let ((data (read (buffer-string))))
			(setq oe--sessions (plist-get data :sessions))
			(setq oe--recent-dirs (plist-get data :recent-dirs))))
	  (error
	   (message "Error loading Oe sessions: %s" (error-message-string err))
	   (setq oe--sessions nil
			 oe--recent-dirs nil)))))

(defun oe-session--save ()
  "Save sessions to persistent storage."
  (condition-case err
	  (with-temp-file oe-session-file
		(let ((data (list :sessions oe--sessions
						  :recent-dirs oe--recent-dirs)))
		  (prin1 data (current-buffer))))
	(error
	 (message "Error saving Oe sessions: %s" (error-message-string err)))))

;;; ============================================================================
;;; Recent Directories
;;; ============================================================================

(defun oe-session-add-recent (dir)
  "Add DIR to recent directories list."
  (setq dir (expand-file-name dir))
  ;; Remove if already exists
  (setq oe--recent-dirs (delete dir oe--recent-dirs))
  ;; Add to front
  (push dir oe--recent-dirs)
  ;; Limit size
  (when (> (length oe--recent-dirs) oe-session-recent-limit)
	(setq oe--recent-dirs (seq-take oe--recent-dirs oe-session-recent-limit)))
  (oe-session--save))

(defun oe-session-clear-recent ()
  "Clear recent directories list."
  (interactive)
  (when (yes-or-no-p "Clear all recent directories? ")
	(setq oe--recent-dirs nil)
	(oe-session--save)
	(message "Recent directories cleared")))

;;;###autoload
(defun oe-session-recent ()
  "Open a recent directory in Oe."
  (interactive)
  (oe-session--load)
  (if (not oe--recent-dirs)
	  (message "No recent directories")
	(let ((dir (completing-read "Recent directory: " oe--recent-dirs nil t)))
	  (when dir
		(if (fboundp 'oe-open)
			(oe-open dir)
		  (dired dir))))))

;;; ============================================================================
;;; Sessions
;;; ============================================================================

;;;###autoload
(defun oe-session-save (name)
  "Save current Oe session as NAME with full state."
  (interactive "sSession name: ")
  (oe-session--load)
  (let ((buf (get-buffer "*oe*")))
	(if (not buf)
		(message "No Oe buffer to save")
	  (with-current-buffer buf
		(unless (and (boundp 'oe--dir) oe--dir)
		  (user-error "Not in an Oe buffer"))

		;; Save current state first
		(when (fboundp 'oe--save-current-state)
		  (oe--save-current-state))

		;; Convert directory states hash table to alist for serialization
		(let* ((states-alist nil))
		  (when (boundp 'oe--directory-states)
			(maphash (lambda (dir state)
					   (push (cons dir state) states-alist))
					 oe--directory-states))

		  (let ((session-data
				 (list :current-dir oe--dir
					   :history (when (boundp 'oe--history) oe--history)
					   :history-index (when (boundp 'oe--history-index)
										oe--history-index)
					   :directory-states states-alist
					   :timestamp (current-time))))

			;; Remove old session
			(setq oe--sessions (assoc-delete-all name oe--sessions))
			;; Add new session
			(push (cons name session-data) oe--sessions)
			(oe-session--save)

			(message "Session '%s' saved (%d directories with states)"
					 name (length states-alist))))))))

;;;###autoload
(defun oe-session-restore (name)
  "Restore Oe session NAME with full state."
  (interactive
   (list (completing-read "Restore session: "
						  (mapcar #'car oe--sessions)
						  nil t)))
  (oe-session--load)
  (let ((session (assoc name oe--sessions)))
	(if (not session)
		(message "Session '%s' not found" name)
	  (let* ((data (cdr session))
			 (current-dir (plist-get data :current-dir))
			 (history (plist-get data :history))
			 (history-index (plist-get data :history-index))
			 (states-alist (plist-get data :directory-states)))

		(if (not current-dir)
			(message "Session '%s' is empty" name)

		  ;; Restore directory states
		  (when (and (boundp 'oe--directory-states) states-alist)
			(clrhash oe--directory-states)
			(dolist (pair states-alist)
			  (puthash (car pair) (cdr pair) oe--directory-states)))

		  ;; Open Oe buffer and restore state
		  (if (fboundp 'oe-open)
			  (progn
				(oe-open current-dir)
				(with-current-buffer "*oe*"
				  ;; Restore history
				  (when history
					(setq oe--history history
						  oe--history-index (or history-index -1)))
				  (message "Session '%s' restored: %d directories, history: %d"
						   name (length states-alist) (length history))))
			(dired current-dir)
			(message "Session restored (Oe not available, using dired)")))))))

;;;###autoload
(defun oe-session-delete (name)
  "Delete Oe session NAME."
  (interactive
   (list (completing-read "Delete session: "
						  (mapcar #'car oe--sessions)
						  nil t)))
  (oe-session--load)
  (when (yes-or-no-p (format "Delete session '%s'? " name))
	(setq oe--sessions (assoc-delete-all name oe--sessions))
	(oe-session--save)
	(message "Session '%s' deleted" name)))

;;;###autoload
(defun oe-session-list ()
  "Show all saved sessions."
  (interactive)
  (oe-session--load)
  (if (not oe--sessions)
	  (message "No saved sessions")
	(with-current-buffer (get-buffer-create "*Oe Sessions*")
	  (let ((inhibit-read-only t))
		(erase-buffer)
		(insert (propertize "Oe Sessions\n" 'face 'bold))
		(insert (propertize "============\n\n" 'face 'bold))
		(dolist (session oe--sessions)
		  (let* ((name (car session))
				 (data (cdr session))
				 (current-dir (plist-get data :current-dir))
				 (history (plist-get data :history))
				 (states (plist-get data :directory-states))
				 (time (plist-get data :timestamp))
				 (time-str (if time (format-time-string "%Y-%m-%d %H:%M" time) "unknown")))
			(insert (propertize (format "📌 %s\n" name) 'face 'font-lock-keyword-face))
			(insert (format "   Created: %s\n" time-str))
			(insert (format "   Current: %s\n" current-dir))
			(insert (format "   History: %d directories\n" (length history)))
			(insert (format "   Saved states: %d directories\n" (length states)))
			(when history
			  (insert "   Recent:\n")
			  (dolist (dir (seq-take (reverse history) 5))
				(insert (format "     • %s\n" dir))))
			(insert "\n")))
		(goto-char (point-min))
		(special-mode))
	  (display-buffer (current-buffer)))))

;;; ============================================================================
;;; Initialization
;;; ============================================================================

;; Auto-load sessions on startup
(oe-session--load)

;; Hook to track recent directories
(defun oe-session--track-directory ()
  "Track current directory when opening Oe buffer."
  (when (and (boundp 'oe--dir) oe--dir)
	(oe-session-add-recent oe--dir)))

;; If oe-mode exists, add hook
(when (boundp 'oe-mode-hook)
  (add-hook 'oe-mode-hook #'oe-session--track-directory))

(provide 'oe-session)
;;; oe-session.el ends here
