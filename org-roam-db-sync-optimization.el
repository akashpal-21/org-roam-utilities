;;; org-roam-db-sync-optimization.el

;;; Commentary:
;;
;; This Emacs Lisp file optimizes the Org-roam database synchronization process by introducing
;; a mechanism to skip unnecessary synchronization when the database file has not been modified
;; since the last update. The timestamp of the last Org-roam database update is stored in an
;; external file.

;;; Code:
(require 'org-roam-db)

(defcustom org-roam-db-last-update-file "~/.emacs.d/org-roam-db-last-update-time"
  "File to store the timestamp of the last Org-roam database update."
  :type 'file
  :group 'org-roam)

(defvar org-roam-db-last-update-time nil
  "Timestamp of the last Org-roam database update.")

(defun org-roam-db-load-last-update-time ()
  "Load the timestamp of the last Org-roam database update from file.
If the file is not readable or does not exist, the timestamp remains nil."
  (when (file-readable-p org-roam-db-last-update-file)
    (setq org-roam-db-last-update-time
          (with-temp-buffer
            (insert-file-contents org-roam-db-last-update-file)
            (read (current-buffer))))))

(defun org-roam-db-save-last-update-time ()
  "Save the timestamp of the last Org-roam database update to file."
  (with-temp-buffer
    (prin1 org-roam-db-last-update-time (current-buffer))
    (write-region (point-min) (point-max) org-roam-db-last-update-file)))

(defun org-roam-db-update-time ()
  "Update the timestamp of the last Org-roam database update.
This function sets the timestamp to the current time and saves it to the external file."
  (setq org-roam-db-last-update-time (current-time))
  (org-roam-db-save-last-update-time))

(defun org-roam-db-sync-advice (orig-fun &rest args)
  "Advice function for org-roam-db-sync to check if syncing is necessary.
This advice checks whether the Org-roam database file has been modified since the last update.
If the file has been modified or the last update time is nil, it calls the original function (`org-roam-db-sync`),
updates the timestamp, and saves it to the external file."
  (let ((db-file-modified-time (nth 5 (file-attributes org-roam-db-location))))
    (when (or (null org-roam-db-last-update-time)
              (time-less-p org-roam-db-last-update-time db-file-modified-time))
      ;; Call the original function to perform synchronization
      (apply orig-fun args)
      ;; Update and save the timestamp
      (org-roam-db-update-time))))

(defun custom/org-roam-db-sync (&optional force)
  "Temporarily remove the sync advice and do org-roam-db-sync.
If FORCE is non-nil, force a rebuild of the cache from scratch."
  (interactive "P")
  (advice-remove 'org-roam-db-sync #'org-roam-db-sync-advice)
  (org-roam-db-sync force)
  (org-roam-db-update-time)
  (advice-add 'org-roam-db-sync :around #'org-roam-db-sync-advice))

;;; Initialization:

;; Load the last update time when Emacs starts
(org-roam-db-load-last-update-time)

;; Advising org-roam-db-sync
(advice-add 'org-roam-db-sync :around #'org-roam-db-sync-advice)

;; Save the last update time when Emacs is about to exit
(add-hook 'kill-emacs-hook 'org-roam-db-save-last-update-time)

 ; Improvements to org-roam-db-sync helper functions

(defun org-roam-db--file-hash (file-path)
  "Compute the hash of FILE-PATH concatenated with its modification time."
  (secure-hash 'sha1 (format "%s %s" file-path (nth 5 (file-attributes file-path)))))

(setq org-roam-list-files-commands '(rg))

(defun org-roam--list-files-search-globs (exts)
  "Given EXTS, return a list of search globs.
E.g. (\".org\") => (\"*.org\" \"*.org.gpg\")"
  (cl-loop for e in exts
           append (list (format "\"*.%s\"" e)
                        (format "\"*.%s.gpg\"" e)
                        (format "\"*.%s.age\"" e)
			(format "\"!*/resources/*\"")
			(format "\"!*/SPS/*\""))))

(defun org-roam--list-files (dir)
  "Return all Org-roam files located recursively within DIR.
Use external shell commands if defined in `org-roam-list-files-commands'."
  (let (path exe)
    (cl-dolist (cmd org-roam-list-files-commands)
      (pcase cmd
        (`(,e . ,path)
         (setq path (executable-find path)
               exe  (symbol-name e)))
        ((pred symbolp)
         (setq path (executable-find (symbol-name cmd))
               exe (symbol-name cmd)))
        (wrong-type
         (signal 'wrong-type-argument
                 `((consp symbolp)
                   ,wrong-type))))
      (when path (cl-return)))
    (if-let* ((files (when path
                       (let ((fn (intern (concat "org-roam--list-files-" exe))))
                         (unless (fboundp fn) (user-error "%s is not an implemented search method" fn))
                         (funcall fn path (format "\"%s\"" dir)))))
              (files (mapcar #'expand-file-name files))) ; canonicalize names
        files
      (org-roam--list-files-elisp dir))))

(provide 'org-roam-db-sync-optimization)

;;; org-roam-db-sync-optimization.el ends here
