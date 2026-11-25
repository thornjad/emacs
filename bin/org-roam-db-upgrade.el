;;; org-roam-db-upgrade.el --- rebuild org-roam.db when its schema is stale  -*- lexical-binding: t; -*-

;; Run via `make org-roam-db-upgrade' with `emacs --batch -Q'; never loads config.el. org-roam's own
;; in-place upgrade runs `org-roam-db-sync' inside a transaction, which closes the connection
;; underneath it and fails on COMMIT. The db is a pure cache, so when its schema version is stale we
;; delete it and let a sync rebuild it from scratch instead.

;;; Code:

;; a batch run only needs the db, never compiled code, so keep native compilation from firing
(setq native-comp-jit-compilation nil
      native-comp-async-report-warnings-errors 'silent)

(let ((root (expand-file-name ".." (file-name-directory load-file-name))))
  (setq user-emacs-directory (file-name-as-directory root)
        borg-drones-directory (expand-file-name "lib/drones" root))
  (add-to-list 'load-path (expand-file-name "lib/borg" root))
  (require 'borg)
  (borg-initialize))

(require 'org-roam)

(setq org-roam-directory (expand-file-name "~/Documents/thornlog/roam")
      org-roam-database-connector 'sqlite-builtin
      org-roam-db-location (expand-file-name "org-roam.db" user-emacs-directory)
      org-id-locations-file (expand-file-name "etc/cache/.org-id-locations" user-emacs-directory))

(defun aero/org-roam-db-stored-version (file)
  "Return the schema version stored in db FILE, or nil if it has none."
  (when (file-exists-p file)
    (let ((db (sqlite-open file)))
      (unwind-protect (caar (sqlite-select db "PRAGMA user_version"))
        (sqlite-close db)))))

(let ((version (aero/org-roam-db-stored-version org-roam-db-location)))
  (if (equal version org-roam-db-version)
      (message "org-roam db already at version %s" version)
    (message "org-roam db version %s -> %s: rebuilding" version org-roam-db-version)
    (dolist (suffix '("" "-wal" "-shm"))
      (let ((f (concat org-roam-db-location suffix)))
        (when (file-exists-p f) (delete-file f))))
    (org-roam-db-sync t)
    (org-roam-db--close-all)))

;;; org-roam-db-upgrade.el ends here
