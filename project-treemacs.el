;; project-treemacs.el --- project.el for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Christopher McCloud

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Simple treemacs backend for project.el:
;; Treemacs Project -> project.el Project
;; Treemacs Workspace -> project.el External Roots
;; 
;; `project-find-file', `project-switch-to-buffer', `project-find-regexp' et al
;; operate on the current treemacs project.
;; 
;; `project-or-external-find-file', and `project-or-external-find-regexp'
;; operate on the current treemacs workspace.
;; 

;;; Code:
(require 'seq)
(require 'treemacs)
(require 'project)

(defgroup project-treemacs nil
  "Project implementation based on the Treemacs package."
  :group 'project)

(defcustom project-treemacs-ignores '("^\\.#" "^flycheck_" "~$" "\\.git/")
  "List of patterns to add to `project-ignores'.
Default value emulates `treemacs--std-ignore-file-predicate'."
  :type '(repeat string)
  :safe #'listp)

(defcustom project-treemacs-prefer-backend t
  "Whether or not to prefer the project-treemacs backend over others.

When `t' `project-treemacs-try' is placed at the front of
 `project-find-functions'.

When `nil', `project-treemacs-try' is instead appended to the end
of `project-find-functions'.")

(defvar project-treemacs--files-cache (make-hash-table)
  "Stores project-treemacs `project-files'.
Only used when `treemacs-filewatch-mode' is enabled.")

(defvar project-treemacs--idle-timer nil)

(defun project-treemacs--clear-cache ()
  "Clears `project-treemacs--files-cache' and updates when idle."
  (when treemacs-filewatch-mode
    (let (keys)
      (maphash (lambda (k _v) (push k keys)) project-treemacs--files-cache)
      (clrhash project-treemacs--files-cache)
      (unless project-treemacs--idle-timer
	;; Check to verify that we don't already have an idle-timer
	;; waiting up update the cache.
	(setq project-treemacs--idle-timer
	      (run-with-idle-timer 1 nil #'project-treemacs--update-cache keys))))))

(defun project-treemacs--update-cache (directories)
  (seq-each (lambda (d) (project-treemacs--get-files-for-dir d)) directories)
  (cancel-timer project-treemacs--idle-timer)
  (setq project-treemacs--idle-timer nil))

(defun project-treemacs--explore-dir-p (dir-name)
  (not (seq-some (lambda (glob) (string-match-p glob dir-name))
                 (project-ignores (project-current) ""))))

(defun project-treemacs--ignore-file-p (project path)
  (seq-contains-p (project-ignores project nil) path #'string-match-p))

(defun project-treemacs--get-files-for-dir (dir)
  (or (and treemacs-filewatch-mode (gethash dir project-treemacs--files-cache))
      (puthash dir (directory-files-recursively dir ".*" nil #'project-treemacs--explore-dir-p nil)
	       project-treemacs--files-cache)))

;; Project.el API
;;;###autoload
(defun project-treemacs-try (dir)
  (when (and (fboundp 'treemacs-current-visibility)
	     (eq (treemacs-current-visibility) 'visible))
    (treemacs--find-project-for-path dir)))

(cl-defmethod project-root ((project treemacs-project))
  (treemacs-project->path project))

(cl-defmethod project-files ((project treemacs-project) &optional dirs)
  (seq-remove (lambda (p) (project-treemacs--ignore-file-p project p))
              (seq-mapcat #'project-treemacs--get-files-for-dir
	                  (append (list (project-root project)) dirs))))

(cl-defmethod project-buffers ((project treemacs-project))
  (let ((root (expand-file-name (file-name-as-directory (project-root project)))))
    (seq-filter
     (lambda (buf)
       (string-prefix-p root (buffer-local-value 'default-directory buf)))
     (buffer-list))))

(cl-defmethod project-ignores ((project treemacs-project) dir)
  project-treemacs-ignores)

(cl-defmethod project-external-roots ((project treemacs-project))
  (remove (project-root project)
	  (mapcar #'treemacs-project->path
		  (treemacs-workspace->projects (treemacs-current-workspace)))))

;;;###autoload
(define-minor-mode project-treemacs-mode
  "Toggles treemacs backend for project.el.

When enabled, by default, treemacs backend is prefered over other backends, but
see `project-treemacs-prefer-backend' user option."
  :group 'project-treemacs
  :global t
  (if project-treemacs-mode
      (progn
        (define-advice treemacs--process-file-events
            (:after (&rest r) update-cache)
          (project-treemacs--clear-cache))
        (add-hook 'treemacs-switch-workspace-hook
                  #'project-treemacs--clear-cache)
        (add-to-list 'project-find-functions
                     #'project-treemacs-try
                     (unless project-treemacs-prefer-backend t)))
    (progn
      (advice-remove 'update-cache #'treemacs--process-file-events)
      (remove-hook 'treemacs-switch-workspace-hook
                   #'project-treemacs--clear-cache)
      (setq project-find-functions
            (remq #'project-treemacs-try
                  project-find-functions))
      (clrhash project-treemacs--files-cache)
      (when project-treemacs--idle-timer
        (cancel-timer project-treemacs--idle-timer)))))

(provide 'project-treemacs)

;; project-treemacs.el ends here
