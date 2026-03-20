;; Lightweight project management on top of project.el
;; - Remembers known projects
;; - Saves/restores open files per project

(require 'project)

(defvar my-projects-dir (expand-file-name "my-projects" user-emacs-directory))
(defvar my-projects-sessions-dir (expand-file-name "sessions" my-projects-dir))
(defvar my-projects-known-file (expand-file-name "known-projects.el" my-projects-dir))
(defvar my-projects--current nil "Root of the currently active project.")

;; Ensure storage dirs exist
(make-directory my-projects-sessions-dir t)

;;; Known projects

(defun my-projects--load-known ()
  "Load the known projects list from disk."
  (when (file-exists-p my-projects-known-file)
    (with-temp-buffer
      (insert-file-contents my-projects-known-file)
      (read (current-buffer)))))

(defun my-projects--save-known (projects)
  "Save PROJECTS list to disk."
  (with-temp-file my-projects-known-file
    (prin1 projects (current-buffer))))

(defun my-projects--add-known (root)
  "Add ROOT to the known projects list if not already present."
  (let ((known (my-projects--load-known)))
    (unless (member root known)
      (my-projects--save-known (cons root known)))))

;;; Per-project sessions

(defun my-projects--session-file (root)
  "Return the session file path for project ROOT."
  (expand-file-name (md5 root) my-projects-sessions-dir))

(defun my-projects--project-buffers (root)
  "Return file paths of all buffers visiting files under ROOT."
  (let ((root (file-truename root)))
    (cl-loop for buf in (buffer-list)
             for file = (buffer-file-name buf)
             when (and file (string-prefix-p root (file-truename file)))
             collect file)))

(defun my-projects--save-session (&optional root)
  "Save open file list for project ROOT (defaults to current)."
  (let ((root (or root my-projects--current)))
    (when root
      (let ((files (my-projects--project-buffers root)))
        (with-temp-file (my-projects--session-file root)
          (prin1 files (current-buffer)))))))

(defun my-projects--restore-session (root)
  "Restore saved files for project ROOT."
  (let ((session-file (my-projects--session-file root)))
    (when (file-exists-p session-file)
      (let ((files (with-temp-buffer
                     (insert-file-contents session-file)
                     (read (current-buffer)))))
        (dolist (f files)
          (when (file-exists-p f)
            (find-file-noselect f)))
        ;; Switch to the first restored file or dired on the project root
        (if-let ((first-file (cl-find-if #'file-exists-p files)))
            (find-file first-file)
          (dired root))))))

;;; Switching projects

(defun my-projects-switch ()
  "Switch to a known project, saving current and restoring target."
  (interactive)
  (let* ((known (my-projects--load-known))
         (root (completing-read "Switch to project: " known nil t)))
    (my-projects--save-session)
    (setq my-projects--current root)
    (my-projects--restore-session root)))

;;; Adding projects manually

(defun my-projects-add-dir (dir)
  "Register DIR as a known project."
  (interactive "DAdd project directory: ")
  (let ((root (file-truename (directory-file-name dir))))
    (my-projects--add-known root)
    (message "Added project: %s" root)))

;;; Auto-track current project

(defun my-projects--track-current ()
  "Track the current project based on `project.el` detection."
  (when-let ((proj (project-current nil)))
    (let ((root (file-truename (directory-file-name (project-root proj)))))
      (unless (equal root my-projects--current)
        (my-projects--save-session)
        (setq my-projects--current root)
        (my-projects--add-known root)))))

;; Track project on file open and buffer switch
(add-hook 'find-file-hook #'my-projects--track-current)
(add-hook 'window-buffer-change-functions
          (lambda (_) (my-projects--track-current)))

;; Save session on quit
(add-hook 'kill-emacs-hook #'my-projects--save-session)

;;; Keybindings

(global-set-key (kbd "C-x p p") #'my-projects-switch)
(global-set-key (kbd "C-x p a") #'my-projects-add-dir)
