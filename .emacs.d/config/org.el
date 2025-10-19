;;;; ==========================================================================
;;;; General Org Mode Configuration
;;;; ==========================================================================
(use-package org
  :ensure t
  :bind ("C-c c" . org-capture)
  :config
  ;; Set the main directory for all our org files
  (setq org-directory "~/org/")

  ;; Set custom TODO keywords for your GTD workflow
  (setq org-todo-keywords
        '((sequence "TODO(t)" "INPROGRESS(p)" "|" "DONE(d)")
          (sequence "PENDING(n)" "|" "CANCELLED(c)")
          (sequence "SOMEDAY(s)")))

  ;; Set files to be included in the agenda
  (setq org-agenda-files (list (concat org-directory "tasks.org")
                               (concat org-directory "projects.org")
                               (concat org-directory "inbox.org")
                               (concat org-directory "journal/")))) ; Scan the whole journal dir

  ;; Configure refiling targets
  ;; This is crucial for your inbox review workflow
  (setq org-refile-targets
        '((org-agenda-files :maxlevel . 3)
          ("~/org/links.org" :maxlevel . 1)
          ("~/org/contacts.org" :maxlevel . 2)
          ("~/org/people.org" :maxlevel . 2)))

  ;; Use full outline path for refiling, which is more descriptive
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-separators "::") ; e.g., "Project A::Subtask B"

  ;; Log when a task is marked as DONE
  (setq org-log-done 'time)

;;;; ==========================================================================
;;;; Org Capture Templates (The heart of your workflow)
;;;; ==========================================================================
(setq org-capture-templates
      `(("i" "Inbox Note" entry (file "~/org/inbox.org")
         "* %?\n%i\n%a" :prepend t)

        ("t" "Task" entry (file "~/org/tasks.org")
         "* TODO %?\n  SCHEDULED: %(org-insert-time-stamp (org-read-date-presets t))")

        ("p" "Project" entry (file "~/org/projects.org")
         "* TODO %? \n:PROPERTIES:\n:CREATED: %U\n:END:\n%i" :empty-lines 1)

	("l" "Link/Bookmark" entry (file "~/org/links.org")
         "* [[%{URL|%c}][%:description]]\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n" :prepend t)

        ("c" "Contact" entry (file "~/org/contacts.org")
         "* %? \n:PROPERTIES:\n:EMAIL:\n:PHONE:\n:END:\n")

        ("P" "Person" entry (file "~/org/people.org")
         "* %? \n:PROPERTIES:\n:BIRTH:\n:DEATH:\n:END:\n")

        ;; Journaling Sub-menu
        ("j" "Journal")
        ("jd" "Daily Journal" entry
         (file+datetree (lambda () (format-time-string "~/org/journal/%Y/%m/%d.org")))
         "* %<%H:%M> %?\n%i" :clock-in t :clock-resume t)

        ("jw" "Weekly Journal" entry
         (file (lambda () (format-time-string "~/org/journal/%Y/week_%V.org")))
         "* %<%Y-%m-%d %a> Weekly Reflection\n\n%?" :empty-lines-before 1)

        ("jm" "Monthly Journal" entry
         (file (lambda () (format-time-string "~/org/journal/%Y/%B.org")))
         "* Monthly Reflection: %^{Month Name|%(format-time-string \"%B\")} %^{Year|%(format-time-string \"%Y\")}\n\n%?" :empty-lines-before 1)

        ("jy" "Yearly Journal" entry
         (file (lambda () (format-time-string "~/org/journal/%Y/summary.org")))
         "* Yearly Reflection: %^{Year|%(format-time-string \"%Y\")}\n\n%?" :empty-lines-before 1)
        ))

;;;; ==========================================================================
;;;; Evergreen Notes / Zettelkasten (using org-roam)
;;;; ==========================================================================
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t) ; Acknowledge V2 is being used
  :custom
  (org-roam-directory (file-truename "~/org/notes/"))
  (org-roam-completion-everywhere t)
  ;; This is the correct way to define org-roam templates
  (org-roam-capture-templates
   `(("n" "New Evergreen Note" plain "%?"
      :target
      (file+head "%(format-time-string \"%Y%m%d%H%M%S\")-${slug}.org"
                 "#+title: ${title}\n#+filetags: \n\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ;; New, robust keybinding for creating notes
         ("C-c n n" . org-roam-capture))
  :config
  ;; Enable org-roam in all org buffers
  (org-roam-db-autosync-mode))
