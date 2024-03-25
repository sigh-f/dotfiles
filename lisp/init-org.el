;;; init-org.el --- org.
;;; Commentary:
;;; Code:


(use-package org
  :straight (:type built-in)
  :demand t
  :init
  (defun user/find-current-notes-other-window ()
    (interactive)
    (when (project-current)
      (find-file-other-window (concat "~/org"
                                      "/project-notes/"
                                      (car (last (butlast (split-string (project-root (project-current)) "/"))))
                                      ".org"))))
  :config
  (setq org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t
        org-hide-emphasis-markers t
        org-log-done 'time
        org-log-into-drawer "LOGBOOK"
        org-startup-indented t
        org-startup-folded t
        org-todo-keywords '((sequence "TODO(t)" "REVIEW(v@)" "RELEASE(r@)" "|" "DONE(d@)" "BLOCKED(b@)")
                            (sequence "BLOCKED(b@)" "|" "DEAD(k@)")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (emacs-lisp . t)
     (makefile . t)
     (perl . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (shell . t)))
  :bind
  (:map global-map
        ("C-x p o" . 'user/find-current-notes-other-window))
  (:map org-mode-map
        ("C-j" . nil)
        ("C-c C-j" . org-todo))
  (:map user-map/org
        ("t" . org-todo-list)))

(use-package org-agenda
  :straight
  (:type built-in)
  ;; :defer t
  :bind
  (:map user-map/org
        ("a" . org-agenda)))

(use-package org-capture
  :straight
  (:type built-in)
  ;; :defer t
  :config
  (setq org-default-notes-file (concat org-directory "/mailbox.org"))
  (setq org-capture-templates
        '(("i" "Idea" entry (file+headline org-default-notes-file "Ideas")
           "** %? :idea:\nCreated: %T\n%i\n")
          ("t" "Todo" entry (file+headline org-default-notes-file "Triage later")
           "** TODO %? \nCreated: %T\n%i\n")))
  :bind
  (:map user-map/org
        ("c" . org-capture)))

(use-package org-modern
  :hook
  ((org-mode . org-modern-mode)
   (org-agenda-finalize . org-modern-agenda)))

(provide 'init-org)
;;; init-org.el ends here
