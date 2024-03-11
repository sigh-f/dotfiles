;;; init-ui.el --- appearance.
;;; Commentary:
;;; Code:

(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(use-package nordic-vein-theme
  :straight
  (:type git :repo "https://github.com/buttertone/nordic-vein")
  :init
  (add-hook 'emacs-startup-hook (load-theme 'nordic-vein t)))

(use-package centaur-tabs
  :hook
  (emacs-startup . centaur-tabs-mode)
  (compilation-mode . centaur-tabs-local-mode)
  (dired-mode . centaur-tabs-local-mode)
  (fundamental-mode . centaur-tabs-local-mode)
  ;; (lisp-interaction-mode . centaur-tabs-local-mode)
  ;; (messages-buffer-mode . centaur-tabs-local-mode)
  (org-mode . centaur-tabs-local-mode)
  :config
  (defun centaur-tabs-buffer-groups-project ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode'
`emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((memq major-mode '(org-mode org-agenda-mode diary-mode))
       "OrgMode")
      ((memq major-mode '(messages-buffer-mode lisp-interaction-mode dired-mode fundamental-mode special-mode))
       "Emacs")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  (setq centaur-tabs-ace-jump-keys '(?a ?s ?d ?f ?j ?k ?l)
        centaur-tabs-buffer-groups-function 'centaur-tabs-buffer-groups-project
        centaur-tabs-gray-out-icons t
        centaur-tabs-icon-type 'nerd-icons
        centaur-tabs-plain-icons t
        centaur-tabs-set-close-button nil
        centaur-tabs-set-icons t
        centaur-tabs-show-new-tab-button nil)
  :bind
  (:map global-map
        ("C-c t t" . centaur-tabs-toggle-groups)
        ("C-c t o" . centaur-tabs-ace-jump)))

(use-package doom-modeline
  :hook
  (emacs-startup . doom-modeline-mode))

(use-package nerd-icons
  :after
  (doom-modeline))

(use-package nerd-icons-dired
  :after
  (nerd-icons)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'init-ui)
;;; init-ui.el ends here
