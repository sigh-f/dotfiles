;;; init-treemacs.el --- treemacs.
;;; Commentary:
;;; Code:

(use-package treemacs
  :config
  (treemacs-project-follow-mode)
  (setq treemacs-width 25
        treemacs--project-follow-delay 0.1)
  :bind
  (:map user-map
        ("C-j" . treemacs-select-window)))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(provide 'init-treemacs)
;;; init-treemacs.el ends here
