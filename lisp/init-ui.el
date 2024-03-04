;;; init-ui.el --- appearance.
;;; Commentary:
;;; Code:

(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(use-package nordic-vein-theme
  :straight
  (:type git :repo "https://github.com/buttertone/nordic-vein")
  :init
  (add-hook 'emacs-startup-hook (load-theme 'nordic-vein t)))

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
