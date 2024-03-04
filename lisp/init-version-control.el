;;; init-version-control.el --- version control.
;;; Commentary:
;;; Code:

(use-package diff-hl
  :hook
  (prog-mode . diff-hl-mode)
  (prog-mode . diff-hl-margin-mode)
  :config
  (setq diff-hl-show-staged-changes nil)
  :bind
  (:map user-map/vc
        ("a" . diff-hl-stage-current-hunk)
        ("k" . diff-hl-revert-hunk)
        ("p" . diff-hl-show-hunk-previous)
        ("n" . diff-hl-show-hunk-next)))

(use-package git-timemachine
  :config
  (add-hook 'git-timemachine-mode-hook 'display-line-numbers-mode)
  :bind
  (:map user-map/vc
        ("t" . git-timemachine)))

(use-package magit
  :defer t)

(use-package magit-todos
  :after
  (magit)
  :hook
  (magit-mode . magit-todos-mode))

(provide 'init-version-control)
;;; init-version-control.el ends here
