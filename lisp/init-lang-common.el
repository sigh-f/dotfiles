;;; init-lang-common.el --- common programming.
;;; Commentary:
;;; Code:

(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)

(use-package lsp-mode
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-modeline-code-actions-mode nil)
  (setq read-process-output-max (* 1024 3072)) ;; 1mb
  (setq lsp-modeline-code-action-fallback-icon ""))

(use-package lsp-ui
  :after
  (lsp-mode)
  :config
  (setq lsp-ui-sideline-delay 1
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-update-mode "line"))

(use-package tree-sitter
  :hook
  (emacs-startup . global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after
  (tree-sitter))

(provide 'init-lang-common)
;;; init-lang-common.el ends here
