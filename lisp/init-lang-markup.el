;;; init-lang-markup.el --- markdown & markup.
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.toml\\'" . conf-mode))

(use-package markdown-mode
  :defer t
  :commands markdown-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook 'display-line-numbers-mode))

(use-package yaml-mode
  :defer t
  :commands yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :config
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode))


(provide 'init-lang-markup)
;;; init-lang-markup.el ends here
