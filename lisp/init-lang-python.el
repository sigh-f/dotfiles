;;; init-lang-python.el --- python.
;;; Commentary:
;;; Code:

(use-package lsp-pyright
  :after
  (lsp-mode)
  :hook
  (python-mode . (lambda () (require 'lsp-pyright) (lsp)))
  :init
  (add-hook 'python-mode-hook (lambda () (add-hook 'before-save-hook 'lsp-pyright-organize-imports 0 t))))

(use-package reformatter
  :config
  (reformatter-define black-reformatter :program "black" :args '("-"))
  (add-hook 'python-mode-hook 'black-reformatter-on-save-mode))

(provide 'init-lang-python)
;;; init-lang-python.el ends here
