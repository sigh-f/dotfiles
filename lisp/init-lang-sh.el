;;; init-lang-sh.el --- (ba)sh
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :hook
  (sh-mode . lsp))

(provide 'init-lang-sh)
;;; init-lang-sh.el ends here
