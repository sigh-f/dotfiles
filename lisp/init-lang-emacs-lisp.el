;;; init-lang-emacs-lisp.el --- emacs lisp.
;;; Commentary:
;;; Code:

(add-hook 'emacs-lisp-mode-hook 'flymake-mode)
(define-key lisp-interaction-mode-map (kbd "C-j") nil) ; user-map prefix conflict

(use-package paren-face
  :hook (emacs-lisp-mode . paren-face-mode))

(provide 'init-lang-emacs-lisp)
;;; init-lang-emacs-lisp.el ends here
