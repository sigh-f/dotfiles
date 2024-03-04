;;; init-emacs.el --- emacs builtins.
;;; Commentary:
;;; Code:

(define-key user-map (kbd "C-k") #'kill-buffer-and-window)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      completion-styles '(basic partial-completion flex)
      truncate-lines nil
      vc-follow-symlinks t)

(setq-default indent-tabs-mode nil
              tab-width 4
              truncate-lines nil)

(define-key global-map (kbd "M-;") 'comment-line)
(define-key global-map (kbd "C-k") 'kill-whole-line)
(define-key user-map (kbd "M-8") 'point-to-register) ; HACK map C-; -> M-8
(define-key user-map (kbd "M-9") 'jump-to-register)  ; HACK map C-' -> M-9
(define-key global-map (kbd "M-m") 'scroll-other-window)      ; HACK: map C-, -> M-m
(define-key global-map (kbd "M-o") 'scroll-other-window-down) ; HACK: map C-. -> M-o

(defun user/open-line-below (arg)
  "Create ARG new lines below the current line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (indent-according-to-mode))
(define-key global-map (kbd "C-o") 'user/open-line-below)

(defun user/open-line-above (arg)
  "Create ARG new lines above the current line."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))
(define-key global-map (kbd "C-q") 'user/open-line-above)

(provide 'init-emacs)
;;; init-emacs.el ends here
