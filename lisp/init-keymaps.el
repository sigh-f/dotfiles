;;; init-keymaps.el --- keymaps.
;;; Commentary:
;;; Code:

(defvar user-map (make-sparse-keymap))
(defvar user-map/org (make-sparse-keymap))
(defvar user-map/vc (make-sparse-keymap))
(global-set-key (kbd "C-j") user-map)
(define-key user-map (kbd "o") user-map/org)
(define-key user-map (kbd "v") user-map/vc)

(provide 'init-keymaps)
;;; init-keymaps.el ends here
