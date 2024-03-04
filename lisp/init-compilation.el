;;; init-compilation.el --- compilation.
;;; Commentary:
;;; Code:

(defvar user-map/compilation (make-sparse-keymap))
(define-key user-map (kbd "C-c") user-map/compilation)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
(setq ;; Follow output in the compilation buffer
      compilation-scroll-output t
      ;; Highlight after jumping to an error
      next-error-message-highlight t
      ;; How long to highlight after jumping to an error
      next-error-highlight 5
      next-error-highlight-no-select 5)

(defun user/compile-dwim ()
  "Run `project-compile` or `compile`."
  (interactive)
  (let ((project (project-current)))
    (if project
        (project-compile)
      (compile))))
(global-set-key (kbd "C-x p c") #'user/compile-dwim)
(define-key user-map/compilation (kbd "c") 'compile)
(define-key user-map/compilation (kbd "g") 'recompile)
(define-key user-map/compilation (kbd "k") 'kill-compilation)
;; (Anywhere) jump to [next|previous] error from the last compilation, AND
;; move point to the corresponding code.
(define-key user-map/compilation (kbd "C-n") 'next-error)
(define-key user-map/compilation (kbd "C-p") 'previous-error)

;; By default compile commands will use a single *compilation* buffer,
;; preventing multiple concurrent compilations. Provide a unique naming
;; function to avoid conflicts. Using setq-default instead of setq to
;; avoid breaking packages that may use compilation buffers with
;; buffer-local naming schemes.
(defun user/current-compilation-buffer-name (&rest _args)
  "Renames the *compilation* buffer to be unique per project."
  (let ((project (project-current)))
    (if project
        (concat "*" (project-root project) " compilation*")
      (concat "*" (buffer-name) " compilation*")))) ;; buffer-name in the start hook is the compilatino buffer
(setq-default compilation-buffer-name-function 'user/current-compilation-buffer-name)

(defun user/find-current-compilation-buffer-other-window ()
  "Find the current compilation buffer if it exists."
  (interactive)
  (let ((compilation-buffer-name (user/current-compilation-buffer-name)))
    (if (get-buffer compilation-buffer-name)
        (switch-to-buffer-other-window compilation-buffer-name)
      (message "No compilation buffer"))))
(define-key user-map/compilation (kbd "C-j") #'user/find-current-compilation-buffer-other-window)

;; Hide compilation buffers until they terminate
(add-hook 'compilation-start-hook (lambda (&rest _args) (delete-window (get-buffer-window (buffer-name)))))
(add-hook 'compilation-finish-functions 'switch-to-buffer-other-window 'compilation)

(provide 'init-compilation)
;;; init-compilation.el ends here
