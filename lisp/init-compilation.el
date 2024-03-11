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

;; 'compile' and 'project-compile' reference the 'compile-command' variable
;; for the default command(s). Consequently, neither commands correctly
;; preserve compilation history. Below, 'user/compile-dwim' and 'user/recompile-dwim'
;; are defined to add context support; specifically, 'compile-command' is made
;; local to the current project, or the current buffer if no project is active.
;; 'compile-command' is effectively split into a hash/alist.
(defvar context-aware-compile-command-alist '() "Mapping of compilable-entity to last compile command.")

(defun user/get-compilation-context ()
  (if (project-current)
      (project-root (project-current))
    (buffer-name)))

(defun user/get-context-aware-compile-command (context)
  (or (cdr (assoc context context-aware-compile-command-alist)) ""))

(defun user/compile-dwim ()
  "Compile with project/buffer history context awareness."
  (interactive)
  (let* ((context (if (project-current) (project-root (project-current)) (buffer-name)))
         (compile-command (or (cdr (assoc context context-aware-compile-command-alist)) ""))
         (new-context-p (eq compile-command "")))
    (call-interactively #'compile)
    (if new-context-p
        (add-to-list 'context-aware-compile-command-alist (list context compile-command))
      (setf (cdr (assoc context context-aware-compile-command-alist)) compile-command))))

(defun user/recompile-dwim ()
  "Recompile with project/buffer history context awareness."
  (interactive)
  (let* ((context-key (if (project-current) (project-root (project-current)) (buffer-name)))
         (compile-command (or (cdr (assoc context-key context-aware-compile-command-alist)) ""))
         (new-context-p (eq compile-command "")))
    (if new-context-p
        (message "No known compile command for the current context.")
      (compile compile-command))))

(define-key user-map/compilation (kbd "c") 'user/compile-dwim)
(define-key user-map/compilation (kbd "g") 'user/recompile-dwim)
(define-key user-map/compilation (kbd "k") 'kill-compilation) ;; TODO: add context

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
