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
  (setq lsp-modeline-code-action-fallback-icon "")
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

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
