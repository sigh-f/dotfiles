;;; init.el --- emacs init.
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-keymaps)
(require 'init-compilation)
(require 'init-persp)
(require 'init-emacs)
(setq org-agenda-files (directory-files-recursively "~/org" "\\.org$"))
(require 'init-org)
(require 'init-version-control)
(require 'init-ui)

;; TODO: learn artist-mode

(require 'init-lang-common)
(require 'init-lang-c)
(require 'init-lang-emacs-lisp)
(require 'init-lang-markup)
(require 'init-lang-python)
(require 'init-lang-ruby)
(require 'init-lang-sh)

(require 'init-treemacs)


(define-key user-map (kbd "C-d") 'zap-up-to-char)




(use-package pulsar
  :hook
  (emacs-startup . pulsar-global-mode))


;; Highlight non-user-input changes (e.g. undo)
(use-package volatile-highlights
  :hook
  (emacs-startup . volatile-highlights-mode))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  (:map global-map
        ("C-x o" . ace-window)))

(use-package avy
  :config
  (setq avy-keys '(?a ?s ?d ?f ?j ?k ?l))
  :bind
  (:map user-map
        ("c l" . avy-copy-line)
        ("c r" . avy-copy-region)
        ("C-f" . avy-goto-word-1)
        ("C-l" . avy-goto-line)
        ("k l" . avy-kill-whole-line)
        ("k r" . avy-kill-region)))

;; Change inner - provide vim 'c-a' & 'c-i'
(use-package change-inner
  :bind
  (:map user-map
        ("C-a" . change-outer)
        ("TAB" . change-inner))) ; HACK-y: C-i sends TAB in most terminals

;; Company - autocomplete
;; TODO: try corfu?
(use-package company
  :hook ((conf-mode org-mode prog-mode) . company-mode)
  :config
  (setq company-idle-delay 0.25
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t))

;; Consult - various builtin function enhancements
(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :bind
  (:map user-map
        ("g" . consult-grep)
        ("G" . consult-git-grep))
  (:map global-map
        ;; ("C-c m" . consult-man)
        ("M-y" . consult-yank-from-kill-ring)))
        ;; ("C-x p b" . consult-project-buffer)
        ;; ("C-x b" . consult-buffer)))

;; Dumb jump - grep-based xref backend
(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate))

;; Flymake - linter
(use-package flymake
  :bind
  (:map user-map
        ("e l" . flymake-show-buffer-diagnostics)
        ("e p" . flymake-goto-prev-error)
        ("e n" . flymake-goto-next-error)))

;; Flymake popon - tui compatible popup diagnostics
(use-package flymake-popon
  :hook (flymake-mode . flymake-popon-mode))

;; Gcmh - smart gc-threshold management
(use-package gcmh
  :hook (emacs-startup . gcmh-mode))


;; H(igh)l(ight) todo - make TODO/FIXME/HACK/etc. standout
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; Marginalia - show documentation for candidates in the minibuffer
(use-package marginalia
  :hook (emacs-startup . marginalia-mode))

;; Vertico - interactive minibuffer candidate filtering
(use-package vertico
  :hook (emacs-startup . vertico-mode))

;; Which key - transient popups to show available keybinds mid keystroke
(use-package which-key
  :hook (emacs-startup . which-key-mode))

;; Whitespace cleanup mode - non-obtrusive whitespace cleanup on save
(use-package whitespace-cleanup-mode
  :hook (emacs-startup . global-whitespace-cleanup-mode))

;; Winner - undo/redo for the window arrangement
(use-package winner
  :straight (:type built-in)
  :hook (emacs-startup . winner-mode)
  :bind
  (:map user-map
        ("C-n" . winner-redo)
        ("C-p" . winner-undo)))

;; Yasnippet - persistent text templates
(use-package yasnippet
  :hook (emacs-startup . yas-global-mode)
  :bind
  (:map user-map
        ("f s" . yas-visit-snippet-file)
        ("i s" . yas-insert-snippet)
        ("n s" . yas-new-snippet)))

(require 'init-local-config)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c" "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176" "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36" "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc" "e312bb200ab9012ae2061fe586ac844c57df5e5fc4e62c28c63f3a502666f304" default))
 '(highlight-indent-guides-method 'character)
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-insert ((t (:background "#4c9e8a" :foreground "#4c9e8a"))))
 '(italic ((t (:foreground "#bb9af7" :slant italic))))
 '(org-code ((t (:inherit shadow :foreground "#e0af68")))))
