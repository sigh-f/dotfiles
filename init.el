;;; init.el --- User config.
;;; Commentary:
;;
;; Sections:
;; - Misc Builtins
;; - Org
;; - Programming (General)
;; - Programming (Language Support)
;; - UI
;; - M(elpa) Packages
;; - Local Overrides
;;
;;; Code:

;; Uncomment to measure startup time
;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;
;; Misc Builtin
;;

(defvar user-map (make-sparse-keymap) "Prefix for user keybinds.")
(defvar user-map/org (make-sparse-keymap) "Prefix for user org keybinds.")
(defvar user-map/vc (make-sparse-keymap) "Prefix for user version control keybinds.")
(global-set-key (kbd "C-j") user-map)
(define-key user-map (kbd "o") user-map/org)
(define-key user-map (kbd "v") user-map/vc)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      compilation-scroll-output t
      completion-styles '(basic partial-completion flex)
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

;;
;; Org
;;

(defun project-find-notes()
  (interactive)
  (when (project-current)
    (find-file-other-window (concat "~/org"
                                    "/project-notes/"
                                    (car (last (butlast (split-string (project-root (project-current)) "/"))))
                                    ".org"))))
(define-key global-map (kbd "C-x p o") 'project-find-notes)

(use-package org
  :defer t
  :straight (:type built-in)
  :config
  (setq org-cycle-separator-lines 1
        org-hide-emphasis-markers t
        org-todo-keywords '((sequence "TODO(t)" "WORK(w@)" "REVW(r@)" "DEPL(d@)" "|" "DONE(c)")
                            (sequence "WAIT(p@)" "LOCK(l@)" "|" "DEAD(k)")))
  (add-hook 'org-mode-hook 'org-indent-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (emacs-lisp . t)
     (makefile . t)
     (perl . t)
     (python . t)
     (ruby . t)
     (shell . t)))
  :bind
  (:map org-mode-map
        ("C-j" . nil)) ; user-map conflict
  (:map user-map/org
        ("f" . (lambda () (interactive) (ido-find-file-in-dir (expand-file-name "~/org"))))))

(use-package org-agenda
  :defer t
  :straight (:type built-in)
  :config
  (setq org-agenda-files (directory-files-recursively "~/org" "\\.org$"))
  :bind
  (:map user-map/org
        ("a" . org-agenda)))

(use-package org-capture
  :defer t
  :straight (:type built-in)
  :config
  (setq org-capture-templates
        '(("i" "Idea" entry (file+headline org-default-notes-file "Ideas")
           "** %? :idea:\nCreated: %T\n%i\n")
          ("t" "Todo" entry (file+headline org-default-notes-file "Triage later")
           "** TODO %? \nCreated: %T\n%i\n")))
  :bind
  (:map user-map/org
        ("c" . org-capture)))

(use-package org-modern
  :hook
  ((org-mode . org-modern-mode)
   (org-agenda-finalize . org-modern-agenda)))

;;
;; Programming (General)
;;

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Show git diff fringe indicators
(use-package diff-hl
  :hook
  (prog-mode . diff-hl-mode)
  (prog-mode . diff-hl-margin-mode)
  :config
  (setq diff-hl-show-staged-changes nil)
  :bind
  (:map user-map/vc
        ("a" . diff-hl-stage-current-hunk)
        ("k" . diff-hl-revert-hunk)
        ("p" . diff-hl-show-hunk-previous)
        ("n" . diff-hl-show-hunk-next)))

;; LSP integration general config; enabled on a per-language basis
(use-package lsp-mode
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-modeline-code-actions-mode nil)
  (setq read-process-output-max (* 1024 3072)) ;; 1mb
  (setq lsp-modeline-code-action-fallback-icon ""))
(use-package lsp-ui
  :after (lsp-mode)
  :config
  (setq lsp-ui-sideline-delay 1
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-update-mode "line"))

;; Syntax tree based parser
(use-package tree-sitter
  :hook (emacs-startup . global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :after (tree-sitter))

;;
;; Programming (Language Support)
;;

;; 00 - But first, config langs
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

;; C
(use-package cc-mode
  :straight (:type built-in)
  :config
  (setq c-basic-offset 4)
  :bind
  (:map c-mode-map
        ("C-c f p" . ff-find-other-file)))
(use-package reformatter
  :config
  (reformatter-define clang-reformatter :program "clang-format" :args '("-style={BasedOnStyle: Google, IndentWidth: 4, BreakBeforeBraces: Stroustrup, AlwaysBreakAfterReturnType: AllDefinitions, ColumnLimit: 120}"))
  (add-hook 'c-mode-hook 'clang-reformatter-on-save-mode))

;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook 'flymake-mode)
(define-key lisp-interaction-mode-map (kbd "C-j") nil) ; user-map prefix conflict
(use-package paren-face
  :hook (emacs-lisp-mode . paren-face-mode))

;; Python
(use-package lsp-mode
  :hook (python-mode . lsp))
(use-package lsp-pyright
  :after (lsp-mode)
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp)))
  :init
  (add-hook 'python-mode-hook (lambda () (add-hook 'before-save-hook 'lsp-pyright-organize-imports 0 t))))  ; or lsp-deferred
(use-package reformatter
  :config
  (reformatter-define black-reformatter :program "black" :args '("-"))
  (add-hook 'python-mode-hook 'black-reformatter-on-save-mode))

;; Ruby
(use-package lsp-mode
  :hook (ruby-mode . lsp))
(use-package ruby-end
  :hook (ruby-mode . ruby-end-mode))

;; Shell
(use-package lsp-mode
  :hook (sh-mode . lsp))

;;
;; UI
;;

(add-hook 'emacs-startup-hook (lambda() (menu-bar-mode -1)))
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(use-package centaur-tabs
  :hook
  ((emacs-startup . centaur-tabs-mode)
   (dired-mode . centaur-tabs-local-mode)
   (org-mode . centaur-tabs-local-mode))
  :config
  (setq centaur-tabs-set-close-button nil
        centaur-tabs-show-new-tab-button nil))

(use-package treemacs
  :config
  (treemacs-project-follow-mode)
  (setq treemacs-width 25
        treemacs--project-follow-delay 0.25)
  :bind
  (:map user-map
        ("C-j" . treemacs-select-window)))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package nordic-vein-theme
  :straight (:type git :repo "https://github.com/buttertone/nordic-vein")
  :init
  (add-hook 'emacs-startup-hook (load-theme 'nordic-vein t)))

;; (use-package doom-themes
;;   :init
;;   (add-hook 'emacs-startup-hook (load-theme 'doom-gruvbox t)))

;; Nicer modeline
(use-package doom-modeline
  :hook (emacs-startup . doom-modeline-mode))

;; TUI-compatible icons
(use-package nerd-icons
  :after (doom-modeline))
(use-package nerd-icons-dired
  :after (nerd-icons)
  :hook (dired-mode . nerd-icons-dired-mode))

;; Highlight non-user-input changes (e.g. undo)
(use-package volatile-highlights
  :hook
  (emacs-startup . volatile-highlights-mode))

;;
;; M(elpa) Packages
;;

;; Ace window - jump to window with transient hotkeys
(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  (:map global-map
        ("C-x o" . ace-window)))

;; Avy - jump to text with transient hotkeys
(use-package avy
  :config
  (setq avy-keys '(?a ?s ?d ?f ?j ?k ?l))
  :bind
  (:map user-map
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
        ("C-c m" . consult-man)
        ("M-y" . consult-yank-from-kill-ring)
        ("C-x p b" . consult-project-buffer)
        ("C-x b" . consult-buffer)))

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

;; Git timemachine - traverse commits of the current file
(use-package git-timemachine
  :config
  (add-hook 'git-timemachine-mode-hook 'display-line-numbers-mode)
  :bind
  (:map user-map/vc
        ("t" . git-timemachine)))

;; H(igh)l(ight) todo - make TODO/FIXME/HACK/etc. standout
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; Magit - git ui
(use-package magit
  :defer t)

;; Magit todos - show TODOs in the magit status buffer
(use-package magit-todos
  :after (magit)
  :hook (magit-mode . magit-todos-mode))

;; Marginalia - show documentation for candidates in the minibuffer
(use-package marginalia
  :hook (emacs-startup . marginalia-mode))

;; Persp mode - persistent workspace arrangements
(use-package persp-mode
  :hook (emacs-startup . persp-mode)
  :init
  (setq-default persp-auto-resume-time 0)
  :bind
  (:map user-map
        ("p" . persp-key-map)
        ("P" . (lambda () (interactive)(persp-load-state-from-file (concat persp-save-dir persp-auto-save-fname))))))

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

;;
;; Load any local config from ~/.config/emacs
;;

(let ((local-config-dir "~/.config/emacs"))
  (unless (file-exists-p local-config-dir)
    (make-directory local-config-dir))
  (cl-loop for file in (directory-files-recursively local-config-dir "\\.el$")
           do (condition-case err
                  (load file)
                ('error (message (format "failed to load local config file: %s %s" file err))))))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c" "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176" "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36" "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc" "e312bb200ab9012ae2061fe586ac844c57df5e5fc4e62c28c63f3a502666f304" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-insert ((t (:background "#4c9e8a" :foreground "#4c9e8a")))))
