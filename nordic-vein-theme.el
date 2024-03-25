;;; nordic-vein-theme.el --- A medium-contrast holographic theme.
;; Version: 0.0.1

;;; Commentary:
;; Things that need to be done before considering a v1 release
;; - TODO: fix glitch-$color-[bg|bg-hl|fg|fg-hl] pairings
;; - TODO: with-eval-after-load external packages?
;; - TODO: make primary & secondary selection configurable by ansi color
;; - TODO: figure out the correct minver (probably 27.1)
;; - TODO: thorough scan through available groups to cover all faces

;;; Code:

(deftheme nordic-vein "A medium-contrast holographic theme.")
(let* ((class '((class color) (min-colors 89)))

      ;;; = Palette =

       ;; == Text ==

       (bg                     "#24283b")
       ;; (dark-bg "#1c2030")
       ;; (dark-bg "#1a1e2d")
       (test-dark-bg "#1e2333")
       (dark-bg "#181c2a")
       (active-bg              "#2f354b")
       (active-bg-2              "#292e42")
       (active-bg-hl           "#414868")

       (fg                     "#a9b1d6")
       (active-fg              "#c0caf5")
       (muted-fg               "#565f89")     ; inactive, but visible
       (inactive-fg            "#313750")     ; inactive, nearly hidden

       ;; == Colors ==

       (glitch-black-bg        "#313750")
       (glitch-black-bg-hl     "#1f2335")
       (glitch-black-fg        "#1f2335")
       (glitch-black-fg-hl     "#565f89")

       ;; Functions, builtins
       (glitch-blue-bg         "#192a4d")
       (glitch-blue-bg-hl      "#7aa2f7")
       (glitch-blue-fg         "#7aa2f7")
       (glitch-blue-fg-hl      "#031130")

       ;; Variables, object properties
       (glitch-cyan-bg         "#274440")
       (glitch-cyan-bg-hl      "#73daca")
       (glitch-cyan-fg         "#73daca")
       (glitch-cyan-fg-hl      "#222a29")
       (nord-cyan "#8FBCBB")

       ;; Info/Success
       (glitch-green-bg        "#273644")
       (glitch-green-bg-hl     "#4c9e8a")
       (glitch-green-fg        "#4c9e8a")
       (glitch-green-fg-hl     "#11221e")

       ;; Keywords
       (glitch-magenta-bg      "#231c31")
       (glitch-magenta-bg-hl   "#bb9af7")
       ;; (glitch-magenta-fg      "#bb9af7")
       ;; (glitch-magenta-fg "#db6582")
       (glitch-magenta-fg "#eb6f92")
       (glitch-magenta-fg-hl   "#171320")

       ;; Errors
       (glitch-red-bg          "#342c3c")
       (glitch-red-bg-hl       "#f7768e")
       (glitch-red-fg          "#f7768e")
       (glitch-red-fg-hl       "#30030b")

       (glitch-white           "#c0caf5")
       (glitch-white-bg-hl     "#bb9af7")
       (glitch-white-fg        "#c0caf5")
       (glitch-white-fg-hl     "#bb9af7")

       ;; Warnings
       (glitch-yellow-bg       "#37363f")
       (glitch-yellow-bg-hl    "#e0af68")
       (glitch-yellow-fg       "#e0af68")
       (glitch-yellow-fg-hl    "#2a1d09")

       ;; == Diagnostics ==

       (info-bg                glitch-green-bg)
       (info-bg-hl             glitch-green-bg-hl)
       (info-fg                glitch-green-fg)
       (info-fg-hl             glitch-green-fg-hl)

       (alt-info-bg            glitch-blue-bg)     ; non-user select highlight (volatile highlights)
       (alt-info-bg-hl         glitch-blue-bg-hl)
       (alt-info-fg            glitch-blue-fg)
       (alt-info-fg-hl         glitch-blue-fg-hl)

       (warning-bg             glitch-yellow-bg)
       (warning-bg-hl          glitch-yellow-bg-hl)
       (warning-fg             glitch-yellow-fg)
       (warning-fg-hl          glitch-yellow-fg-hl)

       (error-bg               glitch-red-bg)
       (error-bg-hl            glitch-red-bg-hl)
       (error-fg               glitch-red-fg)
       (error-fg-hl            glitch-red-fg-hl)

      ;;; == Misc ==

       (qualifier         "#2ac3de")     ; types
       (safe              "#7892bf")     ; constants, numbers, strings
       ;; (special           "#f6c177")     ; escape sequences
       (special "#eac086")
       (types "#7aa2f7")
       )
  ;; (setq-default org-modern-todo-faces
  ;;       '(("BLOCK" :background "#f7768e" :foreground "#181c2a" :weight bold)
  ;;         ("DEPLOY" :background "#bb9af7" :foreground "#181c2a" :weight bold)
  ;;         ("DONE" :background "#181c2a" :foreground "#565f89")
  ;;         ("REVIEW" :background "#7aa2f7" :foreground "#181c2a")
  ;;         ("TODO" :background "#e0af68" :foreground "#181c2a")
  ;;         ("WAIT" :background "#f7768e" :foreground "#181c2a")
  ;;         ("WIP" :background "#4c9e8a" :foreground "#181c2a" :weight bold)))
  (setq-default org-modern-todo-faces
                '(("LOCK" :background "#1c2030" :foreground "#f7768e" :weight bold)
                  ("DEAD" :background "#1c2030" :foreground "#565f89" :weight bold)
                  ("DEPL" :background "#1c2030" :foreground "#bb9af7" :weight bold)
                  ("DONE" :background "#1c2030" :foreground "#565f89" :weight bold)
                  ("REVW" :background "#1c2030" :foreground "#7aa2f7" :weight bold)
                  ("TODO" :background "#1c2030" :foreground "#e0af68" :weight bold)
                  ("WAIT" :background "#1c2030" :foreground "#f7768e" :weight bold)
                  ("WORK" :background "#1c2030" :foreground "#4c9e8a" :weight bold)))
  (setq-default org-modern-priority-faces
                '((?A :background "#f7768e" :foreground "#181c2a" :weight bold)
                  (?B :background "#e0af68" :foreground "#181c2a" :weight bold)
                  (?C :background "#4c9e8a" :foreground "#181c2a" :weight bold)))

  (custom-theme-set-faces
   'nordic-vein

   ;;; Basic Faces

   `(bold                          ((,class (:weight bold))))
   `(bold-italic                   ((,class (:weight bold :slant italic))))
   `(border                        ((,class (:foreground ,dark-bg))))
   `(button                        ((,class (:foreground ,glitch-blue-fg :weight bold))))
   `(child-frame-border            ((,class (:inherit border))))
   `(cursor                        ((,class (:background ,active-fg :foreground ,dark-bg))))
   `(default                       ((,class (:background ,bg :foreground ,fg))))
   `(error                         ((,class (:foreground ,glitch-red-fg))))
   `(escape-glyph                  ((,class (:foreground ,special))))
   `(fill-column-indicator         ((,class (:foreground ,inactive-fg))))
   `(fixed-pitch                   ((,class (:family "monospace"))))
   `(fixed-pitch-serif             ((,class (:family "monospace serif"))))
   `(fringe                        ((,class (:background ,dark-bg))))
   `(glyphless-char                ((,class (:foreground ,special))))
   `(header-line                   ((,class (:inherit mode-line :background ,test-dark-bg :foreground "#a9b1d6"))))
   `(header-line-highlight         ((,class (:inherit mode-line-active))))
   `(highlight                     ((,class (:background ,active-bg :foreground ,active-fg))))
   `(homoglyph                     ((,class (:foreground ,special))))
   `(internal-border               ((,class (:inherit border))))
   `(isearch                       ((,class (:background ,info-fg :foreground ,info-bg))))
   `(isearch-fail                  ((,class (:background ,error-fg :foreground ,error-bg))))
   `(italic                        ((,class (:slant italic))))
   `(lazy-highlight                ((,class (:background ,info-bg :foreground ,info-fg))))
   `(line-number                   ((,class (:foreground ,inactive-fg))))
   `(line-number-current-line      ((,class (:inherit highlight))))
   `(line-number-major-tick        ((,class (:background ,active-bg-2 :foreground ,muted-fg))))
   `(line-number-minor-tick        ((,class (:inherit line-number-major-tick))))
   `(link                          ((,class (:foreground ,glitch-cyan-fg :weight bold))))
   `(link-visited                  ((,class (:inherit link :weight normal))))
   `(match                         ((,class (:inherit isearch))))
   `(menu                          ((,class (:background ,dark-bg :foreground ,muted-fg))))
   `(minibuffer-prompt             ((,class (:foreground ,glitch-blue-fg))))
   `(mode-line                     ((,class (:inherit menu :background ,bg :foreground ,muted-fg))))
   `(mode-line-active              ((,class (:inherit highlight :background ,test-dark-bg :foreground "#a9b1d6"))))
   `(mode-line-buffer-id           ((,class (:weight bold))))
   `(mode-line-buffer-id-inactive  ((,class (:inherit mode-line-buffer-id))))
   `(mode-line-emphasis            ((,class (:weight bold))))
   `(mode-line-highlight           ((,class (:background ,active-bg-hl :foreground ,active-fg))))
   `(mode-line-inactive            ((,class (:inherit mode-line :foreground ,muted-fg))))
   `(mouse                         ((,class (:foreground ,active-fg))))
   `(nobreak-hyphen                ((,class (:foreground ,special))))
   `(nobreak-space                 ((,class (:foreground ,special))))
   ;; TODO: what is this
   ;; `(read-multiple-choice-face     ((,class (:background ,active-bg :foreground ,active-fg))))
   `(region                        ((,class (:background ,info-bg :foreground ,info-fg))))
   `(scroll-bar                    ((,class (:foreground ,inactive-fg))))
   `(secondary-selection           ((,class (:background ,alt-info-bg :foreground ,alt-info-fg))))
   `(shadow                        ((,class (:foreground ,muted-fg))))
   `(success (                     (,class (:foreground ,info-fg))))
   `(tab-bar                       ((,class (:inherit menu))))
   `(tab-line                      ((,class (:inherit menu))))
   `(tool-bar                      ((,class (:inherit menu))))
   `(tool-tips                     ((,class (:inherit highlight))))
   `(trailing-whitespace           ((,class (:background ,error-bg-hl :foreground ,error-fg-hl))))
   `(tty-menu-disabled-face        ((,class (:inherit menu))))
   `(tty-menu-enabled-face         ((,class (:inherit menu :foreground ,fg))))
   `(tty-menu-selected-face        ((,class (:inherit menu :foreground ,active-fg))))
   `(underline                     ((,class (:underline t))))
   `(variable-pitch                ((,class (:family "sans serif"))))
   `(variable-pitch-text           ((,class (:family "sans serif"))))
   ;; `(vertical-border ((,class (:foreground ,dark-bg :background ,bg))))
   `(vertical-border ((,class (:foreground "#1e2333"))))
   ;; `(vertical-border               ((,class (:inherit border))))
   `(warning                       ((,class (:foreground ,warning-fg))))
   `(window-divider                ((,class (:foreground ,dark-bg))))
   `(window-divider-first-pixel    ((,class (:inherit window-divider))))
   `(window-divider-last-pixel     ((,class (:inherit window-divider))))

   `(hl-line                       ((,class (:background ,active-bg))))
   `(show-paren-match ((,class (:background ,active-bg-hl :foreground ,active-fg))))
   `(show-paren-match-expression ((,class (:inherit show-paren-match))))
   `(show-paren-mismatch ((,class (:background ,error-bg-hl :foreground ,error-fg-hl))))
   `(completions-common-part ((,class (:inherit region))))

   `(term-color-black ((,class (:foreground ,glitch-black-fg))))
   `(term-color-bright-black ((,class (:foreground ,glitch-black-fg))))
   `(term-color-white ((,class (:foreground ,glitch-white-fg))))
   `(term-color-bright-white ((,class (:foreground ,glitch-white-fg))))
   `(term-color-blue ((,class (:foreground ,glitch-blue-fg))))
   `(term-color-bright-blue ((,class (:foreground ,glitch-blue-fg))))
   `(term-color-red ((,class (:foreground ,glitch-red-fg))))
   `(term-color-bright-red ((,class (:foreground ,glitch-red-fg))))
   `(term-color-yellow ((,class (:foreground ,glitch-yellow-fg))))
   `(term-color-bright-yellow ((,class (:foreground ,glitch-yellow-fg))))
   `(term-color-cyan ((,class (:foreground ,glitch-cyan-fg))))
   `(term-color-bright-cyan ((,class (:foreground ,glitch-cyan-fg))))
   `(term-color-green ((,class (:foreground ,glitch-green-fg))))
   `(term-color-bright-green ((,class (:foreground ,glitch-green-fg))))
   `(term-color-magenta ((,class (:foreground ,glitch-magenta-fg))))
   `(term-color-bright-magenta ((,class (:foreground ,glitch-magenta-fg))))

   ;;; = Dired =

   `(dired-broken-symlink          ((,class (:foreground ,glitch-red-fg))))
   `(dired-directory               ((,class (:foreground ,glitch-blue-fg))))
   `(dired-flagged                 ((,class (:foreground ,glitch-red-fg))))
   `(dired-header                  ((,class (:foreground ,glitch-blue-fg))))
   `(dired-ignored                 ((,class (:foreground ,muted-fg))))
   `(dired-marked                  ((,class (:foreground ,glitch-yellow-fg))))
   `(dired-perm-write              ((,class (:foreground ,muted-fg))))
   `(dired-set-id                  ((,class (:foreground ,glitch-cyan-fg))))
   `(dired-special                 ((,class (:foreground ,special))))
   `(dired-symlink                 ((,class (:foreground ,glitch-magenta-fg))))
   `(dired-warning                 ((,class (:foreground ,warning-fg :background ,warning-bg))))

   ;;; = Flymake =

   `(flymake-end-of-line-diagnostics-face ((,class (:background ,info-bg :foreground ,info-fg))))
   `(flymake-error ((,class (:background ,error-bg :foreground ,error-fg))))
   `(flymake-error-echo ((,class (:inherit flymake-error))))
   `(flymake-error-echo-at-eol ((,class (:inherit flymake-error))))
   `(flymake-note ((,class (:background ,info-bg :foreground ,info-fg))))
   `(flymake-note-echo ((,class (:inherit flymake-note))))
   `(flymake-note-echo-at-eol ((,class (:inherit flymake-note))))
   `(flymake-note-echo ((,class (:inherit flymake-note :background ,dark-bg))))
   `(flymake-note-echo-at-eol ((,class (:inherit flymake-note :background ,dark-bg))))
   `(flymake-warning ((,class (:background ,warning-bg :foreground ,warning-fg))))
   `(flymake-warning-echo ((,class (:inherit flymake-warning))))
   `(flymake-warning-echo-at-eol ((,class (:inherit flymake-warning))))

   ;;; = Org =

   `(org-modern-statistics ((,class (:background ,glitch-green-bg :foreground ,glitch-green-fg))))
   `(org-modern-tag ((,class (:background ,active-bg-hl :foreground ,active-fg))))

   `(outline-1 ((,class (:foreground ,glitch-blue-fg))))
   `(outline-2 ((,class (:foreground ,safe))))
   `(outline-3 ((,class (:foreground ,glitch-cyan-fg))))
   `(outline-4 ((,class (:foreground ,glitch-magenta-fg))))
   `(outline-5 ((,class (:foreground ,glitch-magenta-fg))))
   `(outline-6 ((,class (:foreground ,glitch-yellow-fg))))
   `(outline-7 ((,class (:foreground ,glitch-red-fg))))
   `(outline-8 ((,class (:foreground ,special))))

   `(org-agenda-column-dateline ((,class (:inherit menu))))
   `(org-agenda-date ((,class (:inherit shadow))))
   `(org-agenda-date-today ((,class (:foreground "#4c9e8a" :weight bold))))
   `(org-agenda-date-weekend ((,class (:inherit org-agenda-date))))
   `(org-agenda-date-weekend-today ((,class (:inherit org-agenda-date-today))))
   `(org-agenda-dimmed-todo-face ((,class (:inherit shadow))))
   `(org-agenda-done ((,class (:inherit shadow))))
   `(org-agenda-restriction-lock ((,class (:inherit menu))))
   `(org-agenda-structure ((,class (:foreground ,glitch-blue-fg :weight bold))))
   `(org-agenda-structure-filter ((,class (:inherit lazy-highlight))))
   `(org-archived ((,class (:inherit shadow))))
   `(org-block ((,class (:background ,dark-bg :foreground ,fg))))
   `(org-checkbox ((,class (:inherit highlight :foreground ,active-fg))))
   `(org-clock-overlay ((,class (:inherit secondary-selection))))
   `(org-column ((,class (:background ,dark-bg :foreground ,fg))))
   `(org-column-title ((,class (:inherit org-column :weight bold))))
   `(org-date ((,class (:foreground ,glitch-cyan-fg))))
   `(org-date-selected ((,class (:background ,alt-info-bg-hl :foreground ,alt-info-fg-hl))))
   `(org-tag ((,class (:background ,glitch-green-bg :foreground ,glitch-green-fg))))
   `(org-imminent-deadline ((,class (:foreground ,glitch-red-fg :weight bold))))
   `(org-dispatcher-highlight ((,class (:inherit lazy-highlight))))
   `(org-document-info ((,class (:foreground ,safe))))
   `(org-document-title ((,class (:foreground ,safe :weight bold))))
   `(org-done ((,class (:inherit shadow))))
   `(org-drawer ((,class (:inherit shadow))))
   `(org-ellipsis ((,class (:foreground ,special))))
   `(org-footnote ((,class (:foreground ,glitch-magenta-fg))))
   `(org-formula ((,class (:foreground ,special))))
   `(org-habit-alert-face ((,class (:background ,warning-bg :foreground ,warning-fg))))
   `(org-habit-alert-future-face ((,class (:inherit org-habit-alert))))
   `(org-habit-clear-face ((,class (:background ,alt-info-bg :foreground ,alt-info-fg))))
   `(org-habit-clear-future-face ((,class (:inherit org-habit-clear-face))))
   `(org-habit-overdue-face ((,class (:background ,error-bg :foreground ,error-fg))))
   `(org-habit-overdue-future-face ((,class (:inherit org-habit-overdue-face))))
   `(org-habit-ready-face ((,class (:background ,info-bg :foreground ,info-fg))))
   `(org-habit-ready-future-face ((,class (:inherit org-habit-ready-face))))
   `(org-headline-done ((,class (:inherit org-done))))
   `(org-headline-todo ((,class (:inherit org-todo))))
   `(org-latex-and-related ((,class (:foreground ,special))))
   `(org-meta-line ((,class (:inherit (shadow menu)))))
   `(org-mode-line-clock ((,class (:foreground ,glitch-cyan-fg))))
   `(org-mode-line-clock-overrun ((,class (:background ,error-bg-hl :foreground ,error-fg-hl))))
   `(org-priority ((,class (:foreground ,glitch-blue-fg))))
   `(org-scheduled ((,class (:foreground ,info-fg))))
   `(org-scheduled-previously ((,class (:inherit warning))))
   `(org-scheduled-today ((,class (:inherit org-scheduled))))
   `(org-sexp-date ((,class (:foreground ,glitch-magenta-fg))))
   `(org-special-keyword ((,class (:inherit font-lock-keyword-face))))
   `(org-table ((,class (:foreground ,muted-fg))))
   `(org-table-header ((,class (:inherit menu))))
   `(org-time-grid ((,class (:inherit org-table))))
   `(org-todo ((,class (:background ,glitch-yellow-bg :foreground ,glitch-yellow-fg))))
   `(org-upcoming-deadline ((,class (:inherit warning))))

   ;; compilation-mode
   `(compilation-info              ((,class (:foreground ,info-fg))))
   `(compilation-error             ((,class (:foreground ,error-fg))))
   `(compilation-warning           ((,class (:foreground ,warning-fg))))
   `(compilation-line-number       ((,class (:foreground ,glitch-blue-fg))))
   `(compilation-column-number     ((,class (:foreground ,muted-fg))))
   `(compilation-mode-line-run     ((,class (:foreground ,glitch-yellow-fg))))
   `(compilation-mode-line-exit     ((,class (:foreground ,info-fg :weight bold))))
   `(compilation-mode-line-fail     ((,class (:foreground ,error-fg :weight bold))))

   ;;; = Syntax Highlighting =

   `(font-lock-builtin-face ((,class (:foreground ,glitch-blue-fg))))
   `(font-lock-comment-face ((,class (:foreground ,muted-fg :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,safe))))
   `(font-lock-doc-face ((,class (:inherit font-lock-comment-face))))
   `(font-lock-function-name-face ((,class (:foreground ,special))))
   `(font-lock-function-call-face ((,class (:inherit font-lock-function-name-face :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,glitch-magenta-fg))))
   `(font-lock-negation-char-face ((,class (:foreground ,glitch-red-fg))))
   `(font-lock-number-face ((,class (:foreground ,safe))))
   `(font-lock-operator-face ((,class (:inherit font-lock-function-call-face))))
   `(font-lock-property-name-face ((,class (:foreground ,glitch-cyan-fg))))
   `(font-lock-property-use-face ((,class (:foreground ,glitch-cyan-fg))))
   `(font-lock-string-face ((,class (:foreground ,safe))))
   `(font-lock-type-face ((,class (:foreground ,types))))
   `(font-lock-variable-name-face ((,class (:foreground ,glitch-cyan-fg))))
   `(font-lock-variable-use-face ((,class (:foreground ,glitch-cyan-fg))))
   `(font-lock-warning-face ((,class (:inherit warning))))

   ;;; = External Packages =

   ;; == ace-window ==

   `(aw-background-face ((,class (:foreground ,inactive-fg))))
   ;; TODO move this
   `(centaur-tabs-dim-buffer-face ((,class (:foreground ,inactive-fg))))
   `(aw-key-face ((,class (:foreground ,glitch-blue-fg))))
   `(aw-leading-char-face ((,class (:foreground ,glitch-green-fg :weight bold))))
   `(aw-minibuffer-leading-char-face ((,class (:inherit aw-leading-char-face))))
   `(aw-mode-line-face ((,class (:foreground ,fg :weight bold))))

   ;; == avy ==

   `(avy-goto-char-timer-face ((,class (:inherit lazy-highlight))))
   `(avy-lead-face ((,class (:inherit isearch))))
   `(avy-lead-face-0 ((,class (:inherit avy-lead-face))))
   `(avy-lead-face-1 ((,class (:background ,error-bg-hl :foreground ,error-fg-hl))))
   `(avy-lead-face-2 ((,class (:background ,alt-info-bg-hl :foreground ,alt-info-fg-hl))))

   ;;; = centaur-tabs =

   `(centaur-tabs-active-bar-face ((,class (:foreground ,active-fg))))
   `(centaur-tabs-close-mouse-face ((,class (:foreground ,active-fg))))
   `(centaur-tabs-close-selected ((,class (:inverse-video t))))
   `(centaur-tabs-close-unselected ((,class (:inherit centaur-tabs-unselected))))

   `(centaur-tabs-default ((,class (:inherit default))))

   `(centaur-tabs-jump-identifier-selected ((,class (:inherit centaur-tabs-selected :foreground ,glitch-green-fg))))
   `(centaur-tabs-jump-identifier-unselected ((,class (:inherit centaur-tabs-unselected :foreground ,active-fg))))

   `(centaur-tabs-modified-marker-selected ((,class (:inherit centaur-tabs-selected))))
   `(centaur-tabs-modified-marker-unselected ((,class (:inherit centaur-tabs-unselected))))

   `(centaur-tabs-name-mouse-face ((,class (:foreground ,active-fg))))

   `(centaur-tabs-selected ((,class (:inherit highlight :foreground ,glitch-green-fg))))
   ;; `(centaur-tabs-selected ((,class (:inherit highlight :background ,nord-cyan :foreground ,muted-fg))))
   `(centaur-tabs-selected-modified ((,class (:inherit centaur-tabs-selected :foreground ,warning-fg))))
   `(centaur-tabs-unselected ((,class (:inherit tab-line :foreground ,muted-fg))))
   `(centaur-tabs-unselected-modified ((,class (:inherit centaur-tabs-unselected :foreground ,glitch-yellow-fg))))

   ;; == company ==

   `(company-echo ((,class (:inherit default))))
   `(company-echo-common ((,class (:background ,info-bg :foreground ,info-fg))))

   `(company-preview ((,class (:inherit highlight))))
   `(company-preview-common ((,class (:inherit company-echo-common))))
   `(company-preview-search ((,class (:inherit company-echo-common))))

   `(company-template-field ((,class (:inherit highlight))))

   `(company-tooltip ((,class (:inherit default :background ,dark-bg))))
   `(company-tooltip-annotation ((,class (:foreground ,glitch-magenta-fg))))
   `(company-tooltip-common ((,class (:inherit company-echo-common))))
   `(company-tooltip-common-selection ((,class (:inherit company-tooltip-common))))
   `(company-tooltip-deprecated ((,class (:foreground ,special))))
   `(company-tooltip-mouse ((,class (:inherit highlight))))
   `(company-tooltip-quick-access ((,class (:foreground ,glitch-magenta-fg))))
   `(company-tooltip-quick-access-selection ((,class (:inherit company-tooltip-quick-access))))
   `(company-tooltip-scrollbar-thumb ((,class (:background ,muted-fg))))
   `(company-tooltip-scrollbar-track ((,class (:background ,active-bg))))
   `(company-tooltip-search ((,class (:inherit highlight))))
   `(company-tooltip-search-selection ((,class (:inherit company-echo-common))))
   `(company-tooltip-selection ((,class (:inherit company-echo-common))))

   ;; == diff-hl ==
   ;; TODO diff fixes
   ;; TODO lsp ui peek
   `(diff-added ((,class (:background ,glitch-green-bg :foreground ,glitch-green-fg))))
   `(diff-removed ((,class (:background ,glitch-red-bg :foreground ,glitch-red-fg))))
   `(diff-context ((,class (:background ,dark-bg :foreground ,active-fg))))

   `(lsp-ui-peek-peek ((,class (:background "#1a1b26"))))
   `(lsp-ui-peek-list ((,class (:background "#1a1b26"))))
   `(lsp-ui-peek-highlight ((,class (:background ,glitch-green-bg :foreground ,glitch-green-fg))))
   `(lsp-ui-peek-selection ((,class (:background ,active-bg :foreground ,active-fg))))
   `(lsp-ui-peek-header ((,class (:background ,glitch-blue-bg-hl :foreground ,glitch-blue-fg-hl))))
   `(lsp-ui-peek-footer ((,class (:background ,glitch-blue-bg-hl :foreground ,glitch-blue-fg-hl))))
   `(lsp-ui-sideline-symbol ((,class (:foreground ,glitch-blue-fg))))
   `(lsp-ui-sideline-current-symbol ((,class (:foreground ,glitch-blue-fg :weight bold))))
   `(lsp-ui-sideline-code-action ((,class (:foreground ,glitch-yellow-fg :weight bold))))
   ;; `(lsp-ui-peek-list ((,class (:background))))

   `(diff-hl-change ((,class (:background ,warning-bg-hl :foreground ,warning-bg-hl))))
   `(diff-hl-delete ((,class (:background ,error-bg-hl :foreground ,error-bg-hl))))
   `(diff-hl-insert ((,class (:background ,info-bg-hl :foreground ,info-bg-hl))))
   `(diff-hl-reverted-hunk-highlight ((,class (:inverse-video t))))

   `(diff-hl-margin-change ((,class (:inherit diff-hl-change))))
   `(diff-hl-margin-delete ((,class (:inherit diff-hl-delete))))
   `(diff-hl-margin-insert ((,class (:inherit diff-hl-insert))))
   `(diff-hl-margin-ignored ((,class (:inverse-video t))))
   `(diff-hl-margin-unknown ((,class (:inverse-video t))))

   `(diff-hl-dired-change ((,class (:inherit diff-hl-change))))
   `(diff-hl-dired-delete ((,class (:inherit diff-hl-delete))))
   `(diff-hl-dired-insert ((,class (:inherit diff-hl-insert))))
   `(diff-hl-dired-ignored ((,class (:inverse-video t))))
   `(diff-hl-dired-unknown ((,class (:inverse-video t))))

   ;; == dirvish ==

   `(dirvish-hl-line ((,class (:background ,active-bg :foreground ,active-fg))))

   ;; == git-timemachine ==

   `(git-timemachine-commit ((,class (:foreground ,glitch-blue-fg))))
   `(git-timemachine-minibuffer-author-face ((,class (:foreground ,glitch-magenta-fg))))
   `(git-timemachine-minibuffer-detail-face ((,class (:foreground ,info-fg))))

   ;; == hl-todo ==

   `(hl-todo ((,class (:foreground ,glitch-yellow-fg :weight bold))))

   ;; = magit =

   `(diff-refine-added ((,class (:background ,info-bg :foreground ,info-fg))))
   `(diff-refine-removed ((,class (:background ,error-bg :foreground ,error-fg))))
   `(magit-bisect-bad ((,class (:background ,error-bg :foreground ,error-fg))))
   `(magit-bisect-good ((,class (:background ,info-bg :foreground ,info-fg))))
   `(magit-bisect-skip ((,class (:background ,warning-bg :foreground, warning-fg))))
   `(magit-blame-date ((,class (:inherit default))))
   `(magit-blame-dimmed ((,class (:inherit shadow))))
   `(magit-blame-hash ((,class (:inherit default))))
   `(magit-branch-local ((,class (:foreground ,glitch-blue-fg))))
   `(magit-branch-remote ((,class (:foreground ,glitch-magenta-fg))))
   `(magit-diff-added ((,class (:inherit magit-diff-context))))
   `(magit-diff-added-highlight ((,class (:background ,info-bg :foreground ,info-fg))))
   `(magit-diff-context ((,class (:inherit shadow :background ,bg))))
   `(magit-diff-context-highlight ((,class (:background ,active-bg :foreground ,fg))))
   `(magit-diff-file-heading ((,class (:inherit shadow :background ,bg))))
   `(magit-diff-file-heading-highlight ((,class (:background ,active-bg :foreground ,active-fg))))
   `(magit-diff-hunk-heading ((,class (:inherit shadow :background ,bg))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,active-bg :foreground ,active-fg))))
   `(magit-diff-hunk-region ((,class (:background ,warning-bg :foreground ,warning-fg))))
   `(magit-diff-lines-heading ((,class (:inherit shadow :background ,bg))))
   `(magit-diff-removed ((,class (:inherit magit-diff-context))))
   `(magit-diff-removed-highlight ((,class (:background ,error-bg :foreground ,error-fg))))
   `(magit-diff-whitespace-warning ((,class ())))
   `(magit-item-highlight ((,class (:inherit highlight))))
   `(magit-section-heading ((,class (:foreground ,glitch-blue-fg))))
   `(magit-section-highlight ((,class (:background ,active-bg :foreground ,active-fg))))

   ;;; = marginalia =

   `(marginalia-archive ((,class (:foreground ,special))))
   `(marginalia-char ((,class (:foreground ,glitch-magenta-fg))))
   `(marginalia-date ((,class (:foreground ,glitch-blue-fg))))
   `(marginalia-documentation ((,class (:inherit comment))))
   `(marginalia-file-name-face ((,class (:inherit comment))))
   `(marginalia-file-owner ((,class (:foreground ,glitch-blue-fg))))
   `(marginalia-file-priv-dir ((,class (:foreground ,glitch-yellow-fg))))
   `(marginalia-file-priv-exec ((,class (:foreground ,glitch-red-fg))))
   `(marginalia-file-priv-link ((,class (:foreground ,glitch-yellow-fg))))
   `(marginalia-file-priv-no ((,class (:inherit comment))))
   `(marginalia-file-priv-other ((,class (:foreground ,glitch-blue-fg))))
   `(marginalia-file-priv-rare ((,class (:foreground ,special))))
   `(marginalia-file-priv-read ((,class (:foreground ,glitch-green-fg))))
   `(marginalia-file-priv-write ((,class (:foreground ,glitch-red-fg))))
   `(marginalia-function ((,class (:inherit font-lock-function-name-face))))
   `(marginalia-installed ((,class (:foreground ,glitch-magenta-fg))))
   `(marginalia-key ((,class (:foreground ,glitch-magenta-fg))))
   `(marginalia-lighter ((,class (:foreground ,safe))))
   `(marginalia-list ((,class (:foreground ,safe))))
   `(marginalia-mode ((,class (:foreground ,qualifier))))
   `(marginalia-modified ((,class (:inherit warning))))
   `(marginalia-null ((,class (:inherit comment))))
   `(marginalia-number ((,class (:foreground ,safe))))
   `(marginalia-off ((,class (:foreground ,glitch-red-fg))))
   `(marginalia-on ((,class (:foreground ,glitch-green-fg))))
   `(marginalia-size ((,class (:foreground ,safe))))
   `(marginalia-string ((,class (:foreground ,safe))))
   `(marginalia-symbol ((,class (:inherit font-lock-keyword-face))))
   `(marginalia-true ((,class (:foreground ,glitch-blue-fg))))
   `(marginalia-value ((,class (:inherit font-lock-variable-use-face))))
   `(marginalia-version ((,class (:foreground ,qualifier))))
   `(marginalia-key ((,class (:foreground ,font-lock-variable-name-face))))
   `(marginalia-file-priv-dir ((,class (:foreground ,glitch-magenta-fg))))
   `(marginalia-file-priv-link ((,class (:foreground ,glitch-magenta-fg))))

   ;;; = pulsar =

   ;; `(pulsar-generic ((,class (:background "#171a27" :foreground ,info-fg))))
   `(pulsar-generic ((,class (:background "#2f354b"))))
   `(pulsar-blue ((,class (:background ,glitch-blue-bg :foreground ,glitch-blue-fg))))
   `(pulsar-cyan ((,class (:background ,glitch-cyan-bg :foreground ,glitch-cyan-fg))))
   `(pulsar-green ((,class (:background ,glitch-green-bg :foreground ,glitch-green-fg))))
   `(pulsar-magenta ((,class (:background ,glitch-magenta-bg :foreground ,glitch-magenta-fg))))
   `(pulsar-red ((,class (:background ,glitch-red-bg :foreground ,glitch-red-fg))))
   `(pulsar-yellow ((,class (:background ,glitch-yellow-bg :foreground ,glitch-yellow-fg))))

   ;;; = tree-sitter =

   `(tree-sitter-hl-face:comment ((,class (:inherit font-lock-comment-face))))
   `(tree-sitter-hl-face:constant ((,class (:inherit font-lock-constant-face))))
   `(tree-sitter-hl-face:constant.builtin ((,class (:inherit font-lock-constant-face))))
   `(tree-sitter-hl-face:constructor ((,class (:inherit font-lock-function-call-face))))
   `(tree-sitter-hl-face:doc ((,class (:inherit font-lock-doc-face))))
   `(tree-sitter-hl-face:function ((,class (:inherit font-lock-function-name-face))))
   `(tree-sitter-hl-face:function.builtin ((,class (:inherit font-lock-builtin-face))))
   `(tree-sitter-hl-face:function.call ((,class (:inherit font-lock-function-call-face))))
   `(tree-sitter-hl-face:function.macro ((,class (:inherit font-lock-function-call-face))))
   `(tree-sitter-hl-face:function.special ((,class (:inherit font-lock-function-call-face))))
   `(tree-sitter-hl-face:keyword ((,class (:inherit font-lock-keyword-face))))
   `(tree-sitter-hl-face:method ((,class (:inherit font-lock-function-call-face))))
   `(tree-sitter-hl-face:method.call ((,class (:inherit font-lock-function-call-face))))
   `(tree-sitter-hl-face:number ((,class (:inherit font-lock-number-face))))
   `(tree-sitter-hl-face:operator ((,class (:inherit font-lock-operator-face))))
   `(tree-sitter-hl-face:property ((,class (:inherit font-lock-property-use-face))))
   `(tree-sitter-hl-face:property.definition ((,class (:inherit font-lock-property-name-face))))
   `(tree-sitter-hl-face:punctuation.special ((,class (:foreground ,special))))
   `(tree-sitter-hl-face:string ((,class (:inherit font-lock-string-face))))
   `(tree-sitter-hl-face:string.special ((,class (:inherit font-lock-string-face))))
   `(tree-sitter-hl-face:type ((,class (:inherit font-lock-type-face))))
   `(tree-sitter-hl-face:type.argument ((,class (:inherit font-lock-type-face))))
   `(tree-sitter-hl-face:type.builtin ((,class (:inherit font-lock-type-face))))
   `(tree-sitter-hl-face:type.parameter ((,class (:inherit font-lock-type-face))))
   `(tree-sitter-hl-face:type.super ((,class (:inherit font-lock-type-face))))
   `(tree-sitter-hl-face:variable ((,class (:inherit font-lock-variable-name-face))))
   `(tree-sitter-hl-face:variable.builtin ((,class (:inherit font-lock-variable-name-face))))
   `(tree-sitter-hl-face:variable.parameter ((,class (:inherit font-lock-variable-name-face))))
   `(tree-sitter-hl-face:variable.special ((,class (:inherit font-lock-variable-name-face))))

   ;; == vertico ==

   `(vertico-current ((,class (:inherit region))))
   `(vertico-group-separator ((,class (:inherit comment))))
   `(vertico-group-title ((,class (:foreground ,glitch-blue-fg))))
   `(vertico-multiline ((,class (:foreground ,special))))

   ;; == volatile-highlights ==

   `(vhl/default-face ((,class (:inherit secondary-selection))))

   ;; == bm ==

   `(bm-face ((,class (:background ,special :foreground ,dark-bg))))
   `(bm-persistent-face ((,class (:background ,glitch-blue-bg-hl :foreground ,glitch-blue-fg-hl))))

   ;; == which-key ==

   `(which-key-command-description-face ((,class (:inherit font-lock-function-name-face))))
   `(which-key-docstring-face ((,class (:inherit comment))))
   `(which-key-group-description-face ((,class (:inherit font-lock-keyword-face))))
   `(which-key-highlighted-command-face ((,class (:inherit font-lock-function-call-face))))
   `(which-key-key-face ((,class (:inherit font-lock-variable-name-face))))
   `(which-key-local-map-description-face ((,class (:foreground ,glitch-green-fg))))
   `(which-key-note-face ((,class (:inherit comment))))
   `(which-key-separator-face ((,class (:inherit comment))))
   `(which-key-special-key-face ((,class (:foreground ,special))))

   ;; == solaire ==
   ;; TODO: finish this
   `(solaire-default-face ((,class (:background ,bg))))
   ;; `(solaire-default-face ((,class (:background "#1f2335"))))

   ;; `(solaire-default-face ((,class (:background "#1c2030"))))
   ;; solaire-fringe-face	fringe
   ;; solaire-line-number-face	line-number
   ;; solaire-hl-line-face	hl-line
   ;; solaire-org-hide-face	org-hide, org-indent
   ;; solaire-mode-line-face	mode-line
   ;; solaire-mode-line-inactive-face	mode-line-inactive
   ;; solaire-header-line-face	header-line

   `(doom-modeline-persp-buffer-not-in-persp ((,class (:foreground ,special :weight bold))))
   `(doom-modeline-persp-name ((,class (:foreground ,glitch-cyan-fg :weight bold))))
   `(doom-modeline-buffer-file ((,class (:foreground ,fg))))
   `(doom-modeline-project-dir ((,class (:foreground ,nord-cyan :weight bold))))

   `(treemacs-nerd-icons-file-face ((,class (:foreground ,glitch-blue-fg))))
   `(treemacs-nerd-icons-root-face ((,class (:foreground ,glitch-blue-fg))))
   `(treemacs-window-background-face ((,class :background "#1f2335")))

   `(persp-selected-face ((,class (:foreground ,special))))

   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nordic-vein)
;;; nordic-vein-theme.el ends here
