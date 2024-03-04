;;; init-lang-c.el --- c.
;;; Commentary:
;;; Code:

(use-package cc-mode
  :straight
  (:type built-in)
  :config
  (setq c-basic-offset 4)
  :bind
  (:map c-mode-map
        ("C-c f p" . ff-find-other-file)))

(use-package reformatter
  :config
  (reformatter-define clang-reformatter
    :program "clang-format"
    :args '("-\
style={\
BasedOnStyle: Google, \
IndentWidth: 4, \
BreakBeforeBraces: Stroustrup, \
AlwaysBreakAfterReturnType: AllDefinitions, \
ColumnLimit: 120\
}"))
  (add-hook 'c-mode-hook 'clang-reformatter-on-save-mode))


(provide 'init-lang-c)
;;; init-lang-c.el ends here
