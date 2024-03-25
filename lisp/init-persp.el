;;; init-persp.el --- persp-mode.
;;; Commentary:
;;; Code:

(use-package persp-mode
  :hook
  (emacs-startup . persp-mode)
  :config
  (setq persp-add-buffer-on-find-file nil
        ;; Disable auto resume
        persp-auto-resume-time 0)
  ;; Automatic persps for project(.el)s.
  (defun user/maintain-persp-for-project (&rest _args)
    (interactive)
    (if (and persp-mode (project-current))
        (progn
          (persp-window-switch (project-root (project-current)))
          (persp-add-buffer (current-buffer)))
      (persp-window-switch "none")))
  (add-hook 'emacs-startup-hook #'user/maintain-persp-for-project)
  (add-hook 'find-file-hook #'user/maintain-persp-for-project)

  ;; (add-hook 'switch-to-buffer #'user/maintain-persp-for-project)
  ;; (add-hook 'persp-switch-to-buffer #'user/maintain-persp-for-project)
  ;; (add-hook 'project-switch-to-buffer #'user/maintain-persp-for-project)
  ;; (add-hook 'ace-window #'user/maintain-persp-for-project)
  ;; (advice-add 'ace-window :after #'user/maintain-persp-for-project)
  ;; (add-hook 'buffer-list-update-hook #'user/maintain-persp-for-project)
  :bind
  (:map global-map
        ("C-x b" . persp-switch-to-buffer))
  (:map persp-key-map
        ("s" . persp-window-switch)
        ("S" . user/maintain-persp-for-project))
  (:map user-map
        ("p" . persp-key-map)
        ("P" . (lambda () (interactive)(persp-load-state-from-file (concat persp-save-dir persp-auto-save-fname))))))

(provide 'init-persp)
;;; init-persp.el ends here
