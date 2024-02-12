;;; early-init.el --- User config before gui & package system initialization.
;;; Commentary:
;;; Code:

(setq gc-cons-threshold most-positive-fixnum  ; minimize gcs to start emacs faster
      inhibit-splash-screen t                 ; open to the *scratch* buffer
      package-enable-at-startup nil)          ; disable the default package system, package.el

;;
;; Initialize package management. straight.el makes it easy to hack on
;; packages because it clones their git repos locally.
;;

(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)

(provide 'early-init)
;;; early-init.el ends here
