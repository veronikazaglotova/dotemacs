(setq straight-use-package-by-default t)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq use-package-always-demand t)
(setq custom-file (concat user-emacs-directory "customize.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
