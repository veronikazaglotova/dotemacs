`';;; init.el --- my Emacs configuration file
;; -*- lexical-binding: t; -*-
;; C-x C-e this function to make everything separated by 2 newline:
;; (replace-regexp "^\n+" "\n\n")


;;; Commentary:
;; I like Emacs


;;; Code:


(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-center-content t)
  (dashboard-banner-logo-title "Welcum to GNU Emacs"))


(use-package use-package-hydra
  :straight t hydra)         ;I don't know how to use this
                                        ;but let it be
(use-package diminish)			;I NEED THIS




(use-package elpy
  :config
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  (elpy-enable)

  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
  (add-hook 'elpy-mode-hook #'flycheck-mode))


(use-package counsel
  :straight t ivy-rich ivy-prescient pcre2el
  :config
  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  (ivy-prescient-mode)
  (ivy-rich-mode)
  (prescient-persist-mode)
  (counsel-mode)
  :bind
  ("M-x" . counsel-M-x)
  ((:prefix-map ctl-comma-map
               :prefix "C-,"
               ("m" . counsel-mark-ring)
               ("y" . counsel-yank-pop)))
  ((:map ivy-minibuffer-map
	          ("C-r" . minibuffer-history)
	          ("TAB" . ivy-alt-done)))
  ((:map ctl-x-map
         ("k" . kill-current-buffer)
         ("C-f" . counsel-find-file)
         ("b" . counsel-switch-buffer)))
  ((:map help-map
        ("f" . counsel-describe-function)
        ("v" . counsel-describe-variable)
        ("o" . counsel-describe-symbol)
        ("l" . counsel-find-library)))
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  :diminish
  counsel-mode
  ivy-rich-mode
  ivy-prescient-mode)


(use-package swiper
  :bind
  ("C-s" . swiper))



(use-package avy
  :custom
  (avy-keys '(?e ?t ?h ?u ?o ?n) "Use home row with avy. All keys but pinkies and middle column on Dvorak.")
  (avy-timeout-seconds 0.7 "Avy is too fast and I'm a boomer.")
  :bind
  ("C-r" . weeb/avy-goto-char-timer-end)
  ("M-r" . avy-goto-char-timer)
  :config
  (defun weeb/avy-goto-char-timer-end (&optional arg)
    "Read one or many consecutive chars and jump to the last one. With
prefix ARG go to the first character instead."
    (interactive "P")
    (if (not arg)
	    (unless (eq (avy-goto-char-timer) t)
          (forward-char (length avy-text)))
      (avy-goto-char-timer))))


(use-package boon
  :custom
  (hi-lock-auto-select-face t))
;; for future

(use-package with-editor
  :hook
  ((eshell-mode term-exec shell-mode) . with-editor-export-editor))


(use-package eshell
  )


(use-package fix-input
  :config
  (fix-input "english-dvorak"   ;; matches alternative layout
             "russian-computer" ;; works with QWERTY
             "dvorak-russian")
  (set-input-method "dvorak-russian" t)
  (toggle-input-method))
    ;; name of new input method that
  ;; preserves
  ;; the same layout with Dvorak


(use-package sly)


(use-package magit
  :straight t forge
  )


(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :diminish
  undo-tree-mode)


(use-package eldoc
  :diminish eldoc-mode)


(use-package company
  :straight t yasnippet-snippets
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.1)
  (company-tooltip-align-annotations t)
  (company-dabbrev-other-buffers nil)
  (company-dabbrev-downcase 0)
  (company--show-numbers t)
  :config
  (global-company-mode)
  (yas-global-mode)
  :diminish
  company-mode
  yas-global-mode
  yas-minor-mode)


(use-package telephone-line
  :config
  (telephone-line-mode))


(use-package flycheck
  :config
  (global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
  :diminish
  flycheck-mode)


(use-package page-break-lines
  :config
  (global-page-break-lines-mode)
  :custom
  (line-move-visual nil)
  :diminish
  (page-break-lines-mode visual-line-mode))


(use-package emacs                      ; fonts and stuff
  :config
  (set-frame-font "Iosevka 12" nil t)
  (prefer-coding-system 'utf-8))


(use-package async
  :config
  (dired-async-mode)
  (async-bytecomp-package-mode))


(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  :hook
  (prog-mode . highlight-indent-guides-mode))


(use-package haskell-mode)


(use-package term
  :bind
  ("M-<return>" . ansi-term))




(use-package smartparens
  :config
  (smartparens-global-strict-mode)
  (show-smartparens-global-mode)
  :diminish
  smartparens-strict-mode
  smartparens-mode
  :hydra
  (sp-hydra (global-map "M-s")
            ("a" sp-beginning-of-sexp "Beginning")
            ("e" sp-end-of-sexp "End")
            ("b" sp-previous-sexp "Backward")
            ("f" sp-next-sexp "Forward")
            ("s" sp-unwrap-sexp "Stripf")
            ("M-s" sp-backward-unwrap-sexp "Stripb")
            ("r f" sp-forward-slurp-sexp ")>")
            ("r b" sp-forward-barf-sexp ")<")
            ("l f" sp-backward-slurp-sexp "(>")
            ("l b" sp-backward-barf-sexp "(<")
            ("k" sp-kill-sexp "Kill")
            ("")
            ("." hydra-repeat "Repeat")))


(use-package base16-theme
  :config
  (load-theme 'base16-tomorrow t))


(use-package which-key
  :config
 (which-key-mode))


(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode)
  :custom
  (display-line-numbers-width 4)
  (display-line-numbers-width-start t)
  (display-line-numbers-grow-only t))


(use-package emacs                      ;fill-column
  :config
  (global-display-fill-column-indicator-mode)
  :custom
  (auto-fill-function 'do-auto-fill)
  (auto-fill-column 79)
  :hook
  (dashboard-mode . (lambda () (display-fill-column-indicator-mode 0))))


(use-package emacs                 ;make it behave like normal editors
  :config
  (global-display-line-numbers-mode)
  (delete-selection-mode)
  (global-prettify-symbols-mode)
  (savehist-mode)
  (prefer-coding-system 'utf-8)
  (delete-selection-mode)
  ;; (desktop-save-mode)			;save session between restarts
  :hook
  (before-save . delete-trailing-whitespace)
  :custom
  (disabled-command-function nil) ;turn off warnings on scary commands


  ;; modeline stuff
  (column-number-mode nil)
  (line-number-mode nil)
  (mode-line-position nil)
  (mode-line-percent-position nil)
  (mode-line-in-non-selected-windows nil)


  (scroll-step 1)
  (scroll-margin 1)
  (scroll-conservatively 100)
  (select-enable-clipboard t)	       ;copy paste between X and Emacs
  (frame-title-format '(:eval
			            (let
			                ((match (string-match "[ *]"
						                          (buffer-name))))
                          (if (and match (= match 0)) "Emacs"
			                "%b — Emacs"))))
  :bind
  ("C-=" . text-scale-increase)
  ("C--" . text-scale-decrease))


(use-package emacs                      ; tabs to spaces
  :custom
  (tab-width 4)
  (standard-indent 4)
  (c-basic-offset tab-width)
  (indent-tabs-mode nil))


(use-package emacs                      ; custom shit
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)	;y or n instead of yes or no


  ;; bury scratch buffer instead of killing it
  (defadvice kill-buffer (around kill-buffer-around-advice activate)
    (let ((buffer-to-kill (ad-get-arg 0)))
      (if (equal buffer-to-kill "*scratch*")
          (bury-buffer)
        ad-do-it))))


(use-package emacs                  ; editing stuff, default shortcuts
  :bind
  ("C-x M-l" . weeb/load-init-file)
  ("C-e" . weeb/end-of-syntax)
  ("C-a" . weeb/back-to-indentation-dwim)
  ("RET" . newline-and-indent)
  ("M-a" . delete-indentation)
  ("C-y" . weeb/kill-or-yank-dwim)
  ("C-o" . weeb/er-smart-open-line)
  ("M-o" . weeb/er-smart-Open-line)
  ("C-;" . comment-line)
  (:map ctl-x-map
	    ("M-s" . weeb/switch-to-scratch-buffer))
  :init
  (defun weeb/load-init-file ()
    "Load the init file"
    (interactive)
    (load-file user-init-file))


  (defun weeb/end-of-syntax ()
    "Move to the end of code (e.g. everything
that isn't comments or spaces/tabs)
When pressed again, this will go to the end of line."
    (interactive)
    (if (not (equal last-command 'weeb/end-of-syntax))
	    (progn (skip-syntax-forward "^<" (line-end-position)) ; test
	           (skip-syntax-backward " " (line-beginning-position)))
      (end-of-line)))


  (defun weeb/kill-or-yank-dwim (&optional arg)
    "Kills region if you marked anything, yanks if you didn't"
    (interactive)
    (cond ((use-region-p)
	       (call-interactively 'kill-ring-save))
	      ((eq last-command 'yank)
	       (call-interactively 'yank-pop))
	      (t
	       (call-interactively 'yank))))


  (defun weeb/switch-to-scratch-buffer ()
    "Switches to scratch buffer, switches back if called again"
    (interactive)
    (if (equal (current-buffer) (get-buffer "*scratch*"))
	    (previous-buffer)
      (switch-to-buffer "*scratch*")))
  (defun weeb/back-to-indentation-dwim ()
    "Go back to the first non-whitespace character. When pressed second time, go to the beginning of the line.
This function alternates between first non-whitespace and beginning of the line."
    (interactive)
    (if (equal last-command 'weeb/back-to-indentation-dwim)
	    (beginning-of-line)
      (back-to-indentation)))


  (defun weeb/er-smart-Open-line ()
    "Insert an empty line before the current line.
Position the cursor at its beginning, according to the current mode"
    (interactive)
    (move-beginning-of-line nil)
    (newline-and-indent)
    (previous-line))


  (defun weeb/er-smart-open-line ()
    "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
    (interactive)
    (move-end-of-line nil)
    (newline-and-indent)))


(use-package frames-only-mode
  :config
  (frames-only-mode)
  :bind
  ("C-x 1" . delete-other-frames)
  ("C-x 2" . make-frame-command))


(use-package projectile
  :config
  (projectile-mode))


(provide 'init)


;;; init.el ends here
