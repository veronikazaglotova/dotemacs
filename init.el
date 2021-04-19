;;; init.el --- my Emacs configuration file
;; -*- lexical-binding: t; -*-
;; C-x C-e this function to make everything separated by 2 newline:
;; (replace-regexp "^\n+" "\n\n")


;;; Commentary:
;; I like Emacs


;;; Code:


(use-package general)


(use-package use-package-hydra
  :straight t hydra)         ;I don't know how to use this
                                        ;but let it be


(use-package diminish)			;I NEED THIS


(use-package elpy
  :custom
  (python-shell-interpreter "jupyter")
  (python-shell-interpreter-args "console --simple-prompt")
  (python-shell-prompt-detect-failure-warning nil)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)
        elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  (elpy-enable)
  :hook
  (elpy-mode . flycheck-mode))


(use-package selectrum-prescient
  :config
  (selectrum-mode +1)
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))


(use-package marginalia
  :straight (marginalia :type git :host github
                        :repo "minad/marginalia" :branch "main")
  :config
  (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light)))


(use-package embark-consult
  :after (embark consult))


(use-package consult
  :bind
  ("M-y" . consult-yank-pop)
  ("C-s" . consult-line)
  (:prefix "C-," :prefix-map ctl-comma-map
        ("r" . consult-ripgrep)
        ("b" . consult-buffer))
  :custom
  (consult-line-point-placement 'match-end)) ; sadly point-placement doesn't work with Selectrum


(use-package avy
  :custom
  (avy-keys '(?e ?t ?h ?u ?o ?n) "Use me row with avy. All keys but pinkies and middle column on Dvorak.")
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


;; (use-package evil
;;   :straight t undo-fu
;;   :custom
;;   (evil-want-C-d-scroll nil)
;;   (evil-want-C-w-delete nil)
;;   (evil-want-Y-yank-to-eol t)
;;   (evil-shift-width tab-width)
;;   (evil-move-cursor-back nil)
;;   (evil-move-beyond-eol t)
;;   (evil-cross-lines t)
;;   (evil-undo-system 'undo-fu)
;;   :config
;;   (evil-mode))


(use-package highlight-parentheses
  :config
  (highlight-parentheses-mode))


(use-package eshell
  )


(use-package fix-input
  :config
  (fix-input "english-dvorak"   ; matches alternative layout
             "russian-typewriter" ; so that it works with Dvorak
             "dvorak-russian")
  (set-input-method "dvorak-russian" t)
  (toggle-input-method))


(use-package sly
  :custom
  (inferior-lisp-program "sbcl"))


(use-package magit
  :straight t forge)


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


(use-package flycheck
  :config
  (global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
  :diminish
  flycheck-mode)


(use-package emacs
  :config
  (global-visual-line-mode)
  :custom
  (line-move-visual nil)
  :diminish
  visual-line-mode)


(use-package emacs                      ; fonts and stuff
  :config
  (set-face-attribute 'default nil :height 150 :family "Iosevka")
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
  (prog-mode . highlight-indent-guides-mode)
  :diminish
  highlight-indent-guides-mode)


(use-package haskell-mode)


(use-package term
  :bind
  ("M-<return>" . ansi-term))


(use-package smartparens
  :straight (smartparens :flavor nil :type git :host github
                         :repo "Fuco1/smartparens"
                         :branch master)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)
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
            ("." hydra-repeat "Repeat")
            ("c" weeb/close-all-parentheses "Close all parents"))
  :init
  (defun weeb/close-all-parentheses ()
  (interactive "*")
  (let ((closing nil))
    (save-excursion
      (while (condition-case nil
         (progn
           (backward-up-list)
           (let ((syntax (syntax-after (point))))
             (cl-case (car syntax)
               ((4) (setq closing (cons (cdr syntax) closing)))
               ((7 8) (setq closing (cons (char-after (point)) closing)))))
           t)
           ((scan-error) nil))))
    (apply #'insert (nreverse closing)))))


(use-package modus-operandi-theme
  :config
  (load-theme 'modus-operandi t))


(use-package which-key
  :config
  (which-key-mode)
  :diminish
  which-key-mode)


(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode)
  :custom
  (display-line-numbers-width 4)
  (display-line-numbers-width-start t)
  (display-line-numbers-grow-only t))


(use-package display-fill-column-indicator                      ;fill-column
  :config
  (global-display-fill-column-indicator-mode)
  :custom
  (fill-column 79)
  (auto-fill-function #'do-auto-fill)
  (display-fill-column-indicator-character 9474)
  :hook
  (dashboard-mode . (lambda () (display-fill-column-indicator-mode 0)))
  :diminish
  auto-fill-function)


(use-package emacs                 ;make it behave like normal editors
  :config
  (defalias 'yes-or-no-p #'y-or-n-p)
  (global-display-line-numbers-mode)
  (delete-selection-mode)
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
  :diminish
  auto-revert-mode
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
  :config)	;y or n instead of yes or no


(use-package mwim
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))


(use-package emacs                  ; editing stuff, default shortcuts
  :bind
  (:map ctl-x-map ("M-l" . weeb/load-init-file))
  (:map global-map
  ("RET" . newline-and-indent)
  ("M-a" . delete-indentation)
  ("C-y" . weeb/kill-or-yank-dwim)
  ("C-o" . weeb/er-smart-open-line)
  ("M-o" . weeb/er-smart-Open-line)
  ("C-;" . comment-line)
  ("M-k" . kill-whole-line))
  (:map ctl-x-map
	    ("M-s" . weeb/switch-to-scratch-buffer))
  (:map emacs-lisp-mode-map
        ("#" . endless/sharp))
  :init


  (defun weeb/load-init-file ()
    "Load the init file"
    (interactive)
    (load-file user-init-file))


  (defun weeb/highlight-loc ()
    (interactive)
    (cl-flet
        ((end-of-code ()
                      (when (comment-search-forward (line-end-position) t)
                        (goto-char (match-beginning 0))
                        (skip-syntax-backward " "
                                              (line-beginning-position)))))


      (back-to-indentation)
      (save-excursion (set-mark (progn (end-of-code) (point))))))


  (defun weeb/kill-or-yank-dwim (&optional arg)
    "Kills region if you marked anything, yanks if you didn't"
    (interactive)
    (cond ((use-region-p)
	       (call-interactively #'kill-ring-save))
	      ((eq last-command #'yank)
	       (call-interactively #'yank-pop))
	      (t
	       (call-interactively #'yank))))


  (defun weeb/switch-to-scratch-buffer ()
    "Switches to scratch buffer, switches back if called again"
    (interactive)
    (if (equal (current-buffer) (get-buffer "*scratch*"))
	    (previous-buffer)
      (switch-to-buffer "*scratch*")))


  (defun weeb/er-smart-Open-line ()     ;code not mine
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
  (:map ctl-x-map
        ("1" . delete-other-frames)
        ("2" . make-frame-command)))


(use-package gcmh
  :config
  (gcmh-mode)
  :diminish
  gcmh-mode)


(use-package projectile
  :config
  (projectile-mode)
  :custom
  (projectile-mode-line-prefix " "))


(provide 'init)


;;; init.el ends here
