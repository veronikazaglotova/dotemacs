;; -*- lexical-binding: t; -*-
;; (replace-regexp "^\n+" "\n\n")
(setq straight-use-package-by-default t)
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
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


(setq custom-file (expand-file-name "~/.config/emacs/customize.el"))
(load custom-file)


(use-package reverse-im
  :custom
  (reverse-im-input-methods '("russian-typewriter"))
  :config
  (reverse-im-mode t))


(use-package async
  :config
  (dired-async-mode))


(use-package yasnippet
  :straight t yasnippet-snippets
  :init
  (yas-global-mode)
  :hook
  (yas-after-exit-snippet . indent-according-to-mode)
  :custom
  (yas-triggers-in-field t "Snippets inside snippets"))


(use-package ctrlf
  :init (ctrlf-mode +1))


(use-package company-auctex
  :straight t auctex company-math
  :init (company-auctex-init)
  :custom
  (TeX-auto-save t))


(use-package magit)


(use-package vterm
  :hook
  (vterm-mode . (lambda () (display-line-numbers-mode -1)))) ; turn the fuck off numbers in the terminal.


(use-package gcmh
  :init
  (gcmh-mode 1))


(use-package flycheck)


(add-hook 'prog-mode-hook  (lambda () (flymake-mode -1)))


(use-package consult-selectrum
  :bind
  ("C-x b" . consult-buffer)
  ("M-y" . consult-yank-pop)
  ("C-," . consult-line))


(use-package base16-theme
  :config
  (setq base16-highlight-mode-line t)
  (load-theme 'base16-google-light))


(defun ap/garbage-collect ()
  (interactive)
  (message (cl-loop for (type size used free) in (garbage-collect)
		    for used = (* used size)
		    for free = (* (or free 0) size)
		    for total = (file-size-human-readable (+ used free))
		    for used = (file-size-human-readable used)
		    for free = (file-size-human-readable free)
		    concat (format "%s: %s + %s = %s\n" type used free total))))


;; replaces Emacs' icomplete with selectrum
(use-package selectrum-prescient
  :init
  (selectrum-mode +1)
  :config
  ;; to make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)
  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1))
  


;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia" :branch "main")
  :config
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))


(use-package which-key
  :config
  (which-key-mode))


(use-package elpy
  :config
  (elpy-enable))


(use-package company
  :custom
  (company-minimum-prefix-length 2 "I don't like company constantly popping up. I can type any 2 letter combo myself.")
  (company-idle-delay 0.3 "Company too fast.")
  (company-tooltip-align-annotations t "Don't know what this does")
  (company-dabbrev-other-buffers nil "Turn off dabbrev for other buffers")
  (company-dabbrev-downcase 0)
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  :hook
  (after-init . global-company-mode))


(use-package org
  :defer t
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-startup-folded nil))


(use-package org-bullets
  :config
  (setq inhibit-compacting-font-caches t)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


(use-package rainbow-delimiters		; Sounds gay
  :hook
  (prog-mode . rainbow-delimiters-mode))


(use-package avy
  :custom
  (avy-keys '(?e ?t ?h ?u ?o ?n) "Avy to use home row, all keys but pinkies and middle column on Dvorak.")
  (avy-timeout-seconds 0.7 "Set my own avy timer timeout.")
  :bind
  ("C-'" . avy-goto-char-timer-end))


(use-package projectile)


(use-package emacs
  :init
  (async-bytecomp-package-mode 1)
  ;; making Emacs saner, removing ugly things, etc
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  (menu-bar-mode -1)
  (set-frame-font "Ubuntu Mono 13" nil t)
  (global-display-line-numbers-mode 1)
  (delete-selection-mode t) ; this makes emacs delete text if I highlight anything and start typing, bretty cool
  (show-paren-mode 1) ; show parents
  (desktop-save-mode 1)	; save current session and start it again on next startup
  (windmove-default-keybindings)	   ; keybindings for switching windows
  (lambda () ;; making more keys available
    (setq unmapped '("C-i" "C-[" "C-m"))
    (dolist (k unmapped)
      (define-key input-decode-map (kbd k) (kbd (concat "<"  k ">")))))
  (global-visual-line-mode t) ; this thing adds nice thingies.
  (setq show-paren-style 'mixed)
  :custom
  (comment-column 0 "Make comments appear right after the code instead of indenting it")
  (comment-fill-column 0)
  (help-window-select t "Autoselect windows")
  (disabled-command-function nil "Enable all disabled commands")
  (display-line-numbers-width-start t "Automatically change line numbers width")
  (fast-but-imprecise-scrolling t "Says that it should be fast if you hold down C-v or M-v on wiki, IDK")
  (scroll-step 1)
  (scroll-margin 1)
  (scroll-conservatively 100 "This line and two lines above make Emacs scroll like it would on other editors.")
  (mouse-autoselect-window t "Autoselect mouse on hover")
  (select-enable-clipboard t "After copying with Ctrl+c in X11, you can paste with C-y in emacs")  
  (help-window-select t "Automatic switch to help buffers")
  (frame-title-format '("%f [%m]"))
  
  :bind
  ("C-DEL" . backward-delete-word)	; do not copy the word when C-bspc
  ;; my custom hotkeys
  ("C-=" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("M-RET" . vterm)
  ("C-;" . comment-line)
  ("C-x M-l" . load-init-file)
  ("C-e" . end-of-syntax)
  ("C-a" . back-to-indentation-dwim)
  ("RET" . newline-and-indent)
  ("M-a" . delete-indentation)
  ("C-x e" . macro-or-region-macro)  
  ;; turning off annoying shit
  ("C-z" . nil)
;; things I miss from Vim
  ("M-o" . open-next-line) ; Open a new line below
  ("C-o" . open-previous-line) ; Open a new line above and indent
  ("M-k" . kill-whole-line)
  ("C-x 4" . toggle-window-split)
  ("M-s s" . switch-to-scratch-buffer))


;; bury *scratch* buffer instead of kill it
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))


;; y/n instead of yes/no everywhere
(defalias 'yes-or-no-p 'y-or-n-p)


(load-file (expand-file-name "menuprefix.el" user-emacs-directory))


;; There go the functions.


(defun nequal (i j)
  (not (equal i j)))


(defun load-init-file ()
  "Load the init file"
  (interactive)
  (load-file user-init-file))


(defun end-of-syntax ()
  "Move to the end of code (e.g. everything that isn't comments or spaces/tabs)
When pressed again, this will go to the end of line. This alternates between these two positions."
  (interactive)
  (if (not (equal last-command 'end-of-syntax))
      (progn (skip-syntax-forward "^<" (line-end-position)) ; test
	     (skip-syntax-backward " " (line-beginning-position)))
    (end-of-line)))


(defun toggle-window-split ()
  "Toggle between vertical and horizontal split"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))


(defun kill-or-yank-dwim (&optional arg)
  "Kills region if you marked anything, yanks if you didn't"
  (interactive)
  (cond ((use-region-p)
     (call-interactively 'kill-ring-save))
    ((eq last-command 'yank)
     (call-interactively 'yank-pop))
    (t
     (call-interactively 'yank))))


(makunbound 'viper-current-state)


(defun switch-to-scratch-buffer ()
    "Switches to scratch buffer, switches back if called again"
    (interactive)
    (if (equal (current-buffer) (get-buffer "*scratch*"))
	(previous-buffer)
      (switch-to-buffer "*scratch*")))


(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (when 'newline-and-indent
    (indent-according-to-mode)))


(defun avy-goto-char-timer-end (&optional arg)
  "Read one or many consecutive chars and jump to the last one.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (avy-goto-char-timer arg)
  (dotimes (i (length avy-text))
    (forward-char)))


(defun open-previous-line (arg)
  "Open a new line before the current one. 
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when 'newline-and-indent
    (indent-according-to-mode)))


(defvar back-to-indent-dwim-p nil)
(defun back-to-indentation-dwim ()
  "Go back to the first non-whitespace character. When pressed second time, go to the beginning of the line.
This function alternates between first non-whitespace and beginning of the line."
  (interactive)
  (if (and (equal back-to-indent-dwim-p t) (equal last-command 'back-to-indentation-dwim))
      (progn (setq back-to-indent-dwim-p nil) (beginning-of-line))
    (if (equal last-command 'back-to-indentation-dwim)
	(progn (setq back-to-indent-dwim-p t) (back-to-indentation))
      (progn (setq back-to-indent-dwim-p t) (back-to-indentation)))))


(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))


(defun select-in-quote ()
  "Select text between the nearest left and right delimiters.)


 the selected char is “c”, not “a(b)c”."
  (interactive)
  (let (
        ($skipChars "^'\"`<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）〘〙")
        $p1
        )
    (skip-chars-backward $skipChars)
    (setq $p1 (point))
    (skip-chars-forward $skipChars)
    (set-mark $p1)))


(defun get-selected-text ()
  (let ((start (region-beginning)) (end (region-end)))
      (buffer-substring start end)))


(defun open-in-xdg ()
  "Open selected thingy in xdg"
  (interactive)
  (let* ((file (get-selected-text)))
    (call-process "zathura" nil 0 nil file)))


(defun xdg-on-point ()
  "Open a video file the cursor is on"
  (interactive)
  (select-in-quote)
  (open-in-xdg))


(defun macro-or-region-macro ()
  "Runs a macro on all lines if you highlight anything, runs the macro on current line if nothing is highlighted."
  (interactive)
  (if (use-region-p)
      (apply-macro-to-region-lines (region-beginning) (region-end))
    (kmacro-end-and-call-macro 1)))


(defun setlatexmkbuffer ()
  "Sets current buffer as the buffer where latexmk runs. Use this on the terminal emulator."
  (interactive)
  (setq latexmkbuffer (current-buffer)))


(setq test 5)
(boundp 'test)


(defun latex-start-or-restart ()
  "This function starts LaTeX and yes."
  (interactive)
  (if (and (boundp 'latexmkbuffer) (get-buffer "vterm"))
      (restartlatexgen)
    (let ((usedbuffer (current-buffer))
	  (usedbuffername (buffer-name)))
      (vterm)
      (vterm-send-string (concat "latexmk -pvc -pdf " "'" usedbuffername "'"))
      ;; (execute-kbd-macro (read-kbd-macro "TAB"))
      (vterm-send-return)
      (setlatexmkbuffer)
      (switch-to-buffer usedbuffer))))
(defun restartlatexgen ()
  "Restarts latexmk if it ran in a problem. Use this after you fixed the problem"
  (interactive)
  (let ((usedbuffer (current-buffer)))
  (switch-to-buffer latexmkbuffer)
  (execute-kbd-macro (read-kbd-macro "C-d"))
  (switch-to-buffer usedbuffer))
  (save-buffer))


(defun latex-add-newlines ()
  "Adds \"\\\\\" at the end of line. With a prefix argument, runs the command for ARG lines."
  (interactive)
  (if (not (use-region-p))
      (progn (end-of-syntax)
	     (insert "\\\\"))
    (let ((undo-inhibit-record-point t))
      (goto-char (region-beginning))
      (while (< (line-number-at-pos) (line-number-at-pos (region-end)))
      (end-of-syntax)
      (insert "\\\\")
      (next-line)))) (point)
  (undo-boundary))
(let ((thing 5))
  (setq the 6))


