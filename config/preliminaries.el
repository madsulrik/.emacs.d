;;; Preliminary emacs settings

(setq lexical-binding t)
(setq gc-cons-threshold 100000000)

(setq use-package-always-ensure t)


;; Defautl font settings
(defvar mus/default-font-size 110)
(defvar mus/default-variable-font-size 160)

;; Make frame transparency overridable
(defvar mus/frame-transparency '(90 . 90))

;; Keeping folders clean
(use-package no-littering)
;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;; Fixing the defaults

;; I don't like scrolling with the mouse and arrow keys
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))

(setq
 ;; No need to see GNU agitprop.
 inhibit-startup-screen t
 ;; No need to remind me what a scratch buffer is.
 initial-scratch-message nil
 ;; Double-spaces after periods is morally wrong.
 sentence-end-double-space nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; Prompts should go in the minibuffer, not in a GUI.
 use-dialog-box nil
 ;; Fix undo in commands affecting the mark.
 mark-even-if-inactive nil
 ;; Let C-k delete the whole line.
 kill-whole-line t
 ;; search should be case-sensitive by default
 case-fold-search nil
 ;; no need to prompt for the read command _every_ time
 compilation-read-command nil
 ;; always scroll
 compilation-scroll-output t)

;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes
;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)
(defalias 'yes-or-no-p 'y-or-n-p)       ; Accept 'y' in lieu of 'yes'.

;; UTF8 by default
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))


(delete-selection-mode t)
(global-display-line-numbers-mode t)
(column-number-mode)


(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
;(set-face-attribute 'hl-line nil :background "gray21")

;; Backups
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(setq custom-file null-device)
(setq custom-safe-themes t)

(require 'recentf)
(add-to-list 'recentf-exclude "\\elpa")

(if (version< "27.0" emacs-version)     ; )
    (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
  (warn "This Emacs version is too old to properly support emoji."))

;; Unbind unuseefull bindings
(unbind-key "C-x C-f") ;; find-file-read-only
(unbind-key "C-x C-d") ;; list-directory
(unbind-key "C-z") ;; suspend-frame
(unbind-key "M-o") ;; facemenu-mode
(unbind-key "<mouse-2>") ;; pasting with mouse-wheel click
(unbind-key "<C-wheel-down>") ;; text scale adjust
(unbind-key "<C-wheel-up>") ;; ditto
(unbind-key "s-n") ;; make-frame

;; The out-of-the-box treatment of whitespace is unfortunate, but fixable.
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

;; remove emacs news
(defalias 'view-emacs-news 'ignore)
(defalias 'describe-gnu-project 'ignore)

;; Fix undo in emacs
(use-package undo-tree
  :diminish
  :bind (("C-c _" . undo-tree-visualize))
  :config
  (global-undo-tree-mode 1)
  (unbind-key "M-_" undo-tree-map))


;; I define a couple of my own configuration variables with ~defvar~,
;; and no matter how many times I mark the variable as safe,
;; it warns me every time I set it in the ~.dir-locals~ file.
;; Disabling these warnings is probably (?) the right thing to do.
(setq enable-local-variables :all)


;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
