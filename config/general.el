(setq lexical-binding t)

(setq gc-cons-threshold 100000000)

(setq use-package-always-ensure t)

;; You will most likely need to adjust this font size for your system!
(defvar mus/default-font-size 110)
(defvar mus/default-variable-font-size 160)

;; Make frame transparency overridable
(defvar mus/frame-transparency '(90 . 90))

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

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

(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(setq custom-file null-device)

(setq custom-safe-themes t)

(require 'recentf)
(add-to-list 'recentf-exclude "\\elpa")

(if ( version< "27.0" emacs-version ) ; )
    (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
  (warn "This Emacs version is too old to properly support emoji."))

(unbind-key "C-x C-f") ;; find-file-read-only
(unbind-key "C-x C-d") ;; list-directory
(unbind-key "C-z") ;; suspend-frame
(unbind-key "M-o") ;; facemenu-mode
(unbind-key "<mouse-2>") ;; pasting with mouse-wheel click
(unbind-key "<C-wheel-down>") ;; text scale adjust
(unbind-key "<C-wheel-up>") ;; ditto
(unbind-key "s-n") ;; make-frame

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

(defalias 'view-emacs-news 'ignore)
(defalias 'describe-gnu-project 'ignore)

(use-package undo-tree
  :diminish
  :bind (("C-c _" . undo-tree-visualize))
  :config
  (global-undo-tree-mode +1)
  (unbind-key "M-_" undo-tree-map))

(setq enable-local-variables :all)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :after evil
  :config
  (general-create-definer mus/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix ","
    :global-prefix "C-,")

  (mus/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "ecf" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/config/general.org")))))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package exec-path-from-shell)

(when (memq window-system '(mac ns x))
 (exec-path-from-shell-initialize))

(set-face-attribute 'default nil :font "Fira Code" :height mus/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height mus/default-font-size)

;; Set the variable pitch face
;; (set-face-attribute 'variable-pitch nil :font "Cantarell" :height mus/default-variable-font-size :weight 'regular)
(set-face-attribute 'variable-pitch nil :font "ETBembo" :height mus/default-variable-font-size :weight 'thin)


(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(when (window-system)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(use-package doom-themes
  :config
  (let ((chosen-theme 'doom-dracula))
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)
    (setq doom-challenger-deep-brighter-comments t
          doom-challenger-deep-brighter-modeline t
          doom-dark+-blue-modeline nil)
    (load-theme chosen-theme)))

(use-package diminish
  :config (diminish 'eldoc-mode))

(use-package doom-modeline
  :ensure t
  :init
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-lsp t))

(use-package dimmer
  :custom (dimmer-fraction 0.1)
  :config (dimmer-mode))

(show-paren-mode)

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package centered-window
  :ensure t
  :custom
  (cwm-centered-window-width 180))

(use-package highlight-indent-guides)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; (use-package tree-sitter
;;   :hook ((ruby-mode . tree-sitter-hl-mode)
;;          (js-mode . tree-sitter-hl-mode)
;;          (typescript-mode . tree-sitter-hl-mode)
;;          (go-mode . tree-sitter-hl-mode)))
;; (use-package tree-sitter-langs)

(setq-default fill-column 135)

(electric-pair-mode)
(add-function :before-until electric-pair-inhibit-predicate (lambda (c) (eq c ?<)))

(use-package s)
(use-package dash)

(defun mus/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "ETBembo" :weight 'bold :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun mus/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :hook (org-mode . mus/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

(setq org-src-tab-acts-natively nil)
  (mus/org-font-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun mus/org-mode-visual-fill ()
  (setq visual-fill-column-width 135
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . mus/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; (fset 'evil-redirect-digit-argument 'ignore)

;; (add-to-list 'evil-digit-bound-motions 'evil-org-beginning-of-line)
;; (evil-define-key 'motion 'evil-org-mode
;;   (kbd "0") 'evil-org-beginning-of-line)

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . (lambda () (evil-org-set-key-theme '(navigation todo insert textobjects additional))))))

(mus/leader-keys
 "o"   '(:ignore t :which-key "org mode")
 "oi"  '(:ignore t :which-key "insert")
 "oil" '(org-insert-link :which-key "insert link")
 "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow"))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom (
           (if (eq system-type 'darwin)
               (insert-directory-program "gls" dired-use-ls-dired t))
             ;; (dired-listing-switches "-al --group-directories-first")
             (dired-listing-switches "-agho --group-directories-first"))
           :config
           (evil-collection-define-key 'normal 'dired-mode-map
             "h" 'dired-single-up-directory
             "l" 'dired-single-buffer))

(use-package dired-single
  :commands (dired dired-jump))

;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode))

;; (use-package dired-open
;;   :commands (dired dired-jump)
;;   :config
;;   ;; Doesn't work as expected!
;;   ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
;;   (setq dired-open-extensions '(("png" . "feh")
;;                                 ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package magit
  :ensure t
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :defer t)

(use-package perspective
  :demand t
  :bind (("C-M-k" . persp-switch)
         ("C-M-n" . persp-next)
         ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-initial-frame-name "Main")
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
    (persp-mode)))

(use-package vterm
  :commands vterm
  :config
  ;;(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(defun mus/switch-project-action ()
  "Switch to a workspace with the project name."
  (persp-switch (projectile-project-name))
  ;; (projectile-dired)
  ;; (magit-status)
  )
  (use-package projectile
    :diminish
    ;; :bind (("C-c k" . #'projectile-kill-buffers)
    ;;       ("C-c M" . #'projectile-compile-project))
    :custom
    (projectile-completion-system 'ivy)
    (projectile-enable-caching t)
    :config (projectile-mode)
    :init
    (setq projectile-switch-project-action #'mus/switch-project-action)
    ;; (setq counsel-projectile-switch-project-action #'mus/switch-project-action)
    )

(use-package ivy
  :diminish
  :custom
  (ivy-height 30)
  (ivy-use-virtual-buffers nil)
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode 1)
  :bind (("C-c C-r" . #'ivy-resume)
         ("C-c s"   . #'swiper-thing-at-point)
         ("C-s"     . #'swiper)))

(use-package ivy-rich
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer nil)
  (ivy-rich-path-style 'full)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode))

(use-package counsel
  :init
  (counsel-mode 1)
  :diminish
  :config
  (mus/leader-keys
    "i" 'counsel-imenu
    "f" 'counsel-find-file
    "s" 'counsel-projectile-rg
    "x" 'counsel-M-x
    "b" 'persp-counsel-switch-buffer))

(use-package counsel-projectile
  :config
  (evil-define-key 'normal global-map (kbd "C-p")     'counsel-projectile)
  ;; (evil-define-key 'normal global-map (kbd "C-S-p")   'counsel-projectile-switch-project)
  (evil-define-key 'normal global-map (kbd "C-S-p")   'projectile-switch-project))

(use-package smex)

(use-package flycheck
  :after org
  :hook
  (org-src-mode . disable-flycheck-for-elisp)
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  (flycheck-set-indication-mode 'left-margin)

  (defun disable-flycheck-for-elisp ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

  (add-to-list 'flycheck-checkers 'proselint)
  (setq-default flycheck-disabled-checkers '(haskell-stack-ghc)))

(use-package flycheck-inline
  :disabled
  :config (global-flycheck-inline-mode))

(use-package flyspell
  :ensure t
  :defer t
  :hook ((org-mode . flyspell-mode)
         (markdown-mode . flyspell-mode))
  :init
  :config

  (use-package flyspell-correct
    :after flyspell
    :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

  (use-package flyspell-correct-ivy
    :after flyspell-correct)


  (use-package flyspell-lazy
    :after flyspell
    :config
    (setq flyspell-lazy-idle-seconds 1
          flyspell-lazy-window-idle-seconds 3)
    (flyspell-lazy-mode 1)))

(use-package deadgrep
  :ensure t)

(use-package company
  :diminish
  :bind (("C-." . #'company-capf))
  :bind (:map company-active-map
         ("C-n" . #'company-select-next)
         ("C-p" . #'company-select-previous))
  :hook (prog-mode . company-mode)
  :custom
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-tooltip-limit 20 "The more the merrier.")
  (company-tooltip-idle-delay 0.4 "Faster!")
  (company-async-timeout 20 "Some requests can take a long time. That's fine.")
  (company-idle-delay 1.5 "Default is way too low.")
  :config)

;(use-package lsp-mode
;  :commands (lsp lsp-execute-code-action)
;  :hook ((go-mode . lsp-deferred)
;         (lsp-mode . lsp-enable-which-key-integration)
;         (lsp-mode . lsp-diagnostics-modeline-mode))
;  :bind ("C-c C-c" . #'lsp-execute-code-action)
;  :custom
;  (lsp-diagnostics-modeline-scope :project)
;  (lsp-file-watch-threshold 5000)
;  (lsp-response-timeout 2)
;  (lsp-ui-doc-mode nil)
;  (lsp-enable-file-watchers nil))

;;; (use-package lsp-ui
;;;   :custom
;;;   (lsp-ui-doc-mode nil)
;;;   :after lsp-mode)

;(use-package lsp-ivy
;  :after (ivy lsp-mode))

;(use-package company-lsp
;  :disabled
;  :custom (company-lsp-enable-snippet t)
;  :after (company lsp-mode))

(defun mus/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred lsp-execute-code-action)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-diagnostics-modeline-mode))
  :bind(("C-c C-c" . #'lsp-execute-code-action)
        ("C-c i" . #'lsp-format-buffer))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-diagnostics-modeline-scope :project)
  (lsp-file-watch-threshold 5000)
  (lsp-response-timeout 2)
  (lsp-ui-doc-mode nil)
  (lsp-enable-file-watchers nil)
  (lsp-auto-guess-root t)
   (lsp-eldoc-enable-hover t)
  :config
  (define-key evil-motion-state-map (kbd "K") 'lsp-describe-thing-at-point))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-doc-mode nil)
  (lsp-ui-doc-enable nil)
  ;; (lsp-ui-doc-position 'bottom)
  :after lsp-mode)

(use-package lsp-treemacs
  :after lsp-mode)

(use-package lsp-ivy
  :after ivy lsp-mode)


   (use-package company-lsp
    :disabled
    :custom (company-lsp-enable-snippet t)
    :after (company lsp-mode))

(use-package yasnippet
      :defer 3 ;; takes a while to load, so do it async
      :diminish yas-minor-mode
      :config
      (yas-global-mode)
      (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet)
      :custom (yas-prompt-functions '(yas-completing-prompt)))

      (use-package yasnippet-snippets
:ensure t)

(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files "Gemfile"))

(use-package ruby-mode
  :after lsp-mode
  :hook (ruby-mode . lsp-deferred)
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (use-package inf-ruby
    :hook
    (ruby-mode . inf-ruby-minor-mode)
    :init
    (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)))

(use-package rvm
  :diminish
  :config
  (rvm-use-default))


(use-package projectile-rails
  ;; :ensure t
  :commands (projectile-rails-on)
  ;; :after projectile
  :hook ((ruby-mode inf-ruby-mode projectile-rails-server-mode) . projectile-rails-mode)
  :hook ((projectile-mode) . projectile-rails-on)
  :config
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)))

(use-package lispyville
  :hook ((lispy-mode . lispyville-mode))
  :config
  (lispyville-set-key-theme '(operators c-w additional
                                        additional-movement slurp/barf-cp
                                        prettify)))

(with-eval-after-load "projectile"
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

  (use-package lsp-dart
:ensure t
:hook (dart-mode . lsp))

(use-package dart-mode
  :after lsp-mode
  :hook (dart-mode . lsp-deferred))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/home/mads/development/flutter"))

(provide 'init)
