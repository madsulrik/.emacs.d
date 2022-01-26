;;; IDE features

;; Dired

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

;;
;; Git
;;

(use-package magit
  :ensure t
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :defer t)

;;
;; Workspaces
;;

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

;;
;; Projectile
;;

(defun mus/switch-project-action ()
  "Switch to a workspace with the project name."
  (persp-switch (projectile-project-name))
  (projectile-dired)
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


;;
;; Ivy, counsel and swiper
;;

(use-package ivy
  :diminish
  :custom
  (ivy-height 30)
  (ivy-use-virtual-buffers nil)
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode 1)
  :bind (("C-c C-r" . #'ivy-resume)
         ("C-c s" . #'swiper-thing-at-point)
         ("C-s" . #'swiper)))

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
  (evil-define-key 'normal global-map (kbd "C-p") 'counsel-projectile)
  ;; (evil-define-key 'normal global-map (kbd "C-S-p")   'counsel-projectile-switch-project)
  (evil-define-key 'normal global-map (kbd "C-S-p") 'projectile-switch-project))

(use-package smex)

;;
;; Flycheck
;;

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

;;
;; Flyspell
;;

(use-package flyspell
  :ensure t
  :defer t
  :hook ((org-mode . flyspell-mode)
         (markdown-mode . flyspell-mode)
         (text-mode . flyspell-mode))
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


;;
;; Write good
;;

(use-package writegood-mode
  :config
  (mus/leader-keys
    "g" '(:ignore t :which-key "Write Good Mode")
    "gg" '(writegood-grade-level :which-key "Write Good Grade level")
    "ge" '(writegood-reading-ease :which-key "Write Good reading ease")))

;;
;; Searching
;;

(use-package deadgrep
  :ensure t)

;;
;; Autocomplete
;;

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
  (company-idle-delay 0.1 "Default is way too low.")
  :config)


;;
;; LSP mode
;;

(defun mus/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred lsp-execute-code-action)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-diagnostics-modeline-mode))
  :bind (("C-c C-c" . #'lsp-execute-code-action)
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

;; something with help buffers displayed at the bottom
(add-to-list
 'display-buffer-alist
 '((lambda (buffer _)
     (with-current-buffer
         buffer
       (seq-some (lambda (mode) (derived-mode-p mode)) '(help-mode))))
   (display-buffer-reuse-window display-buffer-below-selected)
   (reusable-frames . visible)
   (window-height . 0.33)))

;; lsp ui
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


;; lsp Treemacs
(use-package lsp-treemacs
  :after lsp-mode)

;; Lsp ivy
(use-package lsp-ivy
  :after ivy lsp-mode)

(use-package company-lsp
  :disabled
  :custom (company-lsp-enable-snippet t)
  :after (company lsp-mode))

;;
;; Snippets
;;

(use-package yasnippet
  :defer 3 ;; takes a while to load, so do it async
  :diminish yas-minor-mode
  :config
  (yas-global-mode)
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet)
  :custom (yas-prompt-functions '(yas-completing-prompt)))

(use-package yasnippet-snippets
  :ensure t)
