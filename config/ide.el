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
  ;; (treemacs-create-workspace (projectile-project-name))
  ;; (treemacs-switch-workspace (projectile-project-name))
  ;; (treemacs-display-current-project-exclusively)
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
;; Treemacs
;;
;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;   (progn
;;     (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
;;           treemacs-deferred-git-apply-delay        0.5
;;           treemacs-directory-name-transformer      #'identity
;;           treemacs-display-in-side-window          t
;;           treemacs-eldoc-display                   'simple
;;           treemacs-file-event-delay                5000
;;           treemacs-file-extension-regex            treemacs-last-period-regex-value
;;           treemacs-file-follow-delay               0.2
;;           treemacs-file-name-transformer           #'identity
;;           treemacs-follow-after-init               t
;;           treemacs-expand-after-init               t
;;           treemacs-find-workspace-method           'find-for-file-or-pick-first
;;           treemacs-git-command-pipe                ""
;;           treemacs-goto-tag-strategy               'refetch-index
;;           treemacs-indentation                     2
;;           treemacs-indentation-string              " "
;;           treemacs-is-never-other-window           nil
;;           treemacs-max-git-entries                 5000
;;           treemacs-missing-project-action          'ask
;;           treemacs-move-forward-on-expand          nil
;;           treemacs-no-png-images                   nil
;;           treemacs-no-delete-other-windows         t
;;           treemacs-project-follow-cleanup          nil
;;           treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;           treemacs-position                        'left
;;           treemacs-read-string-input               'from-child-frame
;;           treemacs-recenter-distance               0.1
;;           treemacs-recenter-after-file-follow      nil
;;           treemacs-recenter-after-tag-follow       nil
;;           treemacs-recenter-after-project-jump     'always
;;           treemacs-recenter-after-project-expand   'on-distance
;;           treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
;;           treemacs-show-cursor                     nil
;;           treemacs-show-hidden-files               t
;;           treemacs-silent-filewatch                nil
;;           treemacs-silent-refresh                  nil
;;           treemacs-sorting                         'alphabetic-asc
;;           treemacs-select-when-already-in-treemacs 'move-back
;;           treemacs-space-between-root-nodes        t
;;           treemacs-tag-follow-cleanup              t
;;           treemacs-tag-follow-delay                1.5
;;           treemacs-text-scale                      nil
;;           treemacs-user-mode-line-format           nil
;;           treemacs-user-header-line-format         nil
;;           treemacs-wide-toggle-width               70
;;           treemacs-width                           35
;;           treemacs-width-increment                 1
;;           treemacs-width-is-initially-locked       t
;;           treemacs-workspace-switch-cleanup        nil)

;;     ;; The default width and height of the icons is 22 pixels. If you are
;;     ;; using a Hi-DPI display, uncomment this to double the icon size.
;;     ;;(treemacs-resize-icons 44)

;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-fringe-indicator-mode 'always)

;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null treemacs-python-executable)))
;;       (`(t . t)
;;        (treemacs-git-mode 'deferred))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple)))

;;     (treemacs-hide-gitignored-files-mode nil))
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t d"   . treemacs-select-directory)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

;; (use-package treemacs-projectile
;;   :after (treemacs projectile)
;;   :ensure t)

;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once)
;;   :ensure t)

;; (use-package treemacs-magit
;;   :after (treemacs magit)
;;   :ensure t)

;; (use-package treemacs-perspective ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs perspective) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

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
  :hook
  (org-src-mode . disable-flycheck-for-elisp)
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode t)
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (flycheck-set-indication-mode 'left-margin)

  (defun disable-flycheck-for-elisp ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

  (add-to-list 'flycheck-checkers 'proselint)
  (setq-default flycheck-disabled-checkers '(haskell-stack-ghc)))

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode))

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
  :config
  (push 'company-robe company-backends))



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
