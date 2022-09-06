;;; init.el --- Mads init.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;; Compile
;; (eval-and-compile
;;   (leaf *byte-compile
;;     :custom
;;     (byte-compile-warnings . '(not free-vars))
;;     (debug-on-error        . nil)))
;; (leaf *native-compile
;;   :doc "Native Compile by gccemacs"
;;   :url "https://www.emacswiki.org/emacs/GccEmacs"
;;   :if (and (fboundp 'native-comp-available-p)
;;            (native-comp-available-p))
;;   :custom
;;   (comp-deferred-compilation . nil)
;;   (comp-speed                . 5)
;;   (comp-num-cpus             . 4)
;;   :config
;;   ;; (native-compile-async "~/.emacs.d/early-init.el" 4 t)
;;   (native-compile-async "~/.emacs.d/init.el" 4 t)
;;   (native-compile-async "~/.emacs.d/elpa/" 4 t)
;;   (native-compile-async "~/.emacs.d/lisp/" 4 t)
;;   ;; (native-compile-async "~/.emacs.d/el-get/" 4 t)
;;   )

;; -----------------------------------------------------------------------------------------
;;
;; Generic Configurations
;;
;; -----------------------------------------------------------------------------------------

;; Silencer
(leaf no-littering
  :doc "Keep .emacs.d clean"
  :url "https://github.com/emacscollective/no-littering"
  :ensure t
  :require t)

(leaf *to-be-quiet
  :doc "Quite annoying messages"
  :preface
  (defun display-startup-echo-area-message ()
    "no startup message"
    (message ""))
  :config
  (defalias 'yes-or-no-p #'y-or-n-p))

;; Turn server off for now
;;(leaf *server
;;  :doc "Use Emacs as a Server"
;;  :global-minor-mode server-mode)

(leaf *encoding
  :doc "It's time to use UTF-8"
  :config
  (set-locale-environment "en_US.UTF-8")
  (prefer-coding-system          'utf-8-unix)
  (set-default-coding-systems    'utf-8-unix)
  (set-selection-coding-system   'utf-8-unix)
  (set-buffer-file-coding-system 'utf-8-unix))

(leaf *formatting
  :custom
  (truncate-lines        . t)
  (require-final-newline . t)
  (tab-width             . 2)
  (indent-tabs-mode      . nil))

(leaf *autorevert
  :doc "Revert changes if local file is updated"
  :global-minor-mode global-auto-revert-mode
  :custom (auto-revert-interval . 0.1))

(leaf *recovery
  :doc "Save place of cursor"
  :global-minor-mode save-place-mode)

(leaf *savehist
  :doc "save history of minibuffer"
  :global-minor-mode savehist-mode)

(leaf *recentf
  :doc "Record open files history"
  :global-minor-mode recentf-mode
  :custom
  (recentf-max-saved-items . 20000)
  (recentf-max-menu-items  . 20000)
  (recentf-auto-cleanup    . 'never)
  (recentf-exclude
   . '((expand-file-name package-user-dir)
		   ".cache"
		   "cache"
       "bookmarks"
		   "recentf"
       "*.png"
       "*.jpeg"
       ".org_archive"
		   "COMMIT_EDITMSG\\'")))

(leaf *large-file
  :doc "Adjust large file threshold"
  :custom
  (large-file-warning-threshold . 1000000))

(leaf *electric
  :doc "Enable electric pair mode"
  :custom
  (electric-pair-mode . t))

(leaf *osx-fix
  :doc "fix issues for osx"
  :config
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta)

  (leaf exec-path-from-shell
    :ensure t
    :config
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))


;; -----------------------------------------------------------------------------------------
;;
;; Evil setup
;;
;; -----------------------------------------------------------------------------------------

(leaf *rebind-for-evil
  :bind
  (("<escape>" . keyboard-escape-quit)
   ("C-M-U" . universal-argument)))

(leaf evil
  :doc "vi emulation for emacs"
  :ensure t
  :init (setq evil-want-integration t
              evil-want-keybinding nil
              evil-want-C-u-scroll t
              evil-want-C-d-scroll t
              evil-want-C-i-jump t
              evil-respect-visual-line-mode t
              evil-undo-system 'undo-redo)
  :config (evil-mode))

(leaf evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(leaf evil-commentary
  :after evil
  :ensure t
  :config
  (evil-commentary-mode))

(leaf general
  :doc "fantastic library for defining prefixed keybindings, especially in conjunction with Evil modes."
  :ensure t
  :config
  (define-key evil-motion-state-map "," 'nil)
  (general-evil-setup t)
  (general-create-definer mus/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer mus/local-leader-key-def
    :states '(normal visual emacs)
    :prefix ","
    :global-prefix "C-,")
  (general-create-definer mus/ctrl-c-keys
    :prefix "C-c"))

;; -----------------------------------------------------------------------------------------
;;
;; Error checking
;;
;; -----------------------------------------------------------------------------------------

;; TODO: Toggle flycheck with SPC-t-c
(leaf flycheck
  :doc "Syntax checker"
  :url "https://www.flycheck.org/en/latest/"
  :ensure t
  :global-minor-mode global-flycheck-mode
  :custom
  (flycheck-display-errors-delay . 0))

;; -----------------------------------------------------------------------------------------
;;
;; User Interface
;;
;; -----------------------------------------------------------------------------------------

(leaf *minimal-interface
  :doc "Clean up Emacs’ user interface, make it more minimal"
  :config
  ;; Every Emacs window should, by default occupy all the screen space it can.
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Set a better title bar for osx
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq frame-title-format nil)
  (setq ns-use-proxy-icon nil)

  (setq inhibit-startup-message t)
  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  ;; (set-fringe-mode 10)
  (menu-bar-mode -1))

(leaf *improve-scrolling
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll one line at a time
  (setq use-dialog-box nil))

;; Doom theme

(leaf doom-themes
  :doc "Megapack of themes"
  :url "https://github.com/doomemacs/themes"
  :ensure t
  :config
  (load-theme 'doom-dracula t))

;; Doom modeline

(leaf doom-modeline
  :doc "A fancy and fast mode-line inspired by minimalism design."
  :url "https://github.com/seagle0128/doom-modeline"
  :ensure t
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-lsp t))



;; -----------------------------------------------------------------------------------------
;;
;;  Accessibility
;;
;; -----------------------------------------------------------------------------------------

(leaf display-line-numbers
  :doc "Display line number"
  :url "https://www.emacswiki.org/emacs/LineNumbers"
  :config
  (column-number-mode)
  (global-display-line-numbers-mode t))

(leaf rainbow-delimiters
  :doc "Display brackets in rainbow"
  :url "https://www.emacswiki.org/emacs/RainbowDelimiters"
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))


(leaf hl-todo
  :doc "Highlight TODO and similar keywords in comments and strings"
  :url "https://github.com/tarsius/hl-todo"
  :ensure t
  :hook (prog-mode-hook . hl-todo-mode))

(leaf hl-line
  :doc "Highligt current line"
  :hook ((prog-mode-hook . hl-line-mode)
         (text-mode-hook . hl-line-mode)))


(leaf which-key
  :doc "Displays available keybindings in popup"
  :url "https://github.com/justbur/emacs-which-key"
  :ensure t
  :global-minor-mode which-key-mode)



;; -----------------------------------------------------------------------------------------
;;
;; Completion interface
;;
;; -----------------------------------------------------------------------------------------

(leaf vertico
  :doc "Completion interface"
  :url "https://github.com/minad/vertico/"
  :global-minor-mode vertico-mode
  :ensure t
  :custom
  (vertico-cycle . t)
  (vertico-count . 18))


(leaf orderless
  :doc "Completion style that matches multiple regexps"
  :url "https://github.com/oantolin/orderless"
  :ensure t
  :custom
  (completion-styles             . '(orderless basic))
  (completion-category-default   . nil)
  (completion-category-overrides . '((file (styles partial-completion)))))


(leaf marginalia
  :doc "Explain details of the consult candidates"
  :url "https://github.com/minad/marginalia"
  :global-minor-mode marginalia-mode
  :ensure t
  :custom-face
  (marginalia-documentation . '((t (:foreground "#6272a4")))))

(leaf consult
  :doc "Generate completion candidates and provide commands for completion"
  :url "https://github.com/minad/consult"
  :ensure t
  :bind
  (:minibuffer-local-map
   ("C-r" . consult-history))
  :init
  (mus/leader-key-def
    "f"   '(:ignore t :which-key "find")
    "ff"  'find-file
    "s"   '(:ignore t :which-key "search")
    "ss"  '(consult-line :whick-key "search current file")
    "sp"  '(consult-ripgrep :which-key "search current project")
    "b"   '(:ignore t :which-key "buffer")
    "bb"  '(consult-buffer :which-key "Switch buffer"))
  :config
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;; set porjectile as project root
  (autoload 'projectile-project-root "projectile"))
(leaf consult-flycheck
  :doc "Consult integration for Flycheck"
  :url "https://github.com/minad/consult-flycheck"
  :ensure t)
(leaf affe
  :doc "Asynchronous Fuzzy Finder"
  :url "https://github.com/minad/affe"
  :ensure t)

(leaf embark
  :doc "Mini-Buffer Actions Rooted in Keymaps Resources"
  :url "https://github.com/oantolin/embark"
  :ensure t
  :bind*
  ("C-M-." . embark-act)
  :custom
  (prefix-help-command . #'embark-prefix-help-command)
  :config
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(leaf embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(leaf corfu
  :doc "Corfu enhances completion at point with a small completion popup"
  :url "https://github.com/minad/corfu"
  :ensure t
  :require t
  :bind
  ("C-." . completion-at-point)
  (:corfu-map
   ("M-m" . corfu-move-to-minibuffer))
  :preface
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let (completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  :custom
  (corfu-cycle . t)
  (corfu-auto . t)
  (corfu-preview-current . nil)
  (corfu-quit-at-boundary . t)
  (corfu-quit-no-match . t)
  :config
  (global-corfu-mode))

;; Projectile ------------------------------------------------------------------------------

(leaf projectile
  :doc "Project navigation and management library"
  :url "https://github.com/bbatsov/projectile"
  :ensure t
  :global-minor-mode projectile-mode
  :bind-keymap
  (("C-c p" . 'projectile-command-map)) ;; TODO: Should only use the leaders
  :config
  (mus/leader-key-def
    "p"   '(:ignore t :which-key "projectile")
    "pf"  '(projectile-find-file :which-key "find file in project")
    "pa"  '(projectile-add-known-project :which-key "Add project")
    "pp"  '(projectile-switch-project :which-key "switch project")
    "pb"  '(consult-project-buffer :which-key "switch buffer")

    "p4"   '(:ignore t :which-key "in other window")
    "p4b"   '(projectile-switch-to-buffer-other-window :which-key "switch buffer other window")
    "p5"   '(:ignore t :which-key "in other frame")
    ))

;; TODO: ADD a persp-mode


;; -----------------------------------------------------------------------------------------
;;
;; Tools
;;
;; -----------------------------------------------------------------------------------------

(leaf all-the-icons
  :doc "All the icons is used by NeoTree"
  :url "https://github.com/domtronn/all-the-icons.el"
  :ensure t)

(leaf neotree
  :doc "Sidebar for dired"
  :url "https://github.com/jaypei/emacs-neotree"
  :ensure t
  :commands (neotree-toggle)
  :preface
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
  (defun mus/neotree-hook (&optional _unused)
    (display-line-numbers-mode -1))
  :hook
  (neotree-mode-hook . mus/neotree-hook)
  :init
  (setq neo-create-file-auto-open nil
        neo-auto-indent-point nil
        neo-autorefresh nil
        neo-mode-line-type 'none
        neo-window-width 30
        neo-show-updir-line nil
        neo-theme 'icons
        neo-banner-message nil
        neo-confirm-create-file #'off-p
        neo-confirm-create-directory #'off-p
        neo-show-hidden-files nil
        neo-keymap-style 'concise
        neo-show-hidden-files t
        neo-hidden-regexp-list
        '(;; vcs folders
          "^\\.\\(?:git\\|hg\\|svn\\)$"
          ;; compiled files
          "\\.\\(?:pyc\\|o\\|elc\\|lock\\|css.map\\|class\\)$"
          ;; generated files, caches or local pkgs
          "^\\(?:node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
          ;; org-mode folders
          "^\\.\\(?:sync\\|export\\|attach\\)$"
          ;; temp files
          "~$"
          "^#.*#$"))
  (mus/leader-key-def
    "o" '(:ignore t :which-key "open")
    "op" 'neotree-project-dir
    ))


;; Git ------------------------------------------------------------------------------------

(leaf *git-commit-mode
  :doc "Mode for git commit message editing"
  :mode "\\COMMIT_EDITMSG\\'")
(leaf git-modes
  :doc "Modes for git configuration files"
  :url "https://github.com/magit/git-modes"
  :ensure t)

(leaf magit
  :doc "Complete text-based user interface to Git"
  :url "https://magit.vc/"
  :ensure t
  :custom
  (magit-display-buffer-function . #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (mus/leader-key-def
    "g"   '(:ignore t :which-key "git")
    "gg"   'magit-status
    ))

(leaf magit-todos
  :doc "This package displays keyword entries from source code comment"
  :url "https://github.com/alphapapa/magit-todos"
  :ensure t)

(leaf git-gutter
  :doc "Show git status in fringe & operate hunks"
  :url "https://github.com/emacsorphanage/git-gutter"
  :disabled t
  :ensure t
  :global-minor-mode global-git-gutter-mode
  :custom
  (git-gutter:modified-sign . "┃")
  (git-gutter:added-sign    . "┃")
  (git-gutter:deleted-sign  . "┃")
  :custom-face
  (git-gutter:modified . '((t (:foreground "#f1fa8c"))))
  (git-gutter:added    . '((t (:foreground "#50fa7b"))))
  (git-gutter:deleted  . '((t (:foreground "#ff79c6")))))


;; -----------------------------------------------------------------------------------------
;;
;; Programming
;;
;; -----------------------------------------------------------------------------------------

; tree-sitter

; lsp/eglot
;; Lets try eglot first.

(leaf eglot
  :doc "An Emacs LSP client that stays out of your way"
  :url "https://github.com/joaotavora/eglo"
  :ensure t
  ;; TODO: add keymap
  )


;; Ruby ------------------------------------------------------------------------------------

(leaf *ruby-projectile
  :doc "Make sure projectile knows a ruby project"
  :after projectile
  :config
  (add-to-list 'projectile-project-root-files "Gemfile"))
 
(leaf ruby-mode
  :doc "ruby config"
  :mode "\\.\\(?:a?rb\\|aslsx\\)\\'"
  :mode "/\\(?:Brew\\|Fast\\)file\\'"
  :interpreter "j?ruby\\(?:[0-9.]+\\)"
  :hook
  (ruby-mode-hook . eglot-ensure)
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (mus/local-leader-key-def
    :keymaps 'ruby-mode-map
    "'" 'robe-start))
(leaf inf-ruby
  :doc "inf-ruby provides a REPL buffer connected to a Ruby subprocess."
  :url "https://github.com/nonsequitur/inf-ruby"
  :ensure t
  :hook
  (ruby-mode . inf-ruby-minor-mode)
  :init
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter))
;; (leaf robe
;;   :doc "Robe is a code assistance tool that uses a Ruby REPL subprocess with your application or gem code loaded"
;;   :url "https://github.com/dgutov/robe"
;;   :ensure t
;;   :hook (ruby-mode-hook . robe-mode))
(leaf rspec-mode
  :doc "RSpec mode provides some convenience functions for dealing with RSpec."
  :url "https://github.com/pezra/rspec-mode"
  :ensure t
  :hook (ruby-mode-hook . rspec-mode)
  :config
  ;; TODO: fix key commands
  ;; (mus/local-leader-key-def
  ;;   :keymaps 'ruby-mode-map
  ;;   "t"  '(:ignore t :which-key "test")
  ;;   )
  )
;; rubocop

;; Maybe use eglot/lsp instead of robe/rubocop

;; inflections
;; projectile-rails

(leaf web-mode
  :mode (".html?$" ".erb$")
  :ensure t
  ;; :hook 
  ;; (web-mode-hook . eglot-ensure)
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

(leaf emmet-mode
  :doc "the essential toolkit for web-developers"
  :url "https://github.com/smihica/emmet-mode"
  :ensure t
  :hook
  (web-mode-hook . emmet-mode)
  ;; :config
  )

(leaf css-mode
  :ensure t
  ;; :hook
  ;; (css-mode-hook . eglot-ensure)
  )
  

;; Typescript ---------------------------------------------------------------------------


;; Dart + Flutter -----------------------------------------------------------------------

(leaf dart-mode
  :doc "An Emacs major mode for editing Dart files."
  :url "https://github.com/bradyt/dart-mode"
  :ensure t
  :hook
  (dart-mode-hook . eglot-ensure))

(leaf flutter
  :doc "Emacs tools for working with the Flutter SDK"
  :url "https://github.com/amake/flutter.el"
  :ensure t
  :after dart-mode)

;; Clojure ------------------------------------------------------------------------------


(leaf clojure-mode
  :doc "clojure-mode is an Emacs major mode that provides font-lock (syntax highlighting), indentation, navigation and refactoring support"
  :url "https://github.com/clojure-emacs/clojure-mode"
  :ensure t
  :hook
  (clojure-mode-hook . eglot-ensure)
  (clojurescript-mode-hook . eglot-ensure)
  (clojurec-mode-hook . eglot-ensure)
  :config
  (require 'flycheck-clj-kondo))
(leaf flycheck-clj-kondo
  :ensure t)
(leaf cider
  :doc "CIDER extends Emacs with support for interactive programming in Clojure. "
  :url "https://github.com/clojure-emacs/cider"
  :ensure t)


;; Vilpy or paredit or lispy
(leaf avy ;; requirements to vilpy
  :doc "Jump to things in tree-style"
  :url "https://github.com/abo-abo/avy"
  :ensure t)

(leaf vilpy
  :load-path ("~/.emacs.d/lisp/vilpy")
  :require t
  :doc "vilpy is a vi-like paredit"
  :url "https://github.com/Andre0991/vilpy"
  :hook
  (clojure-mode-hook . vilpy-mode)
  (emacs-lisp-mode-hook . vilpy-mode))



(provide 'init)
;;; init.el ends here
