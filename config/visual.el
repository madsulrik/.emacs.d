;;; Visual config

;; Fonts

(set-face-attribute 'default nil :font "Fira Code" :height mus/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height mus/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Fira Code" :height mus/default-font-size)


;; Set the variable pitch face
;; (set-face-attribute 'variable-pitch nil :font "Cantarell" :height mus/default-variable-font-size :weight 'regular)
;; (set-face-attribute 'variable-pitch nil :font "ETBembo" :height mus/default-variable-font-size :weight 'thin)


(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))


;; Every Emacs window should, by default occupy all the screen space it can.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set a better title bar for osx
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq frame-title-format nil)
(setq ns-use-proxy-icon nil)


;; Window chrome both wastes space and looks unappealing.
(when (window-system)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

;; I use the [[https://github.com/hlissner/emacs-doom-themes][Doom Emacs themes]], which are gorgeous.
(use-package doom-themes
  :config
  (let ((chosen-theme 'doom-dracula))
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)
    (setq doom-challenger-deep-brighter-comments t
          doom-challenger-deep-brighter-modeline t
          doom-dark+-blue-modeline nil)
    (load-theme chosen-theme)))

;; Most major modes pollute the modeline, so we pull in diminish.el to quiesce them.
(use-package diminish
  :config (diminish 'eldoc-mode))

;; The default modeline is pretty uninspiring, and doom-modeline doesn't appear to be particularly slow.
(use-package doom-modeline
  :ensure t
  :init
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-lsp t))

;; I find it useful to have a slightly more apparent indicator of which buffer is active at the moment.
(use-package dimmer
  :custom (dimmer-fraction 0.1)
  :config (dimmer-mode))

;; Highlighting the closing/opening pair associated with a given parenthesis is essential. Furthermore, parentheses should be delimited
;; by color. I may be colorblind, but it's good enough, usually.
(show-paren-mode)
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; It's nice to have the option to center a window, given the considerable size of my screen.
(use-package centered-window
  :ensure t
  :custom
  (cwm-centered-window-width 180))

;; Having indentation highlights can help with Haskell and YAML and other languages where indentation is crucial and sometimes difficult to follow in long lines.
(use-package highlight-indent-guides)

;; Which key
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))
