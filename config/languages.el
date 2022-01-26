;;; Language support

;;
;; Ruby on rails
;;

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

(use-package web-mode
  :mode (".html?$" ".erb$")
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

(use-package projectile-rails
  ;; :ensure t
  :commands (projectile-rails-on)
  ;; :after projectile
  :hook ((ruby-mode inf-ruby-mode projectile-rails-server-mode) . projectile-rails-mode)
  :hook ((projectile-mode) . projectile-rails-on)
  :config
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))

;;
;; Meta lisp
;;

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)))

(use-package lispyville
  :hook ((lispy-mode . lispyville-mode))
  :config
  (lispyville-set-key-theme '(operators c-w additional
                                        additional-movement slurp/barf-cp
                                        prettify)))

;;
;; Dart and flutter
;;

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


;;
;; Typescript
;;

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(defun mus/set-js-indentation ()
  (setq-default js-indent-level 2)
  (setq-default evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'mus/set-js-indentation)
  (add-hook 'json-mode-hook #'mus/set-js-indentation))