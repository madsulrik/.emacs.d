;; User info
(setq user-full-name "Mads Ulrik Svendsen")
(setq user-mail-address "mail@madsulrik.com")

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'org-install)
(require 'ob-tangle)

(when (boundp 'comp-speed)
  (setq comp-speed 2))

(defun reload-config ()
  (interactive)
  (org-babel-load-file "~/.emacs.d/config/general.org"))

(setq max-lisp-eval-depth 2000)

(reload-config)
