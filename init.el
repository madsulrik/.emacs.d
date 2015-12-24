;; User info
(setq user-full-name "Mads Ulrik Svendsen")
(setq user-mail-address "mail@madsulrik.com")

;; Setting Environment
(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin:" (getenv "PATH")))
(require 'cl)

;; Initialize Package Mangement
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

(defvar mads/packages '(auto-complete
			ac-math
			autopair
			fsharp-mode
			auctex
			fill-column-indicator
                        flycheck
			org
                        gist
                        graphviz-dot-mode
                        magit
			org-journal
                        marmalade
			python-mode
                        web-mode
                        solarized-theme
			yasnippet
                        htmlize
                        less-css-mode
			fill-column-indicator
			expand-region)
  "Default packages")

(defun mads/packages-installed-p ()
  (loop for pkg in mads/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))



(unless (mads/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg mads/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

; load customizations
(add-hook 'after-init-hook '(lambda ()
                              (load "~/.emacs.d/config/general-config.el")
			      (load "~/.emacs.d/config/modes-config.el")
			      (load "~/.emacs.d/config/org-config.el")
			      (load "~/.emacs.d/config/colors-config.el")
                               ))
