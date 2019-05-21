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
                        gist
                        graphviz-dot-mode
                        magit
			org
                        org-bullets
                        marmalade
                        slime
			python-mode
                        enh-ruby-mode
                        web-mode
                        js2-mode
                        color-theme-sanityinc-tomorrow
                        solarized-theme
                        spacemacs-theme
			yasnippet
                        htmlize
                        less-css-mode
			fill-column-indicator
			expand-region
                        ido-ubiquitous
                        projectile
                        flx-ido
                        cc-mode
                        edts
                        intero
                        elm-mode
                        neotree
			)
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
                              (load "~/.emacs.d/config/evil-config.el")
                               ))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (elm-mode yasnippet web-mode spacemacs-theme solarized-theme slime robe python-mode projectile org-bullets marmalade magit less-css-mode js2-mode iy-go-to-char intero ido-ubiquitous htmlize graphviz-dot-mode gist ghc fsharp-mode flx-ido fill-column-indicator expand-region evil enh-ruby-mode edts color-theme-sanityinc-tomorrow autopair auctex ac-math))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
