;; User info
(setq user-full-name "Mads Ulrik Svendsen")
(setq user-mail-address "mail@madsulrik.com")

;; Setting Environment
(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin" (getenv "PATH")))
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
                        marmalade
			python-mode
                        solarized-theme)
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


;; startup options

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq make-backup-files nil)
(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin/"))

(setq indent-tabs-mode nil)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

;; Fix alt keys
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)


;; enable ido mode
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; line numbers
(require 'linum)
(global-linum-mode t)

;; random setups

(require 'autopair)
(autopair-global-mode)


;; fsharp mode setup
(require 'fsharp-mode)

(setq inferior-fsharp-program "/usr/bin/fsharpi --readline-")
(setq fsharp-compiler "/usr/bin/fsharpc")

(add-hook 'fsharp-mode-hook
 (lambda ()
   (define-key fsharp-mode-map (kbd "M-RET") 'fsharp-eval-region)
   (define-key fsharp-mode-map (kbd "C-SPC") 'fsharp-ac/complete-at-point)))


;; auto complete setup
(require 'auto-complete-config)
(ac-config-default)

(add-to-list 'ac-modes 'latex-mode)

;; Activate ac-math.
(eval-after-load "latex"
  '(when (featurep 'auto-complete)
     ;; See https://github.com/vspinu/ac-math
     (require 'ac-math)
     (defun ac-latex-mode-setup ()       ; add ac-sources to default ac-sources
       (setq ac-sources
         (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
             ac-sources)))
     (add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)))


;; flyspell
(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

;; Setup python mode
(require 'python-mode)


;; Theme
(if window-system
    (load-theme 'solarized-dark t)
  (load-theme 'wombat t))

;; Cycle through this set of themes
(setq my-themes '(solarized-dark solarized-light))

(setq my-cur-theme nil)
(defun cycle-my-theme ()
  "Cycle through a list of themes, my-themes"
  (interactive)
  (when my-cur-theme
    (disable-theme my-cur-theme)
    (setq my-themes (append my-themes (list my-cur-theme))))
  (setq my-cur-theme (pop my-themes))
  (load-theme my-cur-theme t))

;; Switch to the first theme in the list above
(cycle-my-theme)

;; Bind this to C-t
(global-set-key (kbd "C-x t") 'cycle-my-theme)
