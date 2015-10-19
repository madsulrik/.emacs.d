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
                        solarized-theme
			yasnippet
                        htmlize
                        less-css-mode
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


;; startup options

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'fsharp-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq make-backup-files nil)
(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin/"))

(setq indent-tabs-mode nil)

;; (global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;;(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

;; Fix alt keys
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

(pending-delete-mode t)


;; enable ido mode
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; line numbers and column numbers
(require 'linum)
(global-linum-mode t)
(setq column-number-mode t)

;;Display time
(display-time-mode 1)
(setq display-time-string-forms
       '((propertize (concat " " 24-hours ":" minutes " ")
 		    'face 'egoge-display-time)))


;; random setups

(require 'autopair)
(autopair-global-mode)


;; fsharp mode setup
(require 'fsharp-mode)
(setq inferior-fsharp-program "/usr/local/bin/fsharpi --readline-")
(setq fsharp-compiler "/usr/local/bin/fsharpc")

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

;; yasnippet setup
(require 'yasnippet)

(yas-global-mode 1)

;; Expand region setup
(require 'expand-region)
(global-set-key (kbd "C-'") 'er/expand-region)

;; 
;; org mode setup
;;

(require 'ox-latex)
(add-to-list 'org-latex-classes
          '("org-noter"
             "\\documentclass[a4paper]{article}
              \\usepackage[utf8]{inputenc}
              \\usepackage[danish]{babel}
              \\usepackage [T1]{fontenc}
              \\usepackage[margin=2.5cm]{geometry}
              \\usepackage{hyperref}
              \\usepackage{graphicx}
              \\usepackage{amsmath}
              \\setlength\\parindent{0pt}"))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh         . t)
   (js         . t)
   (emacs-lisp . t)
   (perl       . t)
   (scala      . t)
   (clojure    . t)
   (python     . t)
   (ruby       . t)
   (dot        . t)
   (css        . t)
   (plantuml   . t)))

(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)

(setq org-export-with-section-numbers nil)

;; youtube tricks for org-mode
(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"720\""
          " height=\"480\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))

;; org journal stuff
(defvar org-journal-file "~/Documents/.journal/journal.org"
  "Path to OrgMode journal file.")
(defvar org-journal-date-format "%A, %d-%m-%Y"
  "Date format string for journal headings.")

(defun org-journal-entry ()
  "Create a new diary entry for today or append to an existing one."
  (interactive)
  (switch-to-buffer (find-file org-journal-file))
  (widen)
  (setq isearch-forward t)
  (let ((today (format-time-string org-journal-date-format)))
    (beginning-of-buffer)
    (unless (org-goto-local-search-headings today nil t)
      ((lambda ()
         (org-insert-heading)
         (insert today)
         (insert "\n"))))
    (beginning-of-buffer)
    (org-show-entry)
    (org-narrow-to-subtree)
    (end-of-buffer)
    (backward-char 1)
    (unless (= (current-column) 2)
      (insert "\n\n "))))

(global-set-key (kbd "C-c j") 'org-journal-entry)


;; less Mode
(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

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

