;; +------------------------+
;; | GENERAL EMACS SETTINGS |
;; +------------------------+


;; Open emacs in scrath fshap-mode
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'fsharp-mode)

;; Remove scroll/tool/menu - bar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; No backup files
(setq make-backup-files nil)

;; add textbin to the path
(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin/"))

;; Replace tabs with spaces.
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; open emacs in fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Fix alt keys on mac
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

;; selected text can be deleted
(pending-delete-mode t)

;; highlight current line
(global-hl-line-mode t)

;; show parentases
(show-paren-mode t)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; Turn off bip warnings
(setq visible-bell 1)
(setq ring-bell-function 'ignore)

;; Electric indent mode
(setq electric-indent-mode nil)
;; Automatic indentation
(electric-indent-mode 1) 

;; general keybindings
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; enable ido mode
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

(setq ido-auto-merge-work-directories-length -1)

(ido-ubiquitous-mode 1)


;; kill buffer history
(add-hook 'kill-buffer-hook
 (lambda ()
  (setq buffer-name-history
        (delete*
         (buffer-name)
         buffer-name-history :test 'string=))))


;; line numbers and column numbers
(require 'linum)
(defun linum-update-window-scale-fix (win)
  "fix linum for scaled text"
  (set-window-margins win
          (ceiling (* (if (boundp 'text-scale-mode-step)
                  (expt text-scale-mode-step
                    text-scale-mode-amount) 1)
              (if (car (window-margins))
                  (car (window-margins)) 1)
              ))))
(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)

(global-linum-mode t)
(setq column-number-mode t)

;; Display time in buffers
(display-time-mode 1)
(setq display-time-string-forms
       '((propertize (concat " " 24-hours ":" minutes " ")
 		    'face 'egoge-display-time)))

;; Cut whole line
(defun quick-cut-line ()
  "Cut the whole line that point is on.  Consecutive calls to this command append each line to the kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
	(end (line-beginning-position 2)))
    (if (eq last-command 'quick-cut-line)
	(kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end)))
    (delete-region beg end))
  (beginning-of-line 1)
  (setq this-command 'quick-cut-line))

(global-set-key "\C-c\C-k" 'quick-cut-line)

;; Backup stuff
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

(setq vc-make-backup-files t)

;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)
