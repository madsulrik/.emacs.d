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


;; Fix alt keys on mac
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

;; selected text can be deleted
(pending-delete-mode t)

;; highlight current line
(global-hl-line-mode t)

;; show parentases
(show-paren-mode t)

;; Turn off bip warnings
(setq visible-bell 1)

;; general keybindings
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x g") 'magit-status)

;; enable ido mode
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; kill buffer history
(add-hook 'kill-buffer-hook
 (lambda ()
  (setq buffer-name-history
        (delete*
         (buffer-name)
         buffer-name-history :test 'string=))))


;; line numbers and column numbers
(require 'linum)
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

