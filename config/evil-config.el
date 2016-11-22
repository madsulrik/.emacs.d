; Give us back Ctrl+U for vim emulation
(setq evil-want-C-u-scroll t)

(require 'evil)
(evil-mode 1)

;; surround.vim stuff
;;(require 'evil-surround)
;;(global-evil-surround-mode 1)

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; CTRL-U back to Vim-like behaviour (half screen up)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-u")
  (lambda ()
    (interactive)
    (evil-delete (point-at-bol) (point))))


;; ctags for evil mode


(define-key evil-normal-state-map (kbd "gf")
  (lambda () (interactive) (find-tag (find-tag-default-as-regexp))))

(define-key evil-normal-state-map (kbd "gb") 'pop-tag-mark)

(define-key evil-normal-state-map (kbd "gn")
  (lambda () (interactive) (find-tag last-tag t)))

