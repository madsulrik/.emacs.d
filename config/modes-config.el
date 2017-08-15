;; +-------------------+
;; | MODES SETUP EMACS |
;; +-------------------+

;; autopair global
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

;; slime setup
(setq inferior-lisp-program "clisp")
(setq slime-contribs '(slime-fancy))


;; python mode
(require 'python-mode)

;; web mode
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(setq web-mode-engines-alist
  '(("php"    . "\\.phtml\\'")
    ("blade"  . "\\.blade\\."))
)

;; javascript mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; yasnippet setup
(require 'yasnippet)
(yas-global-mode 1)


;; Expand region setup
(require 'expand-region)
(global-set-key (kbd "C-'") 'er/expand-region)


;; less Mode
(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

;; Projectile
(projectile-global-mode)

;;(add-hook 'cc-mode-hook 'projectile-mode)

;; iy go to char

(global-set-key (kbd "C-c f") 'iy-go-to-char)
