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
;;(setq less-css-lessc-options '("-ru"))
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

;; Projectile
(projectile-global-mode)

;;(add-hook 'cc-mode-hook 'projectile-mode)

;;
;; Ruby mode
;;
(add-to-list 'auto-mode-alist
                          '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

;; -------------
;; Haskell Mode
;; -------------

;; Bind the F8 key to navigate to the import section
(eval-after-load 'haskell-mode
  (lambda ()
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports)
  '(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)))

;; Let emacs now the cabal path
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))
(custom-set-variables '(haskell-tags-on-save t))


(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t)
  '(haskell-process-type 'cabal-repl))
(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

;; General ghc mode settings
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
