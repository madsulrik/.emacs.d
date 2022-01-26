;;; OSX fix
;; There is some path issues when using Mac OSX. The following fixes the issues


(use-package exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq mac-option-modifier nil)
(setq mac-command-modifier 'meta)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/opt/homebrew/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))
;; (when (equal system-type 'darwin)
;;  (setq insert-directory-program "/opt/homebrew/bin/gls"))
