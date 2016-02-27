;; +-----------------------------+
;; | COLOR AND THEME SETUP EMACS |
;; +-----------------------------+

;; load theme
(if window-system
    (load-theme 'solarized-dark t)
  (load-theme 'wombat t))

;; Cycle through this set of themes
(setq my-themes '(solarized-dark sanityinc-tomorrow-night solarized-light))

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

;; Bind this to C-x t
(global-set-key (kbd "C-x t") 'cycle-my-theme)


;; 80 column marker
(require 'fill-column-indicator)
(define-globalized-minor-mode fci-mode-global fci-mode turn-on-fci-mode)
(setq-default fill-column 80)
;;(fci-mode-global 1)
