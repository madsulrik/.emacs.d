;; Text manipulation

(setq-default fill-column 135)

(electric-pair-mode)
(add-function :before-until electric-pair-inhibit-predicate (lambda (c) (eq c ?<)))

(use-package s)
(use-package dash)
