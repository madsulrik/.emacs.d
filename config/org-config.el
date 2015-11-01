;; +----------------+
;; | ORG MODE SETUP |
;; +----------------+

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
