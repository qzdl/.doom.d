;;;; -*- lexical-binding: t -*-


;;; INTRODUCE YOSELF
(setq user-full-name "Samuel Culpepper"
      user-mail-address "samuel@samuelculpepper.com")


;;; VISUAL
(setq doom-font (font-spec :family "monospace" :size 30))
(setq doom-theme 'doom-one)
(setq display-line-numbers-type 'relative)


;;; ORG
(use-package! org
  :mode ("\\.org\\'" . org-mode)
  :init
  (map! :leader
        :prefix "n"
        "c" #'org-capture)
  (map! :map org-mode-map
        "M-n" #'outline-next-visible-heading
        "M-p" #'outline-previous-visible-heading)
  (setq org-src-window-setup 'current-window
        org-return-follows-link t
        org-babel-load-languages '((emacs-lisp . t)
                                   (common-lisp . t)
                                   (python . t)
                                   (dot . t)
                                   (R . t))
        org-confirm-babel-evaluate nil
        org-use-speed-commands t
        org-catch-invisible-edits 'show
        org-preview-latex-image-directory "/tmp/ltximg/"
        org-structure-template-alist '(("a" . "export ascii")
                                       ("c" . "center")
                                       ("C" . "comment")
                                       ("e" . "example")
                                       ("E" . "export")
                                       ("h" . "export html")
                                       ("l" . "export latex")
                                       ("q" . "quote")
                                       ("s" . "src")
                                       ("v" . "verse")
                                       ("el" . "src emacs-lisp")
                                       ("d" . "definition")
                                       ("t" . "theorem")))
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'proselint 'org-mode)))

;;; ORG-DIRS
(setq org-directory "~/life/")
(setq qzdl/org-agenda-directory (concat org-directory "gtd/"))
(setq org-roam-directory (concat org-directory "roam/"))

;;; ORG SRC BLOCKS `C-c C-,'
(setq org-structure-template-alist
      '(("a" . "export ascii")
        ("c" . "center")
        ("C" . "comment")
        ("e" . "example")
        ("E" . "export")
        ("h" . "export html")
        ("l" . "export latex")
        ("q" . "quote")
        ("s" . "src")
        ("el" . "src emacs-lisp")
        ("v" . "verse")
        ("d" . "definition")
        ("t" . "theorem")))

;;; ORG-CAPTURE
(require 'org-capture)
(require 'org-protocol)
(global-set-key (kbd "C-c c") 'org-capture)


;; helper capture function
(let ((filebase (lambda (f) (file (concat qzdl/org-agenda-directory f)))))
  (setq org-capture-templates
        `(("i" "inbox" entry ,(filebase "inbox.org")
            "* TODO %?")
          ("c" "org-protocol-capture" entry ,(filebase "inbox.org")
            "* TODO [[%:link][%:description]]\n\n %i"
            :immediate-finish t)
          ("w" "Weekly Review" entry
           (file+olp+datetree ,(concat qzdl/org-agenda-directory "reviews.org"))
           (file ,(concat qzdl/org-agenda-directory "templates/weekly_review.org")))
          ("r" "Reading" todo ""
            ((org-agenda-files '(,(concat qzdl/org-agenda-directory "reading.org"))))))))

;;; ORG-ROAM
(use-package company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend!
    'org-mode
    '(company-org-roam company-yasnippet company-dabbrev)))

(define-key org-roam-mode-map (kbd "C-c n l") #'org-roam)
(define-key org-roam-mode-map (kbd "C-c n f") #'org-roam-find-file)
(define-key org-roam-mode-map (kbd "C-c n b") #'org-roam-switch-to-buffer)
(define-key org-roam-mode-map (kbd "C-c n g") #'org-roam-graph-show)
(define-key org-mode-map (kbd "C-c n i") #'org-roam-insert)
(org-roam-mode +1)

(setq org-refile-targets '(("next.org" :level . 0)
                           ("someday.org" :level . 0)
                           ("reading.org" :level . 1)
                           ("projects.org" :maxlevel . 1)))
;;; DEFT
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory)
  :config/el-patch
  (defun deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
If `deft-use-filename-as-title' is nil, the title is taken to
be the first non-empty line of the FILE.  Else the base name of the FILE is
used as title."
    (el-patch-swap (if deft-use-filename-as-title
                       (deft-base-filename file)
                     (let ((begin (string-match "^.+$" contents)))
                       (if begin
                           (funcall deft-parse-title-function
                                    (substring contents begin (match-end 0))))))
                   (org-roam--get-title-or-slug file))))
(require 'deft)

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  ("C-c n t" . org-journal-today)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "private-%Y-%m-%d.org")
  (org-journal-dir (concat org-roam-directory "private/"))
  (org-journal-carryover-items nil)
  (org-journal-date-format "%Y-%m-%d")
  :config
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t)))
