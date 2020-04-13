;;;; -*- lexical-binding: t -*-


;;; INTRODUCE YOSELF
(setq user-full-name "Samuel Culpepper"
      user-mail-address "samuel@samuelculpepper.com")


;;; VISUAL
(setq doom-font (font-spec :family "monospace" :size 30))
(setq doom-theme 'doom-one)
(setq display-line-numbers-type 'relative)

;;; KEYFREQ
;; blessed be, zah lee
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;;; ORG
(use-package! org
  :mode ("\\.org\\'" . org-mode)
  :init
  (map! :leader
        :prefix "n"
        "c" #'org-capture)
  (map! :map org-mode-map
        "M-n" #'outline-next-visible-heading
        "M-p" #'outline-previous-visible-heading
        "C-c l" #'org-insert-link
        "C-c L" #'org-cliplink)
  (setq org-src-window-setup 'current-window
        org-return-follows-link t
        org-babel-load-languages '((emacs-lisp . t)
                                   ;; (common-lisp . t)
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

;;; ORG-RECOLL
(require 'org-recoll)
(global-set-key (kbd "C-c g") #'org-recoll-search)
(global-set-key (kbd "C-c u") #'org-recoll-update-index)

;; helper capture function
(defun qzdl/current-roam-link ()
  (interactive)
  "Get link to org-roam file with title"
  (concat "* TODO [[" (buffer-file-name) "]["
          (car (org-roam--extract-titles)) "]]"))

(defun qzdl/org-inbox-capture ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "i"))

(defun qzdl/org-roam-capture-current ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "I"))

(setq org-capture-templates
      `(("i" "inbox" entry (file ,(concat qzdl/org-agenda-directory "inbox.org"))
          "* TODO %?")
        ("I" "current-roam" entry (file ,(concat qzdl/org-agenda-directory "inbox.org"))
         (function qzdl/current-roam-link)
         :immediate-finish t)
        ("c" "org-protocol-capture" entry (file ,(concat qzdl/org-agenda-directory "inbox.org"))
          "* TODO [[%:link][%:description]]\n\n %i"
          :immediate-finish t)
        ("w" "Weekly Review" entry
          (file+olp+datetree ,(concat qzdl/org-agenda-directory "reviews.org"))
          (file ,(concat qzdl/org-agenda-directory "templates/weekly_review.org")))
        ("r" "Reading" todo ""
          ((org-agenda-files '(,(concat qzdl/org-agenda-directory "reading.org")))))))

;;; ORG-AGENDA
(use-package! org-agenda
  :init
  (map! "<f1>" #'qzdl/switch-to-agenda)
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode t)
  (defun qzdl/switch-to-agenda ()
    (interactive)
    (org-agenda nil " "))
  :config
  (setq org-columns-default-format
        "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
  (setq org-agenda-custom-commands
        `((" " "Agenda"
           ((agenda ""
                    ((org-agenda-span 'week)
                     (org-deadline-warning-days 365)))
            (todo "TODO"
                  ((org-agenda-overriding-header "To Refile")
                   (org-agenda-files '(,(concat qzdl/org-agenda-directory "inbox.org")))))
            (todo "TODO"
                  ((org-agenda-overriding-header "Emails")
                   (org-agenda-files '(,(concat qzdl/org-agenda-directory "emails.org")))))
            (todo "NEXT"
                  ((org-agenda-overriding-header "In Progress")
                   (org-agenda-files '(,(concat qzdl/org-agenda-directory "someday.org")
                                       ,(concat qzdl/org-agenda-directory "projects.org")
                                       ,(concat qzdl/org-agenda-directory "next.org")))))
            (todo "TODO"
                  ((org-agenda-overriding-header "Projects")
                   (org-agenda-files '(,(concat qzdl/org-agenda-directory "projects.org")))))
            (todo "TODO"
                  ((org-agenda-overriding-header "One-off Tasks")
                   (org-agenda-files '(,(concat qzdl/org-agenda-directory "next.org")))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))))))


;;; ORG-ROAM
(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#005200"))))
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-graph-show
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "qzdl/org-roam-capture-current" "C" #'qzdl/org-roam-capture-current
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-directory org-roam-directory
        org-roam-db-location (concat org-roam-directory "org-roam.db")
        org-roam-graph-exclude-matcher "private")
  :config
  (require 'org-roam-protocol)
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private-%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+ROAM_KEY: ${ref}
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}
- source :: ${ref}"
           :unnarrowed t))))

(use-package company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend!
    'org-mode
    '(company-org-roam company-yasnippet company-dabbrev)))

(define-key! org-roam-mode-map
  "C-c n l" #'org-roam
  "C-c n f" #'org-roam-find-file
  "C-c n b" #'org-roam-switch-to-buffer
  "C-c n g" #'org-roam-graph-show
  "C-c n C" #'qzdl/org-roam-capture-current
  "C-c n i" #'org-roam-insert)

(define-key org-roam-mode-map (kbd "C-c n C") #'qzdl/org-roam-capture-current)

(org-roam-mode +1)

(setq org-refile-targets '(("next.org" :level . 0)
                           ("someday.org" :level . 0)
                           ("reading.org" :level . 1)
                           ("projects.org" :maxlevel . 1)))


(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  ("C-c n t" . org-journal-today)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "private-%Y-%m-%d.org")
  (org-journal-dir org-roam-directory)
  (org-journal-carryover-items nil)
  (org-journal-date-format "%Y-%m-%d")
  :config
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t)))
