;;;; -*- lexical-binding: t -*-

(require 'exwm)
(require 'exwm-config)
(exwm-config-default)
(server-start)


;;; INTRODUCE YOSELF
(setq user-full-name "Samuel Culpepper"
      user-mail-address "samuel@samuelculpepper.com")


;;; DVORAK
;;; stellar tips from https://www.emacswiki.org/emacs/DvorakKeyboard
;; flip `C-u' with `C-x'
(define-key key-translation-map [?\C-x] [?\C-u])
(define-key key-translation-map [?\C-u] [?\C-x])


;;; NICE BINDINGS
(map! "C-x <C-return>" #'+eshell/toggle)


;;; HYPERBOLE
(require 'hyperbole)
(map! "C-<mouse-2>" #'hkey-either)


;;; VISUAL
(setq doom-font (font-spec :family "monospace" :size 15))
(setq doom-theme 'doom-one)
(setq display-line-numbers-type 'relative)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)


;;; PDF
;; auto-enable midnight to take colours from theme.
(add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)


;;; KEYFREQ
;; blessed be, zah lee
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/.local/straight/repos/tron-legacy-emacs-theme/")
(load-theme 'tron-legacy t)
(setq tron-legacy-vivid-cursor t)


;;; JIRA
(setq jiralib-url "https://jira.thinkproject.com")


;;; TRANSPARENCY
;; totally stolen from https://www.emacswiki.org/emacs/TransparentEmacs
(setq qzdl/preferred-transparency-alpha '(90 . 85))

(set-frame-parameter (selected-frame) 'alpha qzdl/preferred-transparency-alpha)
(add-to-list 'default-frame-alist `(alpha . ,qzdl/preferred-transparency-alpha))

(defun qzdl/toggle-transparency ()
  "Toggle between max opacity and `qzdl/preferred-transparency-alpha'"
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         qzdl/preferred-transparency-alpha '(100 . 100)))))


;;; ORG
(require 'ox-reveal)
(require 'org-protocol)

;; total freakshow on that hook
(eval-after-load nil
  (remove-hook 'org-mode-hook #'ob-ipython-auto-configure-kernels))

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
                                   ;; (common-lisp . t)
                                   (python . t)
                                   (ipython . t)
                                   (dot . t)
                                   (R . t))
        org-confirm-babel-evaluate nil
        org-use-speed-commands t
        org-catch-invisible-edits 'show
        org-preview-latex-image-directory "/tmp/ltximg/"
        ;; ORG SRC BLOCKS `C-c C-,'
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


;;; ORG-CAPTURE
(require 'org-capture)
; (require 'org-protocol)
(global-set-key (kbd "C-c c") 'org-capture)


;;; ORG-RECOLL
(require 'org-recoll)
(global-set-key (kbd "C-c g") #'org-recoll-search)
(global-set-key (kbd "C-c u") #'org-recoll-update-index)


;; helper capture function for `org-roam' in `agenda-mode'
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

(defun qzdl/org-roam-capture-todo ()
  (interactive)
  "Capture a task in agenda mode."
  (org-roam-capture nil "_"))

(setq org-capture-templates
      `(("i" "inbox" entry (file ,(concat qzdl/org-agenda-directory "inbox.org"))
         "* TODO %?")
        ;; capture link to live `org-roam' thing
        ("I" "current-roam" entry (file ,(concat qzdl/org-agenda-directory "inbox.org"))
         (function qzdl/current-roam-link)
         :immediate-finish t)
        ;; fire directly into inbox
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
(defun qzdl/utc-timestamp ()
  (format-time-string "%Y%m%dT%H%M%SZ" (current-time) t))

(setq qzdl/capture-title-timestamp "%(qzdl/utc-timestamp)-${slug}")

(setq qzdl/graph-backends '("dot" "neato"))

(defun qzdl/available-graph-backends ()
  (mapcar (lambda (e) (if (equal org-roam-graph-executable e)
                     (concat e " (current)") e))
          qzdl/graph-backends))

(defun qzdl/org-roam-choose-graph-backend ()
  (interactive)
  (setq org-roam-graph-executable
        (completing-read "Choose a graph backend: "
                         (qzdl/available-graph-backends)))
  (message (concat "Graph backend set to " org-roam-graph-executable)))

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#df85ff"))))
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "qzdl/org-roam-capture-todo" "_" #'qzdl/org-roam-capture-todo
        :desc "qzdl/org-roam-capture-current" "C" #'qzdl/org-roam-capture-current
        :desc "qzdl/org-roam-capture-current" "C-c" #'qzdl/org-roam-capture-current
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-directory org-roam-directory
        org-roam-db-location (concat org-roam-directory "org-roam.db")
        org-roam-graph-executable "dot"
        org-roam-graph-extra-config '(("overlap" . "false"))
        org-roam-graph-exclude-matcher nil)
  :config
  (require 'org-roam-protocol)
  (setq org-roam-capture-templates
        `(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name ,qzdl/capture-title-timestamp
           :head "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}\n"
           :unnarrowed t)
          ("_" "pass-though-todo" plain (function org-roam--capture-get-point)
           "%?"
           :file-name ,qzdl/capture-title-timestamp
           :head "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}\n"
           :immediate-finish t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name ,(concat "private-" qzdl/capture-title-timestamp)
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-capture-ref-templates
        `(("r" " ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name ,qzdl/capture-title-timestamp
           :head "#+SETUPFILE:./hugo_setup.org
#+ROAM_KEY: ${ref}
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}
#+SOURCE: ${ref}
- source :: ${ref}"
           :unnarrowed t))))

(use-package company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend!
    'org-mode
    '(company-org-roam company-yasnippet company-dabbrev)))



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
