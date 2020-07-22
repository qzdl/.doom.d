  (map! "<mouse-8>" 'better-jumper-jump-backward)
  (map! "<mouse-9>" 'better-jumper-jump-forward)

(map! "C-x C-k" #'custom/kill-this-buffer)
(map! "C-x k" #'kmacro-keymap)

(map! "s-h" #'windmove-left)
(map! "s-j" #'windmove-down)
(map! "s-k" #'windmove-up)
(map! "s-l" #'windmove-right)

(map! "s-n" #'next-buffer)
(map! "s-p" #'previous-buffer)

(define-key key-translation-map [?\C-x] [?\C-u])
(define-key key-translation-map [?\C-u] [?\C-x])

(map! "C-x C-'" #'+eshell/toggle)

(map! "s-B" 'toggle-rot13-mode)

(defun qzdl/utc-timestamp ()
  (format-time-string "%Y%m%dT%H%M%SZ" (current-time) t))

(defun qzdl/toggle-1->0 (n)
  (if (equal 1 n) 0 1))

(defun qzdl/toggle-on->off (n)
  (if (equal 1 n) "on" "off"))

(defun qz/pprint (form &optional output-stream)
  (princ (with-temp-buffer
           (cl-prettyprint form)
           (buffer-string))
         output-stream))

(setq user-full-name "Samuel Culpepper"
      user-mail-address "samuel@samuelculpepper.com")

(setq doom-font (font-spec :family "monospace" :size 16))
(setq doom-theme nil)
(setq doom-modeline-height 10)
(setq display-line-numbers-type nil)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(tooltip-mode 1)

(setq qzdl/toggle-time-state t)
(display-time-mode qzdl/toggle-time-state)

(defun qzdl/toggle-time-in-modeline ()
  (interactive)
  (message
   (concat "Time display in modeline is "
           (if (display-time-mode
                (setq qzdl/toggle-time-state
                      (qzdl/toggle-1->0 qzdl/toggle-time-state)))
               "on" "off"))))

(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "Time in the modeline"   "T" #'qzdl/toggle-time-in-modeline))

(load! "elegance/elegance.el")
(load! "elegance/sanity.el")

(setq writeroom-width 80)

(add-to-list 'writeroom-mode-hook
             (lambda () (setq writeroom-border-width 50)))

(defun qzdl/load-tron-legacy ()
  (interactive)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/.local/straight/repos/tron-legacy-emacs-theme/")
  (load-theme 'tron-legacy t)
  (setq tron-legacy-vivid-cursor t))

(defun qzdl/load-k ()
  (interactive)
  (load-theme 'k t))

(defun qzdl/load-pink-mountain ()
  (interactive)
  (load-theme 'pink-mountain t))

(require 'ivy-posframe)
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
(add-hook! 'exwm-init-hook
  (after! ivy-posframe
    (add-to-list 'ivy-posframe-parameters '(parent-frame . nil))))
(ivy-posframe-mode 1)

(setq qzdl/preferred-transparency-alpha '(80 . 70))

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
         qzdl/preferred-transparency-alpha '(100 . 100))))
  (message (concat "Frame transparency set to "
                   (number-to-string (car (frame-parameter nil 'alpha))))))

(perfect-margin-mode 1)

(require 'exwm-randr)

(defun qzdl/exwm-ultrawide ()
  (interactive)
  (setq exwm-randr-workspace-monitor-plist '(0 "DP-1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()(start-process-shell-command "xrandr" nil
                                              "xrandr --output DP-1 --mode 5120x1440 --primary --output eDP-1 --off")))
  (exwm-randr-enable))

(defun qzdl/exwm-tpb ()
  (interactive)
  (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1" 1 "DP-1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()(start-process-shell-command "xrandr" nil
                                              "xrandr --output HDMI-2 --off --output HDMI-1 --off --output DP-1 --mode 1680x1050 --pos 1920x0 --rotate normal --output eDP-1 --primary --mode 1920x1080 --pos 0x920 --rotate normal --output DP-2 --off")))
  (exwm-randr-enable))


(defun qzdl/exwm-110vga ()
  (interactive)
  (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1" 1 "DP-2"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()(start-process-shell-command "xrandr" nil
                                              "xrandr --output HDMI-2 --off --output HDMI-1 --off --output DP-1 --off --output eDP-1 --primary --mode 1920x1080 --pos 0x352 --rotate normal --output DP-2 --mode 1920x1080 --pos 1920x0 --rotate normal")))
  (exwm-randr-enable))

(qzdl/exwm-ultrawide)
(exwm-enable)

(setq qzdl/startup-programs
      '("compton"
        "unclutter"))

(defun qzdl/run-programs-n-process (p)
  (mapcar (lambda (c) (start-process-shell-command c nil c)) p))

(defun qzdl/seq-to-kill (p)
  (mapcar (lambda (s) (concat "killall " s)) p))

(defun qzdl/run-startup-programs ()
  (interactive)
  (qzdl/run-programs-n-process
   (qzdl/seq-to-kill qzdl/startup-programs))
  (qzdl/run-programs-n-process qzdl/startup-programs))

(qzdl/run-startup-programs)

(require 'exwm)

;; Set the initial workspace number.
(unless (get 'exwm-workspace-number 'saved-value)
  (setq exwm-workspace-number 4))

;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

;; ensure doom recognises x-windows as 'real' per <link-to-elisp-doc
;; 'doom-real-buffer-p>
 (add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)

(defun qz/mark-this-buffer-as-real ()
  (interactive)
  (doom-mark-buffer-as-real-h))

(defun qz/read-process-shell-command (command)
  "Used to launch a program by creating a process. Invokes
`start-process-shell-command' with COMMAND"
  (interactive (list (read-shell-command "λ ")))
  (start-process-shell-command command nil command))

(setq exwm-input-global-keys
      `(([?\s-r] . exwm-reset)                     ;; `s-r': Reset (to line-mode).
        ([?\s-w] . exwm-workspace-switch)          ;; `s-w': Switch workspace.
        ([?\s-&] . qz/read-process-shell-command)  ;; `s-&': Launch program
        ,@(mapcar (lambda (i)                           ;; `s-N': Switch to certain workspace.
                    `(,(kbd (format "s-%d" i)) .   ;; expands to n binds ([s-N] . λ exwm-wsc N)
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))



(qz/pprint exwm-input-global-keys)

(setq exwm-input-simulation-keys
      '(([?\C-b] . [left])
        ([?\C-f] . [right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\M-d] . [C-delete])
        ([?\M-b] . [C-left])
        ([?\M-f] . [C-right])
        ([?\C-k] . [S-end delete])
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))

(setq wallpaper-cycle-interval 900)

(use-package! wallpaper
  :hook ((exwm-randr-screen-change . wallpaper-set-wallpaper)
         (after-init . wallpaper-cycle-mode))
  :custom ((wallpaper-cycle-interval 900)
           (wallpaper-cycle-single t)
           (wallpaper-scaling 'fill)
           (wallpaper-cycle-directory "~/.config/wallpapers")))

(server-start)

(map! :leader
      (:prefix-map ("n" . "notes")
       (:prefix-map ("d" . "by date")
          :desc "Arbitrary date" "d" #'org-roam-dailies-date
          :desc "Today"          "t" #'org-roam-dailies-today
          :desc "Tomorrow"       "m" #'org-roam-dailies-tomorrow
          :desc "Yesterday"      "y" #'org-roam-dailies-yesterday)
       "f" #'org-roam-find-file
       "F" #'find-file-in-notes))

(setq qzdl/psql-error-rollback 0)

(qzdl/toggle-1->0 qzdl/psql-error-rollback)

(defun qzdl/psql-toggle-error-rollback ()
  (interactive)
  (setq qzdl/psql-error-rollback
        (qzdl/toggle-1->0 qzdl/psql-error-rollback))
  (sql-send-string
   (concat "\\set ON_ERROR_ROLLBACK "
           (qzdl/toggle-on->off qzdl/psql-error-rollback)))
  (sql-send-string
   "\\echo ON_ERROR_ROLLBACK is :ON_ERROR_ROLLBACK"))

  (defun qzdl/upcase-sql-keywords ()
    (interactive)
    (save-excursion
      (dolist (keywords sql-mode-postgres-font-lock-keywords)
        (goto-char (point-min))
        (while (re-search-forward (car keywords) nil t)
          (goto-char (+ 1 (match-beginning 0)))
          (when (eql font-lock-keyword-face (face-at-point))
            (backward-char)
            (upcase-word 1)
            (forward-char))))))

(if (symbolp 'cl-font-lock-built-in-mode)
    (cl-font-lock-built-in-mode 1))

(defun qzdl/slime-eval-last-expression-eros ()
  (interactive)
  (destructuring-bind (output value)
      (sly-eval `(slynk:eval-and-grab-output ,(sly-last-expression)))
    (eros--make-result-overlay (concat output value)
      :where (point)
      :duration eros-eval-result-duration)))

(define-key! emacs-lisp-mode-map "C-c C-c" 'eval-defun)

(require 'hyperbole)

(map! "C-<mouse-2>" #'hkey-either)

(add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(setq org-file-apps
      '((auto-mode . emacs)
        (directory . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . emacs)))

(eval-after-load nil
  (remove-hook 'org-mode-hook #'ob-ipython-auto-configure-kernels))

(setq org-directory "~/life/"
      qzdl/notes-directory (concat org-directory "roam/")
      qzdl/org-agenda-directory (concat org-directory "gtd/")
      org-noter-notes-search-path (list qzdl/notes-directory)
      org-roam-directory qzdl/notes-directory)

(setq org-refile-targets '(("next.org" :level . 0)
                           ("someday.org" :level . 0)
                           ("reading.org" :level . 1)
                           ("projects.org" :maxlevel . 1)))

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
        org-ellipses " ..."
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

(setq jiralib-url "https://jira.thinkproject.com")

(require 'org-recoll)

(setq org-recoll-command-invocation "recollq -t -A"
      org-recoll-results-num 100)

(global-set-key (kbd "C-c g") #'org-recoll-search)
(global-set-key (kbd "C-c u") #'org-recoll-update-index)

(require 'org-protocol)

(require 'org-capture)

(setq qzdl/capture-title-timestamp "%(qzdl/utc-timestamp)-${slug}")

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

;; helper capture function for `org-roam' for `agenda-mode'
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
  (require 'org-roam-protocol))

(org-roam-mode +1)

(setq qzdl/org-roam-capture-head
      "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}\n")

(setq org-roam-capture-templates
        `(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name ,qzdl/capture-title-timestamp
           :head ,qzdl/org-roam-capture-head
           :unnarrowed t)
          ("_" "pass-though-todo" plain (function org-roam--capture-get-point)
           "%?"
           :file-name ,qzdl/capture-title-timestamp
           :head ,qzdl/org-roam-capture-head
           :immediate-finish t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name ,(concat "private-" qzdl/capture-title-timestamp)
           :head ,qzdl/org-roam-capture-head
           :unnarrowed t)))

  (setq org-roam-capture-ref-templates
        `(("r" " ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name ,qzdl/capture-title-timestamp
           :head "#+SETUPFILE:./hugo_setup.org
#+ROAM_KEY: ${ref}
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}
#+SOURCE: ${ref}"
           :unnarrowed t)))

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

(use-package! org-roam-server
:config
(setq org-roam-server-host "127.0.0.1"
      org-roam-server-port 8080
      org-roam-server-export-inline-images t
      org-roam-server-authenticate nil
      org-roam-server-network-label-truncate t
      org-roam-server-network-label-truncate-length 60))



(require 'org-ref)
(setq reftex-bib-path  '("~/life/tex.bib")
      reftex-default-bibliography reftex-bib-path
      org-ref-default-bibliography reftex-bib-path)

(use-package! org-agenda
  :init
  (map! "<f1>" #'qzdl/switch-to-agenda)
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode t
        org-agenda-files (list org-roam-directory))
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

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  ("C-c n t" . org-journal-today)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "private-%Y-%m-%d.org")
  (org-journal-dir org-roam-directory)
  (org-journal-carryover-items nil)
  (org-journal-enable-agenda-integration nil)
  (org-journal-date-format "%Y-%m-%d")
  :config
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t)))

(require 'ox-reveal)

(setq gnus-secondary-select-methods '((nntp "list.postgres.org")))
