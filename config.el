(setq user-full-name "Samuel Culpepper"
      user-mail-address "samuel@samuelculpepper.com")

  (map! "<mouse-8>" 'better-jumper-jump-backward)
  (map! "<mouse-9>" 'better-jumper-jump-forward)

(map! "C-x C-k" #'kill-this-buffer)
(map! "C-x k" #'kmacro-keymap)

(map! "s-h" #'windmove-left)
(map! "s-j" #'windmove-down)
(map! "s-k" #'windmove-up)
(map! "s-l" #'windmove-right)

(map! "C-x C-o" #'ace-window)
(map! "C-x o" #'delete-blank-lines)

(map! "s-n" #'next-buffer)
(map! "s-p" #'previous-buffer)

(define-key key-translation-map [?\C-x] [?\C-u])
(define-key key-translation-map [?\C-u] [?\C-x])

(map! "C-x C-'" #'+eshell/toggle)

(map! "s-B" 'toggle-rot13-mode)

(defun qz/utc-timestamp ()
  (format-time-string "%Y%m%dT%H%M%SZ" (current-time) t))

(defun qz/toggle-1->0 (n)
  (if (equal 1 n) 0 1))

(defun qz/toggle-on->off (n)
  (if (equal 1 n) "on" "off"))

(defun qz/pprint (form &optional output-stream)
  (princ (with-temp-buffer
           (cl-prettyprint form)
           (buffer-string))
         output-stream))

(defun qz/bt-a2dp ()
  (interactive)
  (shell-command "pactl set-card-profile bluez_card.2C_41_A1_87_20_BA a2dp_sink"))

(setq doom-font (font-spec :family "monospace" :size 16))
(setq doom-theme nil)
(setq doom-modeline-height 10)
(setq display-line-numbers-type nil)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(tooltip-mode 1)

(setq qz/toggle-time-state t)
(display-time-mode qz/toggle-time-state)

(defun qz/toggle-time-in-modeline ()
  (interactive)
  (message
   (concat "Time display in modeline is "
           (if (display-time-mode
                (setq qz/toggle-time-state
                      (qz/toggle-1->0 qz/toggle-time-state)))
               "on" "off"))))

(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "Time in the modeline"   "T" #'qz/toggle-time-in-modeline))

                                        ;(load! "elegance/elegance.el")
                                        ;(load! "elegance/sanity.el")

(setq writeroom-width 80)

(add-to-list 'writeroom-mode-hook
             (lambda () (setq writeroom-border-width 50)))

(defun qz/load-tron-legacy ()
  (interactive)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/.local/straight/repos/tron-legacy-emacs-theme/")
  (load-theme 'tron-legacy t)
  (setq tron-legacy-vivid-cursor t))

(defun qz/load-k ()
  (interactive)
  (load-theme 'k t))

(defun qz/load-pink-mountain ()
  (interactive)
  (load-theme 'pink-mountain t))

;; cba
(load-theme 'modus-vivendi t)

;(require 'ivy-posframe)
;
;(defun sarg/ivy-posframe-poshandler (info)
;  (setq-local
;   workarea (elt exwm-workspace--workareas exwm-workspace-current-index)
;   return-value (posframe-poshandler-frame-center info)
;    (cons (+ (aref workarea 0) (car return-value))
;          (+ (aref workarea 1) (cdr return-value))))
;  return-value)
;
;(defun sarg/ivy-posframe-exwm (str)
;  (ivy-posframe--display str #'sarg/ivy-posframe-poshandler))
;
;(after! ivy-posframe
;  (setq ivy-posframe-display-functions-alist '((t . sarg/ivy-posframe-exwm))
;        ivy-posframe-border-width 4
;        ivy-posframe-parameters '((parent-frame nil))))
;
;(ivy-posframe-mode 1)

(setq qz/preferred-transparency-alpha '(80 . 70))

(set-frame-parameter (selected-frame) 'alpha qz/preferred-transparency-alpha)
(add-to-list 'default-frame-alist `(alpha . ,qz/preferred-transparency-alpha))

(defun qz/toggle-transparency ()
  "Toggle between max opacity and `qz/preferred-transparency-alpha'"
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         qz/preferred-transparency-alpha '(100 . 100))))
  (message (concat "Frame transparency set to "
                   (number-to-string (car (frame-parameter nil 'alpha))))))

(perfect-margin-mode 1)
(setq perfect-margin-ignore-regexps nil)

(require 'exwm-randr)

(defun qz/exwm-usbc-ultrawide ()
  (setq exwm-randr-workspace-monitor-plist '(0 "DP-2"))
  (add-hook
   'exwm-randr-screen-change-hook
   (lambda ()
     (start-process-shell-command
      "xrandr" nil
      "xrandr --output HDMI-2 --off --output HDMI-1 --off --output DP-1 --off --output eDP-1 --off --output DP-2 --primary --mode 5120x1440 --pos 0x0 --rotate normal")))
  (exwm-randr-enable))

(qz/exwm-usbc-ultrawide)
(exwm-enable)

(setq wallpaper-cycle-interval 900)

(use-package! wallpaper
  :hook ((exwm-randr-screen-change . wallpaper-set-wallpaper)
         (after-init . wallpaper-cycle-mode))
  :custom ((wallpaper-cycle-interval 900)
           (wallpaper-cycle-single t)
           (wallpaper-scaling 'fill)
           (wallpaper-cycle-directory "~/.config/wallpapers")))

(setq qz/startup-programs
      '("compton"
        "unclutter"))

(defun qz/run-programs-n-process (p)
  (mapcar (lambda (c) (start-process-shell-command c nil c)) p))

(defun qz/seq-to-kill (p)
  (mapcar (lambda (s) (concat "killall " s)) p))

(defun qz/run-startup-programs ()
  (interactive)
  (qz/run-programs-n-process
   (qz/seq-to-kill qz/startup-programs))
  (qz/run-programs-n-process qz/startup-programs))

(qz/run-startup-programs)

(require 'exwm-input)

(defmacro qz/exwm-bind-keys (&rest bindings)
  "Bind input keys in EXWM.
INDINGS is a list of cons cells containing a key (string) and a command."
  `(progn
     ,@(cl-loop for (key . cmd) in bindings
                collect `(exwm-input-set-key
                          ,(cond ((stringp key) (kbd key))
                                 (t key))
                          (quote ,cmd)))))

(require 'window-go)
(qz/exwm-bind-keys
 ("s-r" . exwm-reset)                     ;; `s-r': Reset (to line-mode).
 ("s-w" . exwm-workspace-switch)          ;; `s-w': Switch workspace.
 ("s-&" . qz/read-process-shell-command)  ;; `s-&': Launch program
 ("s-h" . windmove-left)
 ("s-j" . windmove-down)
 ("s-k" . windmove-up)
 ("s-l" . windmove-right)
 ("s-n" . switch-to-next-buffer)
 ("s-p" . switch-to-prev-buffer)
 ("s-0" . delete-window)
 ("s-+" . delete-other-windows)
 ("s-b" . qz/exwm-goto-browser)
 ("s-a" . qz/agenda))

(defvar qz/default-simulation-keys
  '(;; movement
    ([?\C-b] . left)
    ([?\M-b] . C-left)
    ([?\C-f] . right)
    ([?\M-f] . C-right)
    ([?\C-p] . up)
    ([?\C-n] . down)
    ([?\C-a] . home)
    ([?\C-e] . end)
    ([?\M-v] . prior)
    ([?\C-v] . next)
    ([?\C-d] . delete)
    ([?\C-k] . (S-end delete))
    ([?\M-d] . (C-S-right delete))
    ;; cut/paste.
    ([?\C-w] . ?\C-x)
    ([?\M-w] . ?\C-c)
    ([?\C-y] . ?\C-v)
    ;; search
    ([?\C-s] . ?\C-f)))

(with-eval-after-load 'exwm-input
  (exwm-input-set-simulation-keys qz/default-simulation-keys))

;(setq exwm-workspace-minibuffer-position 'top)

(menu-bar-mode -1)
(setq mouse-autoselect-window t
      use-dialog-box nil)

;; Set the initial workspace number.
(unless (get 'exwm-workspace-number 'saved-value)
  (setq exwm-workspace-number 4))

;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda () (exwm-workspace-rename-buffer exwm-class-name)))

(setq window-divider-default-right-width 4)
(setq window-divider-default-bottom-width 4)
(window-divider-mode 1)

(add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)
(add-hook 'doom-switch-window-hook #'doom-mark-buffer-as-real-h)

(defun qz/mark-this-buffer-as-real ()
  (interactive)
  (doom-mark-buffer-as-real-h))

(defun qz/read-process-shell-command (command)
  "Used to launch a program by creating a process. Invokes
start-process-shell-command' with COMMAND"
  (interactive (list (read-shell-command "λ ")))
  (start-process-shell-command command nil command))

(add-hook 'exwm-update-title-hook
          (lambda () (exwm-workspace-rename-buffer exwm-title)))

(defcustom qz/exwm-floating-window-classes '("keybase")
  "List of instance names of windows that should start in the floating mode.")

(defun qz/exwm-float-window-on-specific-windows ()
  (when (member exwm-instance-name qz/exwm-floating-window-classes)
    (exwm-floating-toggle-floating)))
(add-hook 'exwm-manage-finish-hook #'qz/exwm-float-window-on-specific-windows)

(defun exwm-goto--switch-to-buffer (buf)
  (if-let ((w (get-buffer-window buf t)))
      (select-window w)
    (exwm-workspace-switch-to-buffer buf)))

(cl-defun exwm-goto (command &key class)
  (if-let ((bs (cl-remove-if-not (lambda (buf)
                                   (with-current-buffer buf
                                     (and (eq major-mode 'exwm-mode)
                                          (cond
                                           ((stringp class)
                                            (string-equal class exwm-class-name))))))
                                 (buffer-list))))
      (exwm-goto--switch-to-buffer (car bs))
    (start-process-shell-command class nil command)))

(defun qz/exwm-goto-browser ()
  (interactive)
  (exwm-goto "firefox" :class "Firefox"))

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

(setq qz/psql-error-rollback 0)

(qz/toggle-1->0 qz/psql-error-rollback)

(defun qz/psql-toggle-error-rollback ()
  (interactive)
  (setq qz/psql-error-rollback
        (qz/toggle-1->0 qz/psql-error-rollback))
  (sql-send-string
   (concat "\\set ON_ERROR_ROLLBACK "
           (qz/toggle-on->off qz/psql-error-rollback)))
  (sql-send-string
   "\\echo ON_ERROR_ROLLBACK is :ON_ERROR_ROLLBACK"))

  (defun qz/upcase-sql-keywords ()
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

(map! :mode paredit-mode
      "M-p" #'paredit-forward-slurp-sexp
      "M-n" #'paredit-backward-slurp-sexp)

(if (symbolp 'cl-font-lock-built-in-mode)
    (cl-font-lock-built-in-mode 1))

(define-key! emacs-lisp-mode-map "C-c C-c" 'eval-defun)

(setq qz/buffer-mod-commands '(qz/get-ingredients-mod-buffer))

(defun qz/get-ingredients-mod-buffer ()
  "scrape the website found in ROAM_KEY for ingredients,
outputting the result in the buffer at-point"
  (interactive)
  (let* ((c (current-buffer))
         (pt (point))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (jsono (json-read-from-string
                 (shell-command-to-string
                  (concat "~/.local/bin/ingredients " (+org--get-property "roam_key")))))
         (ingreds (gethash "ingredients" jsono)))
    (insert "* Ingredients\n")
    (insert
     (apply
      'concat
      (mapcar (lambda (e)
                (concat "- " (gethash "line" e)
                        " [" (number-to-string (gethash "cups" (gethash "measure" e)))
                        " cups]\n")) ingreds)))))

(defun qz/read-property-mod-buffer ()
 (interactive)
 (let* ((command (completing-read "command: " qz/buffer-mod-commands))
       (args (+org--get-property (completing-read "property: " org-default-properties))))
   (setq current-prefix-arg '(4))
   (shell-command (concat command " " args " &"))))

(require 'em-tramp)
(setq eshell-prefer-lisp-functions nil
      eshell-prefer-lisp-variables t
      password-cache t
      password-cache-expiry 300)

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

(set-face-attribute 'org-headline-done nil :strike-through t)

(setq org-directory "~/life/"
      qz/notes-directory (concat org-directory "roam/")
      qz/org-agenda-directory qz/notes-directory
      qz/org-agenda-files (mapcar (lambda (f) (expand-file-name (concat qz/notes-directory f)))
                                  '("calendar-home.org" "calendar-work.org" "schedule.org"))
      org-ref-notes-directory qz/notes-directory
      bibtex-completion-notes-path qz/notes-directory
      org-ref-bibliography-notes "~/life/bib.org"
      org-noter-notes-search-path (list qz/notes-directory)
      org-roam-directory qz/notes-directory)

(setq org-refile-targets '(("next.org" :level . 0)
                           ("reading.org" :level . 0)
                           ("watching.org" :level . 0)
                           ("learning.org" :level . 0)
                           ("wip.org" :level . 1 )))

(use-package! org
  :mode ("\\.org\\'" . org-mode)
  :init
  (map! :leader
        :prefix "n"
        "l" #'org-capture)
  (map! :map org-mode-map
        "M-n" #'outline-next-visible-heading
        "M-p" #'outline-previous-visible-heading)
  (setq org-src-window-setup 'current-window
        org-return-follows-link t
        org-babel-load-languages '((emacs-lisp . t)
                                   ;; (common-lisp . t)
                                   (python . t)
                                   (ipython . t)
                                   (R . t))
        org-ellipsis " ▼ "
        org-confirm-babel-evaluate nil
        org-use-speed-commands t
        org-catch-invisible-edits 'show
        org-preview-latex-image-directory "/tmp/ltximg/"
        ;; ORG SRC BLOCKS {C-c C-,}
        org-structure-template-alist '(("q" . "quote")
                                       ("d" . "definition")
                                       ("s" . "src")
                                       ("sb" . "src bash")
                                       ("se" . "src emacs-lisp")
                                       ("sp" . "src psql")
                                       ("sr" . "src R")
                                       ("el" . "src emacs-lisp")))
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'proselint 'org-mode)))

(require 'org-recoll)

(setq org-recoll-command-invocation "recollq -t -A"
      org-recoll-results-num 100)

(map! "C-c g" #'org-recoll-search)
(map! "C-c u" #'org-recoll-update-index)
(map! :mode org-recoll-mode "q" #'kill-this-buffer)

(require 'org-protocol)

(require 'org-capture)

(setq qz/capture-title-timestamp "%(qz/utc-timestamp)-${slug}")

(setq org-capture-templates
      `(("i" "inbox" entry
         (file ,(concat qz/org-agenda-directory "inbox.org"))
         "* TODO %? \nCREATED: %u\nFROM: %a")
        ;; capture link to live `org-roam' thing
        ("I" "current-roam" entry (file ,(concat qz/org-agenda-directory "inbox.org"))
         (function qz/current-roam-link)
         :immediate-finish t)
        ;; fire directly into inbox
        ("c" "org-protocol-capture" entry (file ,(concat qz/org-agenda-directory "inbox.org"))
         "* TODO [[%:link][%:description]]\n\n %i"
         :immediate-finish t)
        ("w" "Weekly Review" entry
         (file+olp+datetree ,(concat qz/org-agenda-directory "reviews.org"))
         (file ,(concat qz/org-agenda-directory "templates/weekly_review.org")))
        ("r" "Reading" todo ""
         ((org-agenda-files '(,(concat qz/org-agenda-directory "reading.org")))))))

;; helper capture function for `org-roam' for `agenda-mode'
(defun qz/current-roam-link ()
  (interactive)
  "Get link to org-roam file with title"
  (concat "* TODO [[" (buffer-file-name) "]["
          (car (org-roam--extract-titles)) "]]"))

(defun qz/org-inbox-capture ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "i"))

(defun qz/org-roam-capture-current ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "I"))

(defun qz/org-roam-capture-todo ()
  (interactive)
  "Capture a task in agenda mode."
  (org-roam-capture nil "_"))

(setq org-gcal-fetch-file-alist
      `((qz/calendar-home . ,(concat qz/notes-directory "calendar-home.org"))
        (qz/calendar-work . ,(concat qz/notes-directory "calendar-work.org"))
        (qz/calendar-shared . ,(concat qz/notes-directory "calendar-shared.org"))))

(setq org-gcal-recurring-events-mode 'nested)

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
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-insert" "i" #'qz/roam-insert
        :desc "org-agenda-todo" "t" #'qz/org-agenda-todo
        :desc "org-roam-dailies-today" "J" #'org-roam-dailies-today
        :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today
        :desc "qz/org-roam-capture-current" "C" #'qz/org-roam-capture-current
        :desc "qz/org-roam-capture-current" "C-c" #'qz/org-roam-capture-current
        :desc "qz/org-gcal--current" "C-c" #'qz/org-roam-capture-current
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-directory qz/notes-directory
        org-roam-dailies-directory qz/notes-directory
        org-roam-db-location (concat org-roam-directory "org-roam.db")
        org-roam-graph-executable "dot"
        org-roam-graph-extra-config '(("overlap" . "false"))
        org-roam-graph-exclude-matcher nil)

  :config
  (require 'org-roam-protocol))

(org-roam-mode +1)

(setq qz/org-roam-capture-head
      "#+setupfile:./hugo_setup.org
#+hugo_section: zettels
#+hugo_slug: ${slug}
#+title: ${title}\n")

(setq org-roam-capture-templates
      `(("d" "default" plain (function org-roam--capture-get-point)
         "%?"
         :file-name ,qz/capture-title-timestamp
         :head ,qz/org-roam-capture-head
         :unnarrowed t)
        ("_" "pass-though-todo" plain (function org-roam--capture-get-point)
         "%?"
         :file-name ,qz/capture-title-timestamp
         :head ,qz/org-roam-capture-head
         :immediate-finish t)
        ("p" "private" plain (function org-roam-capture--get-point)
         "%?"
         :file-name ,(concat "private-" qz/capture-title-timestamp)
         :head ,qz/org-roam-capture-head
         :unnarrowed t)))

(setq org-roam-capture-ref-templates
      `(("r" " ref" plain (function org-roam-capture--get-point)
         "%?"
         :file-name ,qz/capture-title-timestamp
         :head "#+setupfile:./hugo_setup.org
#+roam_key: ${ref}
#+hugo_slug: ${slug}
#+title: ${title}
#+source: ${ref}"
         :unnarrowed t)))

(setq org-roam-dailies-capture-templates
      `(("d" "default" entry (function org-roam-capture--get-point)
         "* %<%H:%m> %?\nCREATED: %u"
         :file-name  "private-%<%Y-%m-%d>"
         :head "#+title: <%<%Y-%m-%d>>\n")))

(defun vulpea-ensure-filetag ()
  "Add respective file tag if it's missing in the current note."
  (interactive)
  (let ((tags (org-roam--extract-tags-prop
               (buffer-file-name
                (buffer-base-buffer)))))
    (when (and (seq-contains-p tags "person")
               (null (org-roam--extract-global-props-keyword
                      '("filetags"))))
      (let ((tag (qz/title-to-tag (+org-get-global-property "title"))))
        (progn (message tag)
               (org-roam--set-global-prop "filetags" tag)
               (org-roam--set-global-prop "roam_alias" tag))))))

(defun qz/title-to-tag (title)
  "Convert TITLE to tag."
  (if (equal "@" (subseq title 0 1))
      title
    (concat "@" (s-replace " " "" title))))

(defun vulpea-tags-add ()
  "Add a tag to current note."
  (interactive)
  (when (org-roam-tag-add)
    (vulpea-ensure-filetag)))

(defun qz/roam-dispatch-person (title)
  (save-excursion
    (ignore-errors
      (org-back-to-heading)
      (org-set-tags
       (seq-uniq
        (cons
         (vulpea--title-to-tag title)
         (org-get-tags nil t)))))))

(setq qz/roam-tag-dispatch
      '(("person" . qz/roam-dispatch-person)))

(defun qz/roam-insert ()
  "Insert a link to the note."
  (interactive)
  (when-let*
      ((res (org-roam-insert))
       (path (plist-get res :path))
       (title (plist-get res :title))
       (roam-tags (org-roam-with-file path nil
                    (org-roam--extract-tags path))))
    (when (seq-contains-p roam-tags "person")
      (qz/roam-dispatch-person title)
      (save-buffer res))))

(defun vulpea-project-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks.

(1) parse the buffer using org-element-parse-buffer. It
  returns an abstract syntax tree of the current Org buffer. But
  since we care only about headings, we ask it to return only them
  by passing a GRANULARITY parameter - 'headline. This makes
  things faster.

(2) Then we extract information about TODO keyword from
  headline AST, which contains a property we are interested in -
  :todo-type, which returns the type of TODO keyword according to
  org-todo-keywords - 'done, 'todo or nil (when keyword is not
  present).

(3) Now all we have to do is to check if the buffer list contains
  at least one keyword with 'todo type. We could use seq=find on
  the result of org-element-map, but it turns out that it provides
  an optional first-match argument that can be used for our needs."
  (org-element-map                          ; (2)
      (org-element-parse-buffer 'headline) ; (1)
      'headline
    (lambda (h)
      (eq (org-element-property :todo-type h)
          'todo))
    nil 'first-match))                     ; (3)

(defun vulpea-project-update-tag ()
  "Update PROJECT tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             (vulpea-buffer-p))
    (let* ((file (buffer-file-name (buffer-base-buffer)))
           (all-tags (org-roam--extract-tags file))
           (prop-tags (org-roam--extract-tags-prop file))
           (tags prop-tags))
      (if (vulpea-project-p)
          (setq tags (cons "Project" tags))
        (setq tags (remove "Project" tags)))
      (unless (eq prop-tags tags)
        (org-roam--set-global-prop
         "ROAM_TAGS"
         (combine-and-quote-strings (seq-uniq tags)))))))

(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun vulpea-project-files ()
  "Return a list of note files containing Project tag."
  (seq-map
   #'car
   (org-roam-db-query
    [:select file
     :from tags
     :where (like tags (quote "%\"Project\"%"))])))

(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files
        (delete-duplicates
          (append qz/org-agenda-files (vulpea-project-files))
          :test #'string-equal)))


(advice-add 'org-agenda :before #'vulpea-agenda-files-update)

(add-hook 'find-file-hook #'vulpea-project-update-tag)
(add-hook 'before-save-hook #'vulpea-project-update-tag)
(add-hook 'before-save-hook #'vulpea-ensure-filetag)

(defun qz/org-roam-migrate-jobs ()
  (dolist (file (org-roam--list-all-files))
    (message "processing %s" file)
    (with-current-buffer (or (find-buffer-visiting file)
                             (find-file-noselect file))
      (vulpea-project-update-tag)
      (save-buffer))))

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
  (map! "<f1>" #'qz/switch-to-agenda)
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode t
        org-agenda-files (list qz/org-agenda-directory))
  (defun qz/switch-to-agenda ()
    (interactive)
    (org-agenda nil "g"))
  :config
  (setq org-columns-default-format
        "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
  (setq org-agenda-custom-commands
        `(
          ("d" "Upcoming deadlines" agenda ""
           ((org-agenda-time-grid nil)
            (org-deadline-warning-days 365)        ;; [1]
            (org-agenda-entry-types '(:deadline))  ;; [2]
            ))
          ("ww" "wip all" tags "wip")
          ("wr" "wip reading" tags "wip+reading||wip+read|reading+next")
          ("hh" tags "+habit")
          ("P" "Printed agenda"
           ((agenda "" ((org-agenda-span 7)                      ;; overview of appointments
                        (org-agenda-start-on-weekday nil)         ;; calendar begins today
                        (org-agenda-repeating-timestamp-show-all t)
                        (org-agenda-entry-types '(:timestamp :sexp))))
            (agenda "" ((org-agenda-span 1)                      ; daily agenda
                        (org-deadline-warning-days 7)            ; 7 day advanced warning for deadlines
                        (org-agenda-todo-keyword-format "[ ]")
                        (org-agenda-scheduled-leaders '("" ""))
                        (org-agenda-prefix-format "%t%s")))
            (todo "TODO"                                          ;; todos sorted by context
                  ((org-agenda-prefix-format "[ ] %T: ")
                   (org-agenda-sorting-strategy '(tag-up priority-down))
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-overriding-header "\nTasks by Context\n------------------\n"))))
           ((org-agenda-with-colors nil)
            (org-agenda-compact-blocks t)
            (org-agenda-remove-tags t)
            (ps-number-of-columns 2)
            (ps-landscape-mode t))
           ("~/agenda.ps"))
          ;; other commands go here
          )))

                                        ;(defun qz/rg-get-files-with-tags ()
                                        ;  "Returns a LIST of files that contain TAGS (currently, just `TODO')"
                                        ;  (split-string
                                        ;   (shell-command-to-string "rg TODO ~/life/roam/ -c | awk -F '[,:]' '{print $1}'")))
                                        ;
                                        ;(setq org-agenda-files
                                        ;      (append org-agenda-files (qz/rg-get-files-with-tags)))

(defun qz/org-agenda-gtd ()
  (interactive)
  (org-agenda "d." "g")
  (org-agenda-goto-today))

(add-to-list
 'org-agenda-custom-commands
 `("g" "GTD"
   ((agenda "" ((org-agenda-span 'day) (org-deadline-warning-days 60)))
    (tags-todo "wip"
               ((org-agenda-overriding-header "wip")                        ))
    (todo "TODO"
          ((org-agenda-overriding-header "To Refile")
           (org-agenda-files '(,(concat qz/org-agenda-directory "inbox.org")))))
    (todo "TODO"
          ((org-agenda-overriding-header "Emails")
           (org-agenda-files '(,(concat qz/org-agenda-directory "emails.org")))))
    (todo "NEXT"
          ((org-agenda-overriding-header "Silo")
           (org-agenda-files '(,(concat qz/org-agenda-directory "someday.org")
                               ,(concat qz/org-agenda-directory "projects.org")
                               ,(concat qz/org-agenda-directory "next.org")))))
    (todo "TODO"
          ((org-agenda-overriding-header "Projects")
           (org-agenda-files '(,(concat qz/org-agenda-directory "projects.org")))))
    (todo "TODO"
          ((org-agenda-overriding-header "One-off Tasks")
           (org-agenda-files '(,(concat qz/org-agenda-directory "next.org")))
           (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))))

(defun qz/org-agenda-todo ()
  (interactive)
  (org-agenda nil "t"))



(map! :map org-agenda-mode-map
      "J" #'qz/org-agenda-process-inbox
      "C-j" #'qz/org-agenda-process-item
      "R" #'org-agenda-refile)

(setq org-agenda-bulk-custom-functions '((?b . #'qz/org-agenda-process-item)))

(defun qz/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (org-agenda-bulk-mark-regexp "inbox:")
  (org-agenda-bulk-action ?b))


(defun qz/org-agenda-process-item ()
  "Process a single item in the org-agenda."
  (interactive)
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (org-agenda-refile nil nil t)))



(setq org-tag-alist '(("@errand" . ?e)
                      ("@work" . ?w)
                      ("@home" . ?h)
                      (:newline)
                      ("emacs" . ?E)
                      ("wip" . ?W)
                      ("CANCELLED" . ?c)
                      (:newline)
                      (:newline)
                      ("book" . ?b)
                      ("article" . ?a)
                      ("paper" . ?p)
                      (:newline)
                      (:newline)
                      ("talk" . ?t)
                      ("film" . ?f)))

(require 'ox-reveal)

(setq gnus-secondary-select-methods '((nntp "list.postgres.org")))

                                        ;(require 'orderless)
                                        ;(setq completion-styles '(orderless))
                                        ;(icomplete-mode) ; optional but recommended!
                                        ;
                                        ;(setq orderless-component-separator "[ &]")
                                        ;(setq company-idle-delay 0.1
                                        ;      company-minimum-prefix-length 1)
                                        ;
                                        ; highlight matching parcnfts
(defun just-one-face (fn &rest args)
  (let ((orderless-match-faces [completions-common-part]))
    (apply fn args)))

(advice-add 'company-capf--candidates :around #'just-one-face)

(use-package! orderless
  :config
  (after! ivy
    (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))))

(setq qz/org-agenda-prefix-length 20
      org-agenda-prefix-format
      '((agenda . " %i %(vulpea-agenda-category qz/org-agenda-prefix-length)%?-12t% s")
        (todo . " %i %(vulpea-agenda-category qz/org-agenda-prefix-length) ")
        (tags . " %i %(vulpea-agenda-category qz/org-agenda-prefix-length) ")
        (search . " %i %(vulpea-agenda-category qz/org-agenda-prefix-length) ")))

(defun vulpea-agenda-category (&optional len)
  "Get category of item at point for agenda.

Category is defined by one of the following items:
- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (car-safe (org-roam--extract-titles-title)))
         (category (org-get-category))
         (result
          (or (if (and
                   title
                   (string-equal category file-name))
                  title
                category)
              "")))
    (if (numberp len)
        (s-truncate len (s-pad-right len " " result))
      result)))
