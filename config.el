(setq user-full-name "Samuel Culpepper"
      user-mail-address "samuel@samuelculpepper.com")

(cond
  ((string-equal system-name "qzdl") (setq qz/font-default 32))
  (t (setq qz/font-default 16)))

  (map! "<mouse-8>" 'better-jumper-jump-backward)
  (map! "<mouse-9>" 'better-jumper-jump-forward)

(map! "C-?" #'undo-redo)

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

(defvar qz/buffer-popup-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (set-keymap-parent kmap text-mode-map)
    (define-key kmap (kbd "C-c C-c") #'qz/buffer-popup-commit)
    (define-key kmap (kbd "C-c C-k") #'qz/buffer-popup-abort)
    kmap))

(defcustom qz/buffer-popup-window-config
  '(+popup-display-buffer-stacked-side-window-fn)
  ;;  '((display-buffer-reuse-window display-buffer-split-below-and-attach)    (inhibit-same-window . t) (window-height . 0.25))
  "adjust the behaiour of the popup window

totally stolen from <link-to-elisp-doc 'pdf-annot-edit-contents-display-buffer-action>'")

(define-minor-mode qz/buffer-popup-minor-mode
  "Active when editing the contents of qz/buffer-popup."
  nil nil nil
  (when qz/buffer-popup-minor-mode
    (message "%s"
             (substitute-command-keys
              "Press \\[qz/buffer-popup-commit] to commit your changes, \\[qz/buffer-popup-abort] to abandon them."))))

(put 'qz/buffer-popup-minor-mode 'permanent-local t)



;; FIXME make this better for general shit
(defun qz/buffer-popup-finalize (save? &optional kill backfill)
  (setq qz/buffer-popup-last-value
        (cond
         ((and kill backfill) backfill)
         (t (with-current-buffer qz/buffer-popup-current-or-last (buffer-string)))))
  (dolist (win (get-buffer-window-list))
    (quit-window t win))
  (if qz/buffer-popup-final
      (funcall qz/buffer-popup-final))
  (message "%s" qz/buffer-popup-last-value))

(defun qz/buffer-popup-commit ()
  (interactive)
  (qz/buffer-popup-finalize t))

(defun qz/buffer-popup-abort ()
  (interactive)
  (qz/buffer-popup-finalize nil t))


(defun qz/buffer-popup-create ()
  (interactive)
  (select-window
   (display-buffer
    (with-current-buffer (get-buffer-create
                          (format "*Edit stuff %s*"
                                  (buffer-name)))
      (qz/buffer-popup-minor-mode 1)
      (org-mode)
      (setq qz/buffer-popup-current-or-last (current-buffer)))
    qz/buffer-popup-window-config))
  qz/buffer-popup-current-or-last)

(defun qz/insert-var ()
  (interactive)
  (completing-read
   (format-prompt "Describe variable" (and (symbolp (variable-at-point) (variable-at-point)))
                  #'help--symbol-completion-table
                  (lambda (vv)
                    ;; In case the variable only exists in the buffer
                    ;; the command we switch back to that buffer before
                    ;; we examine the variable.
                    (with-current-buffer orig-buffer
                      (or (get vv 'variable-documentation)
                          (and (boundp vv) (not (keywordp vv))))))
                  t nil nil
                  (if (symbolp v) (symbol-name v)))))

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

;;  (org-noter-insert-note (org-noter--get-precise-info))
;; ~read-event~ is cool -> org-noter--get-precise-info

(defun qz/event-line-offset ()
  "testing click at point functions'"
  (interactive)
  (message (number-to-string (cdr (posn-col-row  (event-start  (read-event "Click!")))))))

(defun qz/thing-at-point ()
  (interactive)
    (cdr (posn-col-row
      (let* ((m (mouse-pixel-position))
             (xy (cdr m)))
        (posn-at-x-y (car xy) (cdr xy) (car m))))))

;;(message (number-to-string (car (posn-col-row (posn-at-point (point)))))))

;;(map! "C-<down-mouse-1>" #'qz/thing-at-point)

;;'(#<window 832 on config.org> ; window
;;  5080        ; area-or-pos
;;  (413 . 966) ; (x . y)
;;  0           ; timestamp
;;  nil         ; object
;;  5080        ; pos
;;  (41 . 50)   ; (col . row)
;;  nil         ; image
;;  (333 . 16)  ; (dx . dy)
;;  (10 . 19))  ; (width . height)

(defun qz/org-noter--get-precise-info ()
                                        ;(org-noter--with-valid-session
  (let ((window (org-noter--get-doc-window))
        (mode (org-noter--session-doc-mode session))
        event)
    (with-selected-window window
      (while (not (and (eq 'mouse-1 (car event))
                       (eq window (posn-window (event-start event)))))
        (setq event (read-event "Click where you want the start of the note to be!")))
      (cond
       ((run-hook-with-args-until-success 'org-noter--get-precise-info-hook mode))

       ((eq mode 'pdf-view-mode)
        (if (pdf-view-active-region-p)
            (cadar (pdf-view-active-region))
          (org-noter--conv-page-scroll-percentage
           (+ (window-vscroll)
              (cdr (posn-col-row (event-start event)))))))

       ((eq mode 'doc-view-mode)
        (org-noter--conv-page-scroll-percentage
         (+ (window-vscroll)
            (cdr (posn-col-row (event-start event))))))

       ((eq mode 'nov-mode)
        (if (region-active-p)
            (min (mark) (point))
          (posn-point (event-start event))))))));)

;;(qz/org-noter--get-precise-info)

(setq doom-font (font-spec :family "monospace" :size qz/font-default))
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

(defun qz/exwm-hdmi-ultrawide ()
  (setq exwm-randr-workspace-monitor-plist '(0 "HDMI-1"))
  (add-hook
   'exwm-randr-screen-change-hook
   (lambda ()
     (start-process-shell-command
      "xrandr" nil
      "xrandr --output eDP-1 --off --output DP-1 --off --output HDMI-1 --primary --mode 5120x1440 --pos 0x0 --rotate normal --output DP-2 --off --output HDMI-2 --off")))
  (exwm-randr-enable))

        
(qz/exwm-hdmi-ultrawide)
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
 ("s-a" . qz/org-agenda-gtd))

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

(defun qz/noter-create-precise ()
  (interactive)
  (org-noter-insert-note (qz/get-precise)))


(defun qz/noter-create-precise-buffer-popup ()
  (interactive)
  (setq qz/org-noter-buffer (current-buffer)
        qz/precise-pos (qz/get-precise)
        qz/buffer-popup-final
        (lambda ()
          (message "yeet")
          (with-current-buffer qz/org-noter-buffer
            (qz/org-noter-insert-note qz/buffer-popup-last-value
                                      qz/precise-pos))))
  (qz/buffer-popup-create))

(defun qz/get-precise ()
  (interactive)
  (let ((v   (org-noter--conv-page-scroll-percentage
      (+ (window-vscroll)
         (qz/thing-at-point))))
)
(message "%s" v)
v))


(map! :mode pdf-sync-minor-mode
      "C-<mouse-1>" #'qz/noter-create-precise-buffer-popup)
(map!
      "C-<mouse-1>" #'qz/noter-create-precise-buffer-popup)

(map! :mode pdf-view-mode
      "h" #'pdf-annot-add-highlight-markup-annotation)

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
        "M-p" #'outline-previous-visible-heading
        "C->" #'org-do-demote
        "C-<" #'org-do-promote)
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

(add-hook 'org-mode-hook 'org-fragtog-mode)

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
        ("n" "now, as in NOW" entry (file ,(concat qz/org-agenda-directory "wip.org"))
         "* TODO [#A1] %? \nDEADLINE: %T\nCREATED: %u")
        ;; fire directly into inbox
        ("c" "org-protocol-capture" entry (file ,(concat qz/org-agenda-directory "inbox.org"))
         "* TODO [[%:link][%:description]]\n\n %i"
         :immediate-finish t)
        ("w" "Weekly Review" entry
         (file+olp+datetree ,(concat qz/org-agenda-directory "reviews.org"))
         (file ,(concat qz/org-agenda-directory "templates/weekly_review.org")))
        ("r" "Reading" todo ""
         ((org-agenda-files '(,(concat qz/org-agenda-directory "reading.org")))))))

(advice-add
 #'org-capture :around
 (lambda (fun &rest args)
   (letf! ((#'+org--restart-mode-h #'ignore))
     (apply fun args))))

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
         :head "#+title: <%<%Y-%m-%d>>\n#+roam_tags: daily private\n\n")))

(defun qz/org-roam-migrate-jobs ()
  (interactive )
  (dolist (file (org-roam--list-all-files))
                                        ;(message "processing %s" file)
    (with-current-buffer (or (find-buffer-visiting file)
                             (find-file-noselect file))
      ;; TODO = project
      (vulpea-project-update-tag)
      ;; making things private
                                        ;      (when (qz/should-be-private-p file)
                                        ;       (qz/org-roam-make-private))
      (save-buffer))))

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
               (qz/org-roam-add-tag tag t))))))

(defun vulpea-tags-add ()
  "Add a tag to current note."
  (interactive)
  (when (org-roam-tag-add)
    (vulpea-ensure-filetag)))

(defun qz/roam-dispatch-person (title)
  "add tag to headline for PERSON"
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
          (setq tags (cons "project" tags))
        (setq tags (remove "project" tags)))
      (if (qz/private-p)
          (setq tags (cons "private" tags))
        (setq tags (remove "private" tags)))
      (unless (eq prop-tags tags)
        (org-roam--set-global-prop
         "ROAM_TAGS"
         (combine-and-quote-strings (seq-uniq tags)))))))

(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (interactive)
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
     :where (like tags (quote "%\"project\"%"))])))

(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files
        (seq-uniq
         (append qz/org-agenda-files (vulpea-project-files)))))



(advice-add 'org-agenda :before #'vulpea-agenda-files-update)

(qz/pprint    (append qz/org-agenda-files (vulpea-project-files)))

(add-hook 'find-file-hook #'vulpea-project-update-tag)
(add-hook 'before-save-hook #'vulpea-project-update-tag)
(add-hook 'before-save-hook #'vulpea-ensure-filetag)

(defun qz/title-to-tag (title)
  "Convert TITLE to tag."
  (if (equal "@" (subseq title 0 1))
      title
    (concat "@" (s-replace " " "" title))))

(defun qz/private-p ()
  (interactive)
  (let ((title (+org--get-property "title")))
                                        ;(message (concat "...checking privateness of " title))
    (if (not title)
        (message "WARNING: unable to evaluate privateness; file [" file "] has no title")
      (or (string-match-p ".?[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}.?" title)
          (string-match-p "meeting" title)
          (qz/org-roam-has-link-to-p title "thinkproject")))))

(defun qz/should-be-private-p (file)
  (with-current-buffer (or (find-buffer-visiting file)
                           (find-file-noselect file))
    (qz/private-p)))

(defun qz/is-file-private ()
  (interactive)
  (message (concat "should " (f-this-file) " be private..? "
                   (or (and (qz/should-be-private-p (f-this-file)) "yes") "no"))))

(defun qz/org-roam-private-files ()
  "Return a list of note files containing tag =private="
  (seq-map
   #'car
   (org-roam-db-query
    [:select file
     :from tags
     :where (like tags (quote "%\"private\"%"))])))

;(qz/pprint (qz/org-roam-private-files))

(defun qz/has-link-to (src dst)
  (org-roam-db-query
   [:select source
    :from links
    :where (and (= dest $r1)
                (= source $r2))]
   src dest))

(defun qz/has-link (a b)
  (seq-map
   #'car
   (org-roam-db-query
    [:select [source dest]
     :from links
     :where (or (and (= dest a) (= source b))
                (and (= dest b) (= source a)))])))

                                        ;(org-roam-db-query
                                        ; [:select *
                                        ;  :from links
                                        ;  :where (and (= dest $r1)
                                        ;              (= source $r2))]
                                        ; "/home/qzdl/life/roam/20200401201707-thinkproject.org"
                                        ; "/home/qzdl/life/roam/20210311T113202Z-chris_heimann.org")
                                        ;
                                        ;(qz/has-link-to
                                        ; "/home/qzdl/life/roam/20210311T113202Z-chris_heimann.org"
                                        ; "/home/qzdl/life/roam/20200401201707-thinkproject.org"))

(org-roam-db-query
    [:select *
     :from links
     :limit 10])

(defun qz/org-roam-make-private ()
  (interactive)
  (qz/org-roam-add-tag "private" t))

(defun qz/org-roam-has-link-to-p (source dest)
  """TODO implement; returns t/nil if source links to dest"
  nil)

(defun qz/org-roam-add-global-prop (prop val)
  (org-roam--set-global-prop
   prop
   (combine-and-quote-strings
    (seq-uniq
     (cons val (split-string-and-unquote
                (or (+org--get-property prop) "")))))))

(defun qz/org-roam-add-tag (tag &optional filetag_too)
  (qz/org-roam-add-global-prop "roam_tags" tag)
  (when filetag_too
    (qz/org-roam-add-global-prop "filetags" tag)))

(defun qz/get-headline-path (&optional self? reverse? sepf)
  (interactive)
  (let* ((s (or sepf
                (lambda (i)
                  (if (< 0 i) " -> " ""))))
         (c (org-get-outline-path self?)))
    (insert "\n")
    (cl-loop
     for e in (if reverse? (reverse c) c)
     for i = 0 then (1+ i)
     do (insert (funcall s i) e))))

(defun qz/headline-mm (i)
  (if (< 0 i)
      (concat "\n" (make-string i ?	)) ""))

;; example, for https://tobloef.com/text2mindmap/ + [[file:../../../life/roam/20210521T102710Z-the_great_ideas_vol_ii.org][The Great Ideas, Vol II]]
;;(qz/get-headline-path t t 'qz/headline-mm)


;; (progn
;;   (qz/get-headline-path)
;;   (qz/get-headline-path nil t)
;;   (qz/get-headline-path t)
;;   (qz/get-headline-path t t)
;;   (qz/get-headline-path t t 'qz/headline-mm)
;;   (qz/get-headline-path t nil 'qz/headline-mm)
;;   (qz/get-headline-path nil nil 'qz/headline-mm))

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

(add-to-list
 'org-agenda-custom-commands
 '("ms" "shopping" tags "buy"))

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



(setq org-tag-alist
      '(("@errand" . ?e)
        ("@work" . ?w)
        ("@home" . ?h)
        ("@blog" . ?B)
        (:newline)
        ("emacs" . ?E)
        ("wip" . ?W)
        ("CANCELLED" . ?c)
        (:newline)
        ("learning" . ?l)
        ("research" . ?r)
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

(require 'mathpix)
(setq mathpix-screenshot-method "scrot -s %s")

(map! "C-c o m" #'qz/mathpix-screenshot)
;; add var capture
(defun qz/mathpix-screenshot ()
  "Capture screenshot and send result to Mathpix API."
  (interactive)
  (let
      ((default-directory "~"))
    (make-directory
     (file-name-directory mathpix-screenshot-file)
     t)
    (if
        (functionp mathpix-screenshot-method)
        (funcall mathpix-screenshot-method mathpix-screenshot-file)
      (shell-command-to-string
       (format mathpix-screenshot-method mathpix-screenshot-file)))
    (if
        (file-exists-p mathpix-screenshot-file)
        (progn
          (insert
           (setq mathpix-last-result
                 (mathpix-get-result mathpix-screenshot-file)))
          (delete-file mathpix-screenshot-file)))))

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
