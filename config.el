(defun dice ()
  (interactive)
  (let ((n (1+ (random* 6))))
    (message "%s" n)
     n))

(setq user-full-name "Samuel Culpepper"
      user-mail-address "samuel@samuelculpepper.com")
(setq qz/capture-title-timestamp-roam "%(qz/utc-timesftamp)-${slug}.org")

(cond
  ((string-equal system-name "qzdl") (setq qz/font-default 32))
  (t (setq qz/font-default 16)))

(setq epg-gpg-program "gpg")

(load-file "~/.doom.d/private/authinfo.el")

  (map! "<mouse-8>" 'better-jumper-jump-backward)
  (map! "<mouse-9>" 'better-jumper-jump-forward)

(map! "C-z" #'+default/newline-above)

(map! "M-z" #'zap-up-to-char) ;; like dt<CHAR> in vim
(map! "C-x C-z" #'zap-up-to-char)

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

(map! "s-i" #'qz/roam-capture-todo)

(map! "s-=" #'er/expand-region)

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
         (t (with-current-buffer qz/buffer-popup-current-or-last
              (buffer-substring-no-properties (point-min) (point-max))))))
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

(defun qz/contract-file-name (file)
  "turn an objective path to a relative path to homedir `~/`"
  (replace-regexp-in-string(expand-file-name "~/") "~/" file))

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

(defun qz/bt-headphone-off ()
  (interactive)
  (async-shell-command "bluetoothctl disconnect 2C:41:A1:87:20:BA"))

(defun qz/bt-headphone-on ()
  (interactive)
  (async-shell-command "bluetoothctl connect 2C:41:A1:87:20:BA"))

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

(server-start)

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


(defun qz/exwm-hdmi-tv ()
  (setq exwm-randr-workspace-monitor-plist '(0 "HDMI-1"))
  (add-hook
   'exwm-randr-screen-change-hook
   (lambda ()
     (start-process-shell-command
      "xrandr" nil
      "xrandr --output eDP1 --off --output DP1 --off --output DP2 --off --output HDMI1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --scale 2x2 --output HDMI2 --off --output VIRTUAL1 --off")))
  (exwm-randr-enable))


(cond
  ((string-equal system-name "qzdl") (qz/exwm-hdmi-tv))
  (t (qz/exwm-usbc-ultrawide)))
(exwm-enable)
(exwm-init)

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

(defun qz/exwm-input--update-global-prefix-keys ()
  "an interactive wrapper to rebind with `exwm-input--update-global-prefix-keys'"
  (interactive)
  (exwm-input--update-global-prefix-keys))

(with-eval-after-load (exwm-input--update-global-prefix-keys))

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

(defcustom qz/exwm-floating-window-classes '("keybase" "mpv")
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

(require 'nano-layout)
(require 'nano-theme-dark)

;; Theme
(require 'nano-faces)
(nano-faces)

(require 'nano-theme)
(nano-theme)

;; Nano default settings (optional)
;; (require 'nano-defaults)

;; Nano session saving (optional)
;(require 'nano-session)

;; Nano header & mode lines (optional)
(require 'nano-modeline)

;; Nano key bindings modification (optional)
;;(require 'nano-bindings)

;; Compact layout (need to be loaded after nano-modeline)
;(require 'nano-compact)

;; Nano counsel configuration (optional)
;; Needs "counsel" package to be installed (M-x: package-install)
(require 'nano-counsel)

;; Help (optional)
(require 'nano-help)

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

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

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
;(load-theme 'modus-vivendi t)

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
(setq perfect-margin-ignore-regexps nil
      perfect-margin-ignore-filters nil)

(defun qz/set-message-filter ()
  (interactive)
  (defun message-filter-center (args)
    "Center message string.
  This is a :filter-args advice for `message`."
    (set-window-margins (minibuffer-window) 0)
    (set-window-margins
     (minibuffer-window)
     (if (not (car args))
         (/ 2 (frame-width)) ;; a new 'default' echo position
       (max 0 (/ (- (window-width (minibuffer-window))
                    (string-width (car args)))
                 2))))
    args)) ;; allow regular args to be passed

(defun qz/reset-message-filter ()
  (interactive)
  (defun message-filter-center (args) args))

(advice-add #'message :filter-args #'message-filter-center)
(qz/set-message-filter)

;; Anselm

(map! :leader
      (:prefix-map ("n" . "notes")
       (:prefix-map ("d" . "by date")
          :desc "Arbitrary date" "d" #'org-roam-dailies-date
          :desc "Today"          "t" #'org-roam-dailies-today
          :desc "Tomorrow"       "m" #'org-roam-dailies-tomorrow
          :desc "Yesterday"      "y" #'org-roam-dailies-yesterday)
       "C-c" #'org-capture
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

(let ((f (expand-file-name "~/.roswell/helper.el")))
  (when (file-exists-p f)
      (load f)))

(define-key! emacs-lisp-mode-map "C-c C-c" 'eval-defun)

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
                                   (lisp . t)
                                   (jupyter . t)
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
                                       ("jp" . "src jupyter-python")
                                       ("jr" . "src jupyter-R")
                                       ("sr" . "src R")
                                       ("el" . "src emacs-lisp")))
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'proselint 'org-mode)))

(require 'org-id)
(setq org-id-track-globally t)

(setq org-file-apps
      '((auto-mode . emacs)
        (directory . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . emacs)))

(eval-after-load nil
  (remove-hook 'org-mode-hook #'ob-ipython-auto-configure-kernels))

(if (boundp 'org-headline-done) (set-face-attribute 'org-headline-done nil :strike-through t))

(defun qz/definer-headliner (s) "civiliser 85er" (concat "<<<.*-" s ">>>.*:" s ":"))

(defun qz/get-radio-naked (r)
  (car (split-string (cadr (split-string r "<<<")) ">>>")))

;;(qz/get-radio-naked "<<<data>>>")

(defun qz/insert-radio-children ()
  "fucking awesome"
  (interactive)
  (org-with-wide-buffer
   (org-back-to-heading t)
   (let* ((case-fold-search nil)
          (pt (point)))
     (when (looking-at org-complex-heading-regexp)
       (let ((relate (qz/get-radio-naked (match-string-no-properties 4))))
         (message "relating: %s" relate)
         (mapc (lambda (s) (save-excursion (message "inserting subheading %s" s) (org-insert-subheading nil) (insert s) s))
               (mapcar #'qz/get-radio-naked
                       (qz/matches-in-buffer (message (qz/definer-headliner relate))))))))))

;; (defun qz/matches-in-buffer (regexp &optional buffer with-point?)
;;   "return a list of matches of REGEXP in BUFFER or the current buffer if not given."
;;   (let ((matches))
;;     (save-match-data
;;       (save-excursion
;;         (with-current-buffer (or buffer (current-buffer))
;;           (save-restriction
;;             (widen)
;;             (goto-char 1)
;;             (while (search-forward-regexp regexp nil t 1)
;;               (let ((s (match-string-no-properties 0))
;;                     (push (if with-point? (cons s (point)) s) matches)))))))
;;       matches)))

;; (defun qz/matches-in-buffer (regexp &optional buffer with-point)
;;   "return a list of matches of REGEXP in BUFFER or the current buffer if not given."
;;   (let ((matches nil))
;;     (save-match-data
;;       (save-excursion
;;         (with-current-buffer (or buffer (current-buffer))
;;           (save-restriction
;;             (widen)
;;             (goto-char 1)
;;             (while (search-forward-regexp regexp nil t 1)
;;               (let ((s (match-string-no-properties 0)))
;;                 (push (if with-point (cons s (point)) s) matches)))))))
;;     matches))

(setq org-directory "~/life/"
      qz/notes-directory (concat org-directory "roam/")
      qz/org-agenda-directory qz/notes-directory
      qz/org-agenda-files (mapcar (lambda (f) (expand-file-name (concat qz/notes-directory f)))
                                  '("calendar-home.org" "calendar-work.org" "schedule.org"))
      org-ref-notes-directory qz/notes-directory
      bibtex-completion-notes-path qz/notes-directory
      org-ref-bibliography-notes "~/life/bib.org"
      org-noter-notes-search-path (list qz/notes-directory)
      org-roam-directory qz/notes-directory
      org-roam-dailies-directory (concat qz/notes-directory "daily/"))

(require 'org-fragtog)
(add-hook 'org-mode-hook 'org-fragtog-mode)

(require 'org-auto-tangle)
(add-hook 'org-mode-hook 'org-auto-tangle-mode)

(defvar qz/agenda-daily-files nil)

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
  (org-agenda nil "g")
  (goto-char (point-min))
  (org-agenda-goto-today))

(setq org-agenda-custom-commands nil)
(add-to-list
 'org-agenda-custom-commands
 `("g" "GTD"
   ((agenda "" ((org-agenda-span 'day) (org-deadline-warning-days 60)))
    (tags-todo "wip"
               ((org-agenda-overriding-header "wip")))
    (todo "TODO"
          ((org-agenda-overriding-header "to process")
           (org-agenda-files '(,(concat qz/org-agenda-directory "inbox.org")))))
    (todo "TODO"
          ((org-agenda-overriding-header "daily inbox")
           (org-agenda-files qz/agenda-daily-files)))
    (todo "TODO"
          ((org-agenda-overriding-header "emails")
           (org-agenda-files '(,(concat qz/org-agenda-directory "emails.org")))))
    (todo "TODO"
          ((org-agenda-overriding-header "one-off Tasks")
           (org-agenda-files '(,(concat qz/org-agenda-directory "next.org"))))))))
    ;;        (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))

(qz/pprint org-agenda-custom-commands)

 (defun +org-defer-mode-in-agenda-buffers-h ()
      "`org-agenda' opens temporary, incomplete org-mode buffers.
I've disabled a lot of org-mode's startup processes for these invisible buffers
to speed them up (in `+org--exclude-agenda-buffers-from-recentf-a'). However, if
the user tries to visit one of these buffers they'll see a gimped, half-broken
org buffer. To avoid that, restart `org-mode' when they're switched to so they
can grow up to be fully-fledged org-mode buffers."
      (dolist (buffer org-agenda-new-buffers)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (add-hook 'doom-switch-buffer-hook #'+org--restart-mode-h
                      nil 'local)))))

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

(defun qz/org-agenda-process-item ()
  "Process a single item in the org-agenda."
  (interactive)
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (org-agenda-refile nil nil t)))

(defun qz/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (org-agenda-bulk-mark-regexp "inbox:")
  (org-agenda-bulk-action ?b))

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

(setq org-refile-targets '(("next.org" :level . 0)
                           ("reading.org" :level . 0)
                           ("watching.org" :level . 0)
                           ("learning.org" :level . 0)
                           ("inbox.org" :level . 0)
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
                                   (jupyter . t)
                                   (lisp . t)
                                   (python . t)
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
                                       ("sp" . "src psql")
                                       ("sr" . "src R")
                                       ("ss" . "src ")
                                       ("jp" . "src jupyter-python")
                                       ("jr" . "src jupyter-R")
                                       ("el" . "src emacs-lisp")))
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'proselint 'org-mode)))

(add-hook 'org-mode-hook 'org-fragtog-mode)

(require 'org-habit)

(require 'org-auto-tangle)
(add-hook 'org-mode-hook 'org-auto-tangle-mode)

(require 'org-recoll)

(setq org-recoll-command-invocation "recollq -t -A"
      org-recoll-results-num 100)

(map! "C-c g" #'org-recoll-search)
(map! "C-c u" #'org-recoll-update-index)
(map! :mode org-recoll-mode "q" #'kill-this-buffer)

(require 'org-protocol)

(require 'org-capture)

(setq qz/capture-title-timestamp "%(qz/utc-timestamp)-${slug}")

;; ORG ROAM BREAKS COMPAT WITH ORG CATURE BY REQUIRING '.ORG' IN FILE

(setq org-capture-templates
      `(("i" "inbox" entry
         (file ,(concat qz/org-agenda-directory "inbox.org"))
         "* TODO %? \nCREATED: %u\nFROM: %a")
        ;; spanish language capturing
        ("v" "vocab; spanish" entry
         (file+headline ,(concat qz/notes-directory "spanish_language.org") "vocab, phrases")
         "** \"%?\" :es:\nFROM: %a\n\n*** :en:\n")
        ;; capture link to live `org-roam' thing
        ("n" "now, as in NOW" entry (file ,(concat qz/org-agenda-directory "wip.org"))
         "* TODO [#A1] %? \nDEADLINE: %T\nCREATED: %u")
        ;; fire directly into inbox
        ("c" "org-protocol-capture" entry (file ,(concat qz/org-agenda-directory "inbox.org"))
         "* TODO [[%:link][%:description]]\nCREATED: %u\n\n#+begin_quote\n\n%i\n\n#+end_quote"
         :immediate-finish t)
        ;; push last captured item into inbox
        ("l" "last-capture" entry (file ,(concat qz/org-agenda-directory "inbox.org"))
         (function qz/inbox-last-captured)
         :immediate-finish t)
        ("I" "current-roam" entry (file ,(concat qz/org-agenda-directory "inbox.org"))
         (function qz/current-roam-link)
         :immediate-finish t)
        ("t" "tangent" entry (file ,(concat org-roam-dailies-directory (format-time-string "private-%Y-%m-%d.org")))
         "* TANGENT [%<%H:%M>] %?\nCREATED: %u\nFROM: %a")
        ("w" "weekly review" entry
         (file+datetree ,(concat qz/org-agenda-directory "reviews.org"))
         (file ,(concat qz/org-agenda-directory "templates/weekly_review.org")))))

(defun qz/inbox-last-captured (&optional buffer)
  (interactive)
  (when-let ((b (or (and org-capture-last-stored-marker
                         (marker-buffer org-capture-last-stored-marker))
                    buffer)))
    (with-current-buffer b
      (org-goto-marker-or-bmk org-capture-last-stored-marker)
      (concat "* [[id:" (org-roam-id-at-point)  "][" (qz/node-title) "]]"))))

(defun qz/capture-last-captured ()
  (interactive)
  (when-let ((b (and org-capture-last-stored-marker
                     (marker-buffer org-capture-last-stored-marker))))
    (if (with-current-buffer b
          (not (string-equal "inbox" (qz/node-title))))
        (org-capture nil "l")
      (message "qz/capture-last-captured: skipping; last capture was inbox"))))

(advice-add
 #'org-capture :around
 (lambda (fun &rest args)
   (letf! ((#'+org--restart-mode-h #'ignore))
     (apply fun args))))

;; helper capture function for `org-roam' for `agenda-mode'
(defun qz/current-roam-link ()
  (interactive)
  "Get link to org-roam file with title"
  (concat "* TODO [[" (buffer-file-name) "][" (qz/node-title) "]]"))

(defun qz/org-inbox-capture ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "i"))

(defun qz/org-daily-tangent-capture ()
  (interactive)
  "Capture the inevitable tangent"
  (org-capture nil "t"))

(defun qz/org-roam-capture-current ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "I"))

(defun qz/roam-capture-todo ()
  (interactive)
  "Capture a task in agenda mode."
  (destructuring-bind (thing region) (qz/thing-at-point-or-region-and-region)
    (org-roam-capture- :goto t
                       :keys "n"
                       :node (org-roam-node-create :title thing)
                       :props `(:immediate-finish t :jump-to-captured nil
                                :region ,region     :insert-at ,(point-marker)
                                :finalize 'insert-link))
    (qz/capture-last-captured)))

(defun qz/org-roam-has-link-to-p (source dest)
  "TODO implement; returns t/nil if source links to dest"
  nil)

(ignore-errors
  (setq org-gcal-fetch-file-alist
      `((,qz/calendar-home . ,(concat qz/notes-directory "calendar-home.org"))
        (,qz/calendar-work . ,(concat qz/notes-directory "calendar-work.org"))
        (,qz/calendar-shared . ,(concat qz/notes-directory "calendar-shared.org"))))
  (qz/pprint org-gcal-fetch-file-alist))

(setq org-gcal-recurring-events-mode 'nested)

(defun qz/format-link-from-title (title)
  (let ((file (org-roam-link--get-file-from-title title)))
    (and file (org-roam-format-link file title))))

(defun qz/roam-auto-youtube-video ()
  (interactive)
  (let ((key (+org--get-property "roam_key")))
    (when (string-match "YouTube" key)
      (let*
        ((channel+url (qz/roam-key->yt-channel key))
         (link (qz/format-link-from-title (first channel+url)))
         (channel (or link (concat "[[roam:" (first channel+url) "]]")))
         (str
          (concat "A "
                  (qz/format-link-from-title "YouTube") " "
                  (qz/format-link-from-title "video") " from "
                  channel)))
        (insert str)))))

(defun qz/roam-key->yt-channel (key)
  (let ((str (shell-command-to-string
              (concat
               "youtube-dl " key "--skip-download --dump-json | "
               "python -c 'import sys;import json;b=json.loads(sys.stdin.read());print(b.get(\"channel\")+\",\"+b.get(\"channel_url\"))'"))))
    (message str)
    (and str (split-string str ","))))

;(qz/roam-key->yt-channel "https://www.youtube.com/watch?v=KopB4l5QkEg")

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

(use-package! org-roam
  :after org
  :commands
  (org-roam-buffer
   org-roam-setup
   org-roam-capture
   org-roam-node-find)
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam-buffer-toggle
        :desc "org-roam: capture entry today" "j" #'org-roam-dailies-capture-today
        :desc "org-roam: go to today" "J" #'org-roam-dailies-find-today
        :desc "org-roam: go to today" "C-j" #'qz/org-daily-tangent-capture
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "qz/org-roam-immediate-node-insert" "C-i" #'qz/org-roam-immediate-node-insert
        :desc "org-roam-node-find" "f" #'org-roam-node-find)
  :config
  (setq org-roam-mode-sections
       (list #'org-roam-backlinks-insert-section
             #'org-roam-reflinks-insert-section
             #'org-roam-unlinked-references-insert-section))
  (org-roam-setup))
(use-package! org-roam-protocol
  :after org-protocol)

(defun my/replace-file-with-id-link ()
  "Replaces file links with ID links where possible in current buffer."
  (interactive)
  (let (path desc)
    (org-with-point-at 1
      (while (re-search-forward org-link-bracket-re nil t)
        (setq desc (match-string 2))
        (when-let ((link (save-match-data (org-element-lineage (org-element-context) '(link) t))))
          (when (string-equal "file" (org-element-property :type link))
            (setq path (expand-file-name (org-element-property :path link)))
            (replace-match "")
            (insert (org-roam-format-link path desc))))))))

(defun qz/or-migrate-v2 ()
  (dolist (file (org-roam--list-all-files))
    (with-current-buffer (or (find-buffer-visiting file)
                             (find-file-noselect file))
      (org-with-point-at 1
        (org-id-get-create))
      (save-buffer)))
  (org-roam-db-sync)
  (dolist (file (org-roam--list-all-files))
    (with-current-buffer (or (find-buffer-visiting file)
                             (find-file-noselect file))
      (my/replace-file-with-id-link)
      (save-buffer)))
  (org-roam-db-sync))

;;(qz/or-migrate-v2)

(defun qz/or-migrate-filetags ()
  (dolist (file (seq-uniq
                 (append
                  (split-string-and-unquote
                   (shell-command-to-string "rg 'roam_tags' ~/life/roam -il"))
                  (split-string-and-unquote
                   (shell-command-to-string "rg 'filetags' ~/life/roam -il")))))
    (with-current-buffer (or (find-buffer-visiting file)
                             (find-file-noselect file))
      (org-with-point-at (point-min)
        (mapc (lambda (s)  (org-roam-tag-add s))
              (qz/get-old-tags)))
      (save-buffer)
      (kill-this-buffer))
    (shell-command-to-string "~/.local/bin/wrg '^\\#\\+roam_tags:.*' ~/life/roam/ -i --replace ''")))

(defun qz/get-old-tags ()
  (split-string-and-unquote
    (concat (+org-get-global-property "roam_tags")
            " "
            (+org-get-global-property "filetags"))))

;;(qz/or-migrate-filetags)

;;(length
;; (seq-uniq
;;  (append
;;   (split-string-and-unquote
;;    (shell-command-to-string "rg '^\\#\\+roam_tags: \\w' ~/life/roam/ -il"))
;;   (split-string-and-unquote
;;    (shell-command-to-string "rg 'filetags' ~/life/roam -l")))))

(setq qz/org-roam-capture-head "#+title: ${title}\n")
(setq qz/capture-title-timestamp-roam "%(qz/utc-timestamp)-${slug}.org")

(setq org-roam-capture-templates
      `(("d" "default" plain "%?"
         :if-new (file+head ,qz/capture-title-timestamp-roam
                            ,qz/org-roam-capture-head)
         :unnarrowed t)
        ("n" "empty" plain "%?"
         :if-new (file+head ,qz/capture-title-timestamp-roam
                            ,qz/org-roam-capture-head)
         :immediate-finish t)
        ))

(setq org-roam-capture-ref-templates
      `(("r" "ref" plain
         "%?"
         :if-new (file+head ,qz/capture-title-timestamp-roam
                            "#+title: ${title}\n")
         :unnarrowed t)))

(setq org-roam-dailies-capture-templates
      `(("d" "default" plain
         "* [%<%H:%M>] %?\nCREATED: %u\nFROM: %a"
         :if-new (file+head "private-%<%Y-%m-%d>.org"
                            "#+title: <%<%Y-%m-%d>>\n#+filetags: daily private\n\n"))))

(defun qz/point->roam-id (&optional pos)
  (org-roam-node-id (org-roam-node-at-point)))

(setq qz/daily-title-regexp ".?[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}.?")

(defun qz/title->roam-id (title)
  (org-roam-node-id (org-roam-node-from-title-or-alias title)))

(defun qz/node-title (&optional limit)
  (save-excursion
    (goto-char (org-roam-node-point (qz/org-roam-node-at-point 'assert limit)))
    (if (= (org-outline-level) 0)
        (cadr (car (org-collect-keywords '("title"))))
      (substring-no-properties (org-get-heading t t t)))))

;(qz/node-title)

(defun qz/node-tags ()
  (save-excursion
    (goto-char (org-roam-node-point (org-roam-node-at-point 'assert)))
    (if (= (org-outline-level) 0)
        (split-string-and-unquote (or (cadr (car (org-collect-keywords '("filetags")))) ""))
      (org-get-tags))))

;(qz/node-tags)

(defun qz/roam-immediate-insert ()
  (interactive)
  (qz/org-roam-node-insert nil t))

(defun qz/thing-at-point-or-region-and-region (&optional thing prompt)
  "Grab the current selection, THING at point, or xref identifier at point.

Returns THING if it is a string. Otherwise, if nothing is found at point and
PROMPT is non-nil, prompt for a string (if PROMPT is a string it'll be used as
the prompting string). Returns nil if all else fails.

NOTE: Don't use THING for grabbing symbol-at-point. The xref fallback is smarter
in some cases."
  (declare (side-effect-free t))
  (cond ((stringp thing)
         thing)
        ((doom-region-active-p)
         (cons (buffer-substring-no-properties (region-beginning) (region-end))
               (cons (region-beginning)
                     (region-end))))
        (thing
         (cons (thing-at-point thing t)
               (bounds-of-thing-at-point thing)))
        ((require 'xref nil t)
         ;; Eglot, nox (a fork of eglot), and elpy implementations for
         ;; `xref-backend-identifier-at-point' betray the documented purpose of
         ;; the interface. Eglot/nox return a hardcoded string and elpy prepends
         ;; the line number to the symbol.
         (let* ((val
                 (if (memq (xref-find-backend) '(eglot elpy nox))
                     (thing-at-point 'symbol t)
                   ;; A little smarter than using `symbol-at-point', though in most
                   ;; cases, xref ends up using `symbol-at-point' anyway.
                   (xref-backend-identifier-at-point (xref-find-backend)))))
           (cons val (bounds-of-thing-at-point 'symbol))))
        (prompt
         (read-string (if (stringp prompt) prompt "")))))



(destructuring-bind (a . (b . c)) '(1  . (2 . 3))
  (message "%s %s %s" a b c))

(defun qz/org-roam-node-insert (&optional filter-fn pass-thru)
  "Find an Org-roam file, and insert a relative org link to it at point.
Return selected file if it exists.
If LOWERCASE is non-nil, downcase the link description.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions."
  (interactive)
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* ((pt (qz/thing-at-point-or-region-and-region))
               (beg (set-marker (make-marker) (car (cdr pt))))
               (end (set-marker (make-marker) (cdr (cdr pt))))
               (region-text (org-link-display-format
                             (substring-no-properties (car pt))))
               (node (if pass-thru
                         (or (org-roam-node-from-title-or-alias region-text)
                             (org-roam-node-create :title region-text))
                       (org-roam-node-read region-text filter-fn)))
               (description (or (and node region-text (org-roam-node-title node))
                                region-text)))
          (if (org-roam-node-id node)
              (progn
                (when region-text
                  (delete-region beg end)
                  (set-marker beg nil)
                  (set-marker end nil))
                (insert (org-link-make-string
                         (concat "id:" (org-roam-node-id node))
                         description)))
            (funcall
              `(lambda ()
                 (org-roam-capture-
                  :node node
                  ,@(when pass-thru '(:keys "n")) ; ; [[id:bc3c61d4-d720-40a8-9018-6357f05ae85e][roam-capture-template]]
                  :props (append
                          (when (and beg end)
                            (list :region (cons beg end)))
                          (list :insert-at (point-marker)
                                :link-description description
                                :finalize 'insert-link))))))))
    (deactivate-mark)))

(defun qz/thing-at-point-or-region-and-region (&optional thing prompt)
  "Grab the current selection, THING at point, or xref identifier at point.

Returns THING if it is a string. Otherwise, if nothing is found at point and
PROMPT is non-nil, prompt for a string (if PROMPT is a string it'll be used as
the prompting string). Returns nil if all else fails.

NOTE: Don't use THING for grabbing symbol-at-point. The xref fallback is smarter
in some cases."
  (declare (side-effect-free t))
  (cond ((stringp thing)
         thing)
        ((doom-region-active-p)
         (cons (buffer-substring-no-properties (region-beginning) (region-end))
               (cons (region-beginning)
                     (region-end))))
        (thing
         (cons (thing-at-point thing t)
               (bounds-of-thing-at-point thing)))
        ((require 'xref nil t)
         ;; Eglot, nox (a fork of eglot), and elpy implementations for
         ;; `xref-backend-identifier-at-point' betray the documented purpose of
         ;; the interface. Eglot/nox return a hardcoded string and elpy prepends
         ;; the line number to the symbol.
         (let* ((val
                 (if (memq (xref-find-backend) '(eglot elpy nox))
                     (thing-at-point 'symbol t)
                   ;; A little smarter than using `symbol-at-point', though in most
                   ;; cases, xref ends up using `symbol-at-point' anyway.
                   (xref-backend-identifier-at-point (xref-find-backend)))))
           (cons val (bounds-of-thing-at-point 'symbol))))
        (prompt
         (read-string (if (stringp prompt) prompt "")))))

(defun qz/org-roam-immediate-node-insert ()
"Insert a node on `thing-at-point', corresponding to the thing, creating
if not existing"
  (interactive)
  (qz/org-roam-node-insert nil t))

(defun qz/org-roam-node-insert (&optional filter-fn pass-thru)
  "Find an Org-roam file, and insert a relative org link to it at point.
Return selected file if it exists.
If LOWERCASE is non-nil, downcase the link description.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions."
  (interactive)
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* ((pt (qz/thing-at-point-or-region-and-region))
               (beg (set-marker (make-marker) (car (cdr pt))))
               (end (set-marker (make-marker) (cdr (cdr pt))))
               (region-text (org-link-display-format
                             (substring-no-properties (car pt))))
               (node (if pass-thru
                         (or (org-roam-node-from-title-or-alias region-text)
                             (org-roam-node-create :title region-text))
                       (org-roam-node-read region-text filter-fn)))
               (description (or (and node region-text (org-roam-node-title node))
                                region-text)))
          (if (org-roam-node-id node)
              (progn
                (when region-text
                  (delete-region beg end)
                  (set-marker beg nil)
                  (set-marker end nil))
                (insert (org-link-make-string
                         (concat "id:" (org-roam-node-id node))
                         description)))
            (funcall
              `(lambda ()
                 (org-roam-capture-
                  :node node
                  ;; there could be a better way to reference the `immediate-finish'
                  ;; roam-capture-template here, but for demo it's just easier to splice in
                  ;; with a funny metaprogramming hack for conditional interning `:key "val"'
                  ,@(when pass-thru '(:keys "n"))  ; where "n" is an `immediate-finish' template
                  :props (append
                          (when (and beg end)
                            (list :region (cons beg end)))
                          (list :insert-at (point-marker)
                                :link-description description
                                :finalize 'insert-link))))))))
    (deactivate-mark)))

(defun qz/title-to-tag (title)
  "Convert TITLE to tag."
  (if (equal "@" (subseq title 0 1))
      title
    (concat "@" (s-replace " " "" title))))

(defun qz/hard-refresh-org-tags-in-buffer ()
  (interactive)
  (setq org-file-tags nil)      ; blast the cache
  (org-set-regexps-and-options) ; regen property detection regexp
  (org-get-tags))               ; write to cache

(defun qz/roam-get-node-by-tag (tag)
  (seq-map
   #'car
   (org-roam-db-query
    [:select :distinct file
     :from tags
     :inner :join nodes
     :on (= tags:node_id nodes:id)
     :where (= tags:tag tag)])))

(defun qz/should-be-private-p (file)
  (with-current-buffer (or (find-buffer-visiting file)
                           (find-file-noselect file))
    (qz/private-p)))

(defun qz/is-file-private ()
  (interactive)
  (message (concat "should " (f-this-file) " be private..? "
                   (or (and (qz/should-be-private-p (f-this-file)) "yes") "no"))))

(defun qz/project-files ()
  "Return a list of note files containing Project tag."
  (seq-map
   #'car
   (org-roam-db-query
    [:select :distinct file
     :from tags
     :inner :join nodes
     :on (= tags:node_id nodes:id)
     :where (= tags:tag "project")])))


;(qz/project-files)

(defun qz/agenda-daily-files-f ()
  (seq-filter (lambda (s) (string-match qz/daily-title-regexp s))
              org-agenda-files))
;(qz/agenda-daily-files-f)

(defun qz/org-roam-migrate-jobs ()
  (interactive )
  (dolist (file (org-roam--list-all-files))
    (with-current-buffer (or (find-buffer-visiting file)
                             (find-file-noselect file))
      (message "%s visiting" file)
      (qz/dispatch-hook)
      (save-buffer))))
;(qz/org-roam-migrate-jobs)

(defun qz/headline-add-tag (title)
  "add tag to headline for `title'"
  (save-excursion
    (ignore-errors
      (org-back-to-heading)
      (org-set-tags
       (seq-uniq
        (cons
         (qz/title-to-tag title)
         (org-get-tags nil t)))))))

(defun qz/ensure-tag (tagstring tag)
  "Apply `org-roam-tag-add' for `tag' to node with existing tags
`tagstring'

HACK: using `re-search-backward' to jump back to applicable
point (implicitly, `point-min' for file-level; :PROPERTIES: drawer for
entry); covering 'inherited match'.

this could be updated to jump back, but only 'landing' final on
PROPERTIES with non-nil :ID:"
  (progn (message "ensuring tag for %s" tag)
         (org-roam-tag-add tag)))

(defun qz/agenda-files-update (&rest _)
  "Update the value of `org-agenda-files' with relevant candidates"
  (interactive)
  (setq org-agenda-files
        (seq-uniq (append qz/org-agenda-files (qz/project-files)))
        qz/agenda-daily-files (qz/agenda-daily-files-f)))

(defun qz/note-buffer-p (&rest _)
  "Return non-nil if the currently visited buffer is a note."
  (interactive)
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun qz/file-has-todo-p (&rest _)
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
  (org-with-wide-buffer
   (org-element-map                          ; (2)
       (org-element-parse-buffer 'headline) ; (1)
       'headline
     (lambda (h)
       (eq (org-element-property :todo-type h)
           'todo))
     nil 'first-match)))                     ; (3)

(defun qz/has-tag-person-p (&rest tags)
  (message "has-tag-person-p %s" tags)
  (seq-contains-p tags "person"))

(defun qz/has-link-p (a b)
  "undirected connection exists, from `src' to `dst'"
   (org-roam-db-query
    [:select [source dest]
     :from links
     :where (or (and (= dest a) (= source b))
                (and (= dest b) (= source a)))]))

(defun qz/has-link-to-p (dst &optional src)
  "directed connection exists, from `src' to `dst'"
  (if-let ((nap (org-roam-node-at-point)))
      (let ((src (or src (org-roam-node-id nap))))
        (org-roam-db-query
         [:select source
          :from links
          :where (and (= dest $r1)
                      (= source $r2))]
         src dst))))

(defun qz/private-p (&rest _)
  (interactive)
  (let ((title (qz/node-title)))
    (if (not title)
        (and (message "unable to evaluate privateness; no title") nil) ; return false (not private)
      (or (string-match-p qz/daily-title-regexp title) ; daily
          (string-match-p "meeting" title)                                    ; concerns a meeting
          (qz/has-link-to-p (qz/title->roam-id "thinkproject"))))))           ; concerns work

(setq qz/auto-buffer-action
      '((qz/file-has-todo-p  . (lambda (tagstring) (qz/ensure-tag tagstring "project")))
        (qz/private-p   . (lambda (tagstring) (qz/ensure-tag tagstring "private")))
        (qz/has-tag-person-p . (lambda (tagstring)
                                 (qz/ensure-tag tagstring (qz/title-to-tag (qz/node-title)))))))

;;(car (car qz/auto-buffer-action))

(defun qz/dispatch-hook ()
  "Dispatches actions in notes based on filetags given `qz/auto-tag-action'. Assumes current buffer"
  (interactive)
  (when (and (not (+org-capture-frame-p))
             (not (org-roam-capture-p))
             (qz/note-buffer-p))
    (let ((tags (qz/node-tags)))
      (mapc (lambda (tag+fun)
              (when (funcall (car tag+fun) tags)
                (funcall (cdr tag+fun) "")))
            qz/auto-buffer-action))))

(advice-add 'org-agenda :before #'qz/agenda-files-update)
(add-hook 'find-file-hook   #'qz/dispatch-hook)
(add-hook 'before-save-hook #'qz/dispatch-hook)

(defun qz/sqlite-row-col (table)
  (cl-loop
   for tuple in (org-roam-db-query
                 `[:select *
                   :from ,table
                   :limit 1])
   collect (cl-loop for attr in tuple
                    for heading in (org-roam-db-query
                                    `[:select name
                                      :from (funcall pragma_table_info ',table)
                                      :order-by cid :asc])
                    collect (cons (car heading) (or attr 'null)))))

;(qz/pprint (qz/sqlite-row-col 'links))

(defun qz/org-roam-private-files ()
  "Return a list of note files containing tag =private="
  (seq-map
   #'car
   (org-roam-db-query
    [:select node_id
     :from tags
     :where (like tag (quote "%\"private\"%"))])))

;(qz/pprint (qz/org-roam-private-files))

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

;;(qz/get-headline-path t t 'qz/headline-mm)


;; (progn
;;   (qz/get-headline-path)
;;   (qz/get-headline-path nil t)
;;   (qz/get-headline-path t)
;;   (qz/get-headline-path t t)
;;   (qz/get-headline-path t t 'qz/headline-mm)
;;   (qz/get-headline-path t nil 'qz/headline-mm)
;;   (qz/get-headline-path nil nil 'qz/headline-mm))

(defun qz/org-noter-insert-note (notetext &optional precise-info)
  "Insert note associated with the current location.

This command will prompt for a title of the note and then insert
it in the notes buffer. When the input is empty, a title based on
`org-noter-default-heading-title' will be generated.

If there are other notes related to the current location, the
prompt will also suggest them. Depending on the value of the
variable `org-noter-closest-tipping-point', it may also suggest
the closest previous note.

PRECISE-INFO makes the new note associated with a more
specific location (see `org-noter-insert-precise-note' for more
info).

When you insert into an existing note and have text selected on
the document buffer, the variable `org-noter-insert-selected-text-inside-note'
defines if the text should be inserted inside the note."
  (interactive)
  (org-noter--with-valid-session
   (message "double yeet")
   (let* ((ast (org-noter--parse-root)) (contents (org-element-contents ast))
          (window (org-noter--get-notes-window 'force))
          (selected-text
           (cond
            ((eq (org-noter--session-doc-mode session) 'pdf-view-mode)
             (when (pdf-view-active-region-p)
               (mapconcat 'identity (pdf-view-active-region-text) ? )))

            ((eq (org-noter--session-doc-mode session) 'nov-mode)
             (when (region-active-p)
               (buffer-substring-no-properties (mark) (point))))))
          force-new
          (location (org-noter--doc-approx-location (or precise-info 'interactive) (gv-ref force-new)))
          (view-info (org-noter--get-view-info (org-noter--get-current-view) location)))

     (let ((inhibit-quit t))
       (with-local-quit
         (select-frame-set-input-focus (window-frame window))
         (select-window window)

         ;; IMPORTANT(nox): Need to be careful changing the next part, it is a bit
         ;; complicated to get it right...

         (let ((point (point))
               (minibuffer-local-completion-map org-noter--completing-read-keymap)
               collection default default-begin title selection
               (empty-lines-number (if org-noter-separate-notes-from-heading 2 1)))

           (cond
            ;; NOTE(nox): Both precise and without questions will create new notes
            ((or precise-info force-new)
             (setq default (and selected-text (replace-regexp-in-string "\n" " " selected-text))))
            (org-noter-insert-note-no-questions)
            (t
             (dolist (note-cons (org-noter--view-info-notes view-info))
               (let ((display (org-element-property :raw-value (car note-cons)))
                     (begin (org-element-property :begin (car note-cons))))
                 (push (cons display note-cons) collection)
                 (when (and (>= point begin) (> begin (or default-begin 0)))
                   (setq default display
                         default-begin begin))))))

           (setq collection (nreverse collection)
                 title (if org-noter-insert-note-no-questions
                           default
                         notetext)
                 selection (unless org-noter-insert-note-no-questions (cdr (assoc title collection))))

           (if selection
               ;; NOTE(nox): Inserting on an existing note
               (let* ((note (car selection))
                      (insert-before-element (cdr selection))
                      (has-content
                       (eq (org-element-map (org-element-contents note) org-element-all-elements
                             (lambda (element)
                               (if (org-noter--check-location-property element)
                                   'stop
                                 (not (memq (org-element-type element) '(section property-drawer)))))
                             nil t)
                           t)))
                 (when has-content (setq empty-lines-number 2))
                 (if insert-before-element
                     (goto-char (org-element-property :begin insert-before-element))
                   (goto-char (org-element-property :end note)))


                 (if (org-at-heading-p)
                     (progn
                       (org-N-empty-lines-before-current empty-lines-number)
                       (forward-line -1))
                   (unless (bolp) (insert "\n"))
                   (org-N-empty-lines-before-current (1- empty-lines-number)))

                 (when (and org-noter-insert-selected-text-inside-note selected-text) (insert selected-text)))

             ;; NOTE(nox): Inserting a new note
             (let ((reference-element-cons (org-noter--view-info-reference-for-insertion view-info))
                   level)
               (when (zerop (length title))
                 (setq title (replace-regexp-in-string (regexp-quote "$p$") (number-to-string (car location))
                                                       org-noter-default-heading-title)))

               (if reference-element-cons
                   (progn
                     (cond
                      ((eq (car reference-element-cons) 'before)
                       (goto-char (org-element-property :begin (cdr reference-element-cons))))
                      ((eq (car reference-element-cons) 'after)
                       (goto-char (org-element-property :end (cdr reference-element-cons)))))

                     ;; NOTE(nox): This is here to make the automatic "should insert blank" work better.
                     (when (org-at-heading-p) (backward-char))

                     (setq level (org-element-property :level (cdr reference-element-cons))))

                 (goto-char (org-element-map contents 'section
                              (lambda (section) (org-element-property :end section))
                              nil t org-element-all-elements))
                 (setq level (1+ (org-element-property :level ast))))

               ;; NOTE(nox): This is needed to insert in the right place
               (outline-show-entry)
               (org-noter--insert-heading level title empty-lines-number location)
               (when (org-noter--session-hide-other session) (org-overview))

               (setf (org-noter--session-num-notes-in-view session)
                     (1+ (org-noter--session-num-notes-in-view session)))))

           (org-show-set-visibility t)
           (org-cycle-hide-drawers 'all)
           (org-cycle-show-empty-lines t)))
       (when quit-flag
         ;; NOTE(nox): If this runs, it means the user quitted while creating a note, so
         ;; revert to the previous window.
         (select-frame-set-input-focus (org-noter--session-frame session))
         (select-window (get-buffer-window (org-noter--session-doc-buffer session))))))))

(require 'org-ref)
(setq reftex-bib-path  '("~/life/tex.bib")
      reftex-default-bibliography reftex-bib-path
      org-ref-default-bibliography reftex-bib-path)

(require 'org-super-agenda)

(require 'ox-reveal)

(require 'mathpix)
(setq mathpix-screenshot-method "scrot -s %s")

(map! "C-c o m" #'qz/mathpix-screenshot)

;; add var capture, to save last result
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
;; (defun just-one-face (fn &rest args)
;;   (let ((orderless-match-faces [completions-common-part]))
;;     (apply fn args)))

;; (advice-add 'company-capf--candidates :around #'just-one-face)

;; (use-package! orderless
;;   :config
;;   (after! ivy
;;     (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))))


(use-package! orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))
  :config
  (after! ivy
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))))


;;(prescient-persist-mode 1)
;;(selectrum-prescient-mode 1)
;;(setq selectrum-refine-candidates-function #'orderless-filter)
;;(setq selectrum-highlight-candidates-function #'orderless-highlight-matches)

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

(defun qz/thing-at-point-or-region-and-region (&optional thing prompt)
  "Grab the current selection, THING at point, or xref identifier at point.

Returns THING if it is a string. Otherwise, if nothing is found at point and
PROMPT is non-nil, prompt for a string (if PROMPT is a string it'll be used as
the prompting string). Returns nil if all else fails.

NOTE: Don't use THING for grabbing symbol-at-point. The xref fallback is smarter
in some cases."
  (declare (side-effect-free t))
  (cond ((stringp thing)
         thing)
        ((doom-region-active-p)
         (cons (buffer-substring-no-properties (region-beginning) (region-end))
               (cons (region-beginning)
                     (region-end))))
        (thing
         (cons (thing-at-point thing t)
               (bounds-of-thing-at-point thing)))
        ((require 'xref nil t)
         ;; Eglot, nox (a fork of eglot), and elpy implementations for
         ;; `xref-backend-identifier-at-point' betray the documented purpose of
         ;; the interface. Eglot/nox return a hardcoded string and elpy prepends
         ;; the line number to the symbol.
         (let* ((val
                 (if (memq (xref-find-backend) '(eglot elpy nox))
                     (thing-at-point 'symbol t)
                   ;; A little smarter than using `symbol-at-point', though in most
                   ;; cases, xref ends up using `symbol-at-point' anyway.
                   (xref-backend-identifier-at-point (xref-find-backend)))))
           (cons val (bounds-of-thing-at-point 'symbol))))
        (prompt
         (read-string (if (stringp prompt) prompt "")))))

(defun qz/org-roam-node-insert (&optional filter-fn pass-thru)
  "Find an Org-roam file, and insert a relative org link to it at point.
Return selected file if it exists.
If LOWERCASE is non-nil, downcase the link description.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions."
  (interactive)
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* ((pt (qz/thing-at-point-or-region-and-region))
               (beg (set-marker (make-marker) (car (cdr pt))))
               (end (set-marker (make-marker) (cdr (cdr pt))))
               (region-text (org-link-display-format
                             (substring-no-properties (car pt))))
               (node (if pass-thru
                         (or (org-roam-node-from-title-or-alias region-text)
                             (org-roam-node-create :title region-text))
                       (org-roam-node-read region-text filter-fn)))
               (description (or (and node region-text (org-roam-node-title node))
                                region-text)))
          (if (org-roam-node-id node)
              (progn
                (when region-text
                  (delete-region beg end)
                  (set-marker beg nil)
                  (set-marker end nil))
                (insert (org-link-make-string
                         (concat "id:" (org-roam-node-id node))
                         description)))
            (funcall
              `(lambda ()
                 (org-roam-capture-
                  :node node
                  ,@(when pass-thru '(:keys "n")) ; ; [[id:bc3c61d4-d720-40a8-9018-6357f05ae85e][roam-capture-template]]
                  :props (append
                          (when (and beg end)
                            (list :region (cons beg end)))
                          (list :insert-at (point-marker)
                                :link-description description
                                :finalize 'insert-link))))))))
    (deactivate-mark)))

(setq qz/org-agenda-prefix-length 20
      org-agenda-prefix-format nil)
      ;; '((agenda . " %i %(vulpea-agenda-category qz/org-agenda-prefix-length)%?-12t% s")
      ;;   (todo . " %i %(vulpea-agenda-category qz/org-agenda-prefix-length) ")
      ;;   (tags . " %i %(vulpea-agenda-category qz/org-agenda-prefix-length) ")
      ;;   (search . " %i %(vulpea-agenda-category qz/org-agenda-prefix-length) ")))

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
         (title (qz/node-title))
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
