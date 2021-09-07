;; Modified version of contrib/lisp/org-man.el; see
;; (http://orgmode.org/manual/Adding-hyperlink-types.html#Adding-hyperlink-types)
(require 'org)

(org-add-link-type "info" 'org-info-open)
(org-link-set-parameters "man"
                         :follow #'org-info-open
                         :export #'org-info-export
                         :store #'org-info-store-link)

(defcustom org-info-command 'info
  "The Emacs command to be used to display an info page."
  :group 'org-link
  :type '(choice (const info)))

(defun org-info-open (path)
  "Visit the infopage on PATH.
   PATH should be a topic that can be thrown at the info command."
  (funcall org-info-command path))

(defun org-info-store-link ()
  "Store a link to an info page."
  (when (memq major-mode '(Info-mode))
    ;; This is a info page, we do make this link
    (let* ((page (org-info-get-page-name))
           (link (concat "info:" page))
           (description (format "Infopage for %s (L%s)" page (locate-current-line-number))))
      (org-store-link-props
       :type "info"
       :link link
       :description description))))

(defun org-info-get-page-name ()
  "Extract the page name from Info in a hackish way."
  ;; This works for `Info-mode'.
  ;; Hackity-hack: copy the node name into the kill ring.
  (concat (Info-copy-current-node-name) ":" (locate-current-line-num)
  ;; Just return the kill.
  (current-kill 0))

(defun org-info-export (link description format _)
  "Export an info page link from Org files."
  (let ((path (format "https://samuelculpepper.com/share/info/%s" link))
        (desc (or description link)))
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
      (`latex (format "\\href{%s}{%s}" path desc))
      (`texinfo (format "@uref{%s,%s}" path desc))
      (`ascii (format "%s (%s)" desc path))
      (t path)))))

(provide 'ol-info)
