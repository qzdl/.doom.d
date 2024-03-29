;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.
;;
;; WARNING: Disabling core packages listed in ~/.emacs.d/core/packages.el may
;; have nasty side-effects and is not recommended.


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(unpin! t)

;; ...but to unpin a single package:
;(unpin! pinned-package)
;; Use it to unpin multiple packages
;(unpin! pinned-package another-pinned-package)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;;; Code:

(package! exwm)
(package! posframe)
(package! ivy-posframe
  :recipe (:host github :repo "tumashu/ivy-posframe"))
(package! which-key-posframe)
(package! perfect-margin
  :recipe (:host github :repo "mpwang/perfect-margin"))

(package! emacs-wallpaper
  :recipe (:host github :repo "farlado/emacs-wallpaper"))
(package! modus-themes
  :recipe (:host gitlab :repo "protesilaos/modus-themes"))
(package! hyperbole)

(package! speed-type)
(package! orderless
 :recipe (:host github :repo "oantolin/orderless"))
(package! ovpn-mode)

(package! emacs-slack
  :recipe (:host github :repo "aculich/emacs-slack"
           :branch "cookie"))


;; matrix
(package! plz
  :recipe (:host github :repo "alphapapa/plz.el"))
(package! ement
  :recipe (:host github :repo "alphapapa/ement.el"))

(package! eva
  :recipe (:host github :repo "meedstrom/eva"
           :files (:defaults "assets"  "renv" "*.R" "*.gnuplot")))

(package! kubernetes)

;;=============
;; ORG STUFF ;;
;;=============
(package! mathpix
 :recipe (:host github :repo "jethrokuan/mathpix.el"))
(package! org-drill)
(package! org-jira)
(package! org-fragtog)
(package! org-gcal)
(package! org-download)
(package! org-auto-tangle) ; https://github.com/yilkalargaw/org-auto-tangle
(package! org-super-agenda)

;;; ORG ROAM
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam"))
(package! org-transclusion
  :recipe (:host github :repo "nobiot/org-transclusion" :branch "dev/0.2.1"))
;;(package! org-roam-server)
;;(package! org-roam-bibtex)

;;; OTHER PDF & BIB
(package! org-ref)
;(package! org-pdftools)
;(package! org-noter-pdftools)

(package! org-recoll
  :recipe (:host github :repo "alraban/org-recoll"))

(package! ox-hugo)

;;; OX
(package! ox-reveal)

;;; BABEL
;;(package! ob-ipython)

;;==========
;; WINDOW ;;
;;==========
(package! window-go
  :recipe (:host github :repo "akirak/emacs-window-go"))
;(use-package exwm-window-go
;  :straight window-go)

;;========
;; MISC ;;
;;========
(package! keyfreq)
(package! google-translate)
(package! gif-screencast
  :recipe (:host gitlab :repo "ambrevar/emacs-gif-screencast"))
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el"))
(package! explain-pause-mode
  :recipe (:host github :repo "lastquestion/explain-pause-mode"))
(package! counsel-spotify
  :recipe (:host github :repo "Lautaro-Garcia/counsel-spotify"))
(package! ereader)

;;=========
;; THEME ;;
;;=========
(package! tron-legacy-emacs-theme
  :recipe (:host github :repo "ianpan870102/tron-legacy-emacs-theme"))
(package! nano-emacs
  :recipe (:host github :repo "rougier/nano-emacs"))
(package! smex)

;;; packages.el ends here
