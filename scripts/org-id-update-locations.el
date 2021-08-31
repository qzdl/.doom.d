(require 'org)
(org-id-update-id-locations
    (directory-files-recursively "~/life/roam" ".org$\\|.org.gpg$"))
