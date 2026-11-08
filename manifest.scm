;; What follows is a "manifest" equivalent to the command line you gave.
;; You can store it in a file that you may then pass to any 'guix' command
;; that accepts a '--manifest' (or '-m') option.

(specifications->manifest
 (list "emacs"
       "emacs-async"
       "emacs-f"
       "emacs-logito"
       "emacs-package-lint"
       "emacs-pcache"
       "pkg-config"))
