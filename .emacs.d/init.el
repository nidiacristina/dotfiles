;; User configuration file for Emacs.  Targets C/Matlab/Python/Fortran/Shell
;; development, with a splash of LaTex, on terminals.
;;
;; NOTE: This targets Emacs 23 and newer and should work with any OS that
;;       came out after the second half of 2009.  That said, this has only
;;       been lightly tested on Emacs 23 itself...

(let ((minver "23.1"))
  (when (version<= emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher." minver)))

;; specify where our code and packages live, as well as the compatibility
;; packages needed for them.
;;
;; NOTE: only the contents of the directories listed here are searched.
;;       load-path is not processed recursively.
(setq user-paths '("~/.emacs.d/elisp"
                   "~/.emacs.d/elisp/align-f90"
                   "~/.emacs.d/elisp/gitsum"
                   "~/.emacs.d/elisp/mirror/help-fns+"
                   "~/.emacs.d/elisp/markdown-mode"
                   "~/.emacs.d/elisp/matlab"))
(setq compatibility-paths '"~/.emacs.d/elisp/mirror/cl-lib")

;; add all of the user paths to the front of the search and the compatibility
;; paths to the back.  this should keep them properly shadowed when loaded on
;; systems where they're already implemented.
(mapcar (lambda (arg)
	  (add-to-list 'load-path arg))
	  user-paths)
(mapcar (lambda (arg)
	  (add-to-list 'load-path arg) t)
	  compatibility-paths)

(require 'whitespace)    ; highlight errant whitespace.
(require 'uniquify)      ; keep buffer names unique as needed.
(require 'comint)        ; command interpreters like Make, shell, etc.

;; some aspects of the configuration are influenced by the following top-level
;; parameters.

;; specify whether we use matlab.el from SourceForge or octave-mode.  by
;; default we use what is distributed with Emacs.
(setq matlab-mode-p nil)

;; the configuration is broken down into the following categories for
;; ease of maintenance.
(load-library "functions")
(load-library "modes")
(load-library "keys")
(load-library "style")

;; save every four lines or so.
(setq auto-save-interval 300)

;; always edit the target of a symlink rather than the symlink itself.
(add-hook 'find-file-hooks 'visit-target-instead)

;; fire up a server that clients can connect to.  if we've been given a better
;; server name, use it.
(let ((user-server-name (getenv "EMACS_SERVER")))
  (when user-server-name
    (setq server-name user-server-name)))
(server-start)

;; show the time in the mode line.  we do this absolutely last as an easy
;; indicator that our configuration loaded properly.
(setq display-time-day-and-date t)
(display-time)
