;; Modes customization.

;; ============================== General ===================================

;; turn on spell checking for everything we do.
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; refresh unmodified files that have been modified on disk outside of Emacs.
(global-auto-revert-mode 1)

;; ================================ Text =====================================

;; automatically wrap text at fill-column many columns and enable spell
;; checking.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; =============================== C/C++ =====================================

;; our C/C++ code is BSD-like though we don't indent braces.
(defun my-c-mode-hook ()
  (c-set-style "bsd")

  ;; brace indendation.
  (c-set-offset 'innamespace 0 t)
  (c-set-offset 'namespace-open 0 t)
  (c-set-offset 'namespace-close 0 t)
  (c-set-offset 'defun-block-intro 4 t)

  ;; statement indentation.
  (setq c-basic-offset 4)
  (setq c-basic-indent 4)

  ;; case statement indentation is slightly offset.
  (c-set-offset 'case-label 0 t)
  (c-set-offset 'substatement-open 0 t)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; ============================== Fortran ====================================

(autoload 'align-f90-load "align-f90" "Enable alignment in Fortran modes." nil)

;; the f90 mode has reasonable defaults (arguably there isn't much to
;; configure) though we ensure align works and turn off block matching blink.
;; the former makes up for any deficiencies in the mode's default formatting
;; and the latter reduces frustration as Emacs pauses to show us something
;; we probably already know about.
(defun my-f90-mode-hook ()
  (align-f90-load)

  (setq f90-smart-end 'no-blink)

  (setq f90-do-indent 3)
  (setq f90-if-indent 3)
  )

(add-hook 'f90-mode-hook 'my-f90-mode-hook)

;; ================================ MATLAB ===================================

(autoload 'matlab-mode  "matlab" "MATLAB editing mode." t)
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)

;; only enable matlab-mode when requested.  if we don't, octave-mode will be
;; used instead.
(when matlab-mode-p
  (add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode)))

;; we are perfectly capable of wrapping our own code.
(setq matlab-auto-fill nil)
(setq matlab-fill-code nil)

;; our inferior process should not present a desktop or a splash screen.
(setq matlab-shell-command-switches "-nodesktop -nosplash")

;; disable the "help" to match a file's function name to the file name.
(setq matlab-verify-on-save-flag nil)

;; indent Matlab code like we do most every language.
(setq matlab-indent-level 4)

;; ensure that continued lines are aligned with the preceding's
;; parentheses/brackets/braces.
(setq matlab-align-to-paren t)

;; ================================ Octave ===================================

;; only enable octave-mode when requested.  if we don't, matlab-mode will be
;; used instead.
(unless matlab-mode-p
  (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode)))

;; Octave mode is, umm, less than helpful in providing knobs to configure
;; its formatting so we fiddle with its internals to make it usable.  hence
;; the hook to modify buffer-local variables.
(defun my-octave-mode-hook ()

  ;; disable the "help" to match a file's function name to the file name.
  ;;
  ;; XXX: this (repeated) redefinition does the trick though should probably
  ;;      be closer to something like the following:
  ;;
  ;;   (remove-hook 'before-save-hook 'octave-sync-function-file-names)
  (defun octave-sync-function-file-names ())

  ;; match MATLAB's indentation level.
  (setq octave-block-offset matlab-indent-level)

  ;; override the Octave-specific comment parameters and match what we
  ;; use with MATLAB.  this aligns comments at the current indentation
  ;; level, uses a single comment, and adds a space after the comment
  ;; delimiter.
  (setq comment-start "% ")
  (setq comment-add 0)
  (setq comment-column octave-block-offset)
  (setq comment-style "plain")

  ;; don't waste time showing us matching control structures.
  (setq octave-blink-matching-block nil)
  )

(add-hook 'octave-mode-hook 'my-octave-mode-hook)

;; ========================== Shell Processes ================================

;; ensure that shell's are started with unique names.
(add-hook 'shell-mode-hook (lambda () (rename-uniquely)))

;; ============================= Makefiles ===================================

;; electric keys help us fill in macro and target names in Makefile mode.
(setq makefile-electric-keys t)

;; ================================ XML ======================================

; fix the XML mode indentation level to be sane.
(add-hook 'sgml-mode-hook
          (lambda ()
            (setq sgml-basic-offset 4)))

;; let kill-sexp work on tags.
(setq nxml-sexp-element-flag t)

;; close tags automatically when we type </.
(setq nxml-slash-auto-complete-flag t)

;; ============================== Man Pages ==================================

;; have man pages rendered in a separate window that does not take
;; focus.
;;
;; XXX: it would be nice to have a single man page buffer rather
;;      than one per page.  need to look at how uniquify interacts
;;      here.
(setq Man-notify-method 'friendly)

;; ============================ xrdb/Xdefaults ===============================

(setq auto-mode-alist
      (append '(("\\.Xdefaults$"     . xrdb-mode)
                ("\\.Xenvironment$"  . xrdb-mode)
                ("\\.Xresources$"    . xrdb-mode))
              auto-mode-alist))

(autoload 'xrdb-mode "xrdb-mode" "Mode for editing X resource files." t)

;; ============================= Regex Builder ===============================

;; avoid backslash hell when working with regex-builder.  see:
;;
;;   https://www.emacswiki.org/emacs/RegularExpression
;;
;; for a concise cheat sheet on the syntax.
(setq reb-re-syntax 'string)
