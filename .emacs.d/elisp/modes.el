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

;; set all indentation to four spaces for Fortran90, Fortran95, Fortran 2003 and
;; Fortran 2008 code.  we turn on Fortran-specific alignment mode and disable
;; block matching to avoid unnecessary slowdown/distractions during indentation
;; of an END statement.
(defun my-f90-mode-hook ()
  ;; turn on alignment for '=' and '::' (and by proxy, a kludge for '=>').
  (align-f90-load)

  ;; prevent Emacs from blinking the point to the beginning of the block
  ;; associated with the current END statement.  this makes bulk formatting
  ;; unnecessarily slow.
  (setq f90-smart-end 'no-blink)

  ;; indent everything consistently to four space.
  (setq f90-do-indent 4)
  (setq f90-if-indent 4)
  (setq f90-type-indent 4)
  (setq f90-program-indent 4)
  (setq f90-associate-indent 4)
  (setq f90-critical-indent 4)

  ;; do not continue lines with additional ampersands.
  (setq f90-beginning-ampersand nil)
  )

(add-hook 'f90-mode-hook 'my-f90-mode-hook)

;; ================================= GDB =====================================

;; we just want the GUD buffer when we start debugging.
(setq gdb-many-windows nil)

;; configures a 6 window layout for debugging:
;;
;;   +--------------------------+--------------------------+
;;   |                          |        break points      |
;;   |                          +--------------------------+
;;   |                          |            stack         |
;;   |                          +--------------------------+
;;   |                          |            local         |
;;   |       source code        |          variables       |
;;   |                          +--------------------------+
;;   |                          |                          |
;;   |                          |                          |
;;   |                          |                          |
;;   |                          |            GDB           |
;;   +--------------------------+                          |
;;   |  standard input/output   |                          |
;;   +--------------------------+--------------------------+
;;
;; each window is fixed in its position for the duration of the debugging.
;;
;; derived from the code found here:
;;
;;   https://stackoverflow.com/questions/3860028/customizing-emacs-gdb/41326527#41326527
;;
;; with minor cleanup for readability and ease of understanding.
(defun set-gdb-layout (&optional c-buffer)
  "Configures the frame for a six panel GDB layout.  If provided a buffer,
display it as the source, otherwise use the current buffer."
  ;; default to the current buffer if we weren't provided one.
  (if (not c-buffer)
      (setq c-buffer (window-buffer (selected-window))))

  ;; reset this frame to a single window containing our GDB shell.
  ;;
  ;; originally gleaned from:
  ;;
  ;;   from http://stackoverflow.com/q/39762833/846686
  ;;
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)

  (let*
      (
       ;; our source code window should be 90% of the left pane's height.  this
       ;; implies the I/O window is only 10%.
       (source-body-height (floor (* 0.9 (window-body-height))))

       ;; define the six windows we'll show while debugging.
       ;;
       ;; NOTE: the order matters here both in the sense of where each buffer
       ;;       resides in the layout as well as how large it will be.
       ;;
       ;; NOTE: for terminal Emacs users, seriously consider any update to the
       ;;       arrangement of windows as the GDB window redefines M-p
       ;;       (comint-previous-input instead of other-window-backward).  the
       ;;       layout below allows quick navigation between the source and GDB
       ;;       buffers via M-p and M-o.
       (w-source      (selected-window))                                 ;; left top
       (w-gdb         (split-window w-source nil                'right)) ;; right bottom
       (w-locals      (split-window w-gdb    nil                'above)) ;; right middle bottom
       (w-stack       (split-window w-locals nil                'above)) ;; right middle top
       (w-breakpoints (split-window w-stack  nil                'above)) ;; right top
       (w-io          (split-window w-source source-body-height 'below)) ;; left bottom
       )

    ;; create the layout.  each window, save for the GDB and source code, are
    ;; persistent and cannot contain other buffers than what they're created
    ;; with.
    (set-window-buffer      w-io          (gdb-get-buffer-create 'gdb-inferior-io))
    (set-window-dedicated-p w-io t)
    (set-window-buffer      w-breakpoints (gdb-get-buffer-create 'gdb-breakpoints-buffer))
    (set-window-dedicated-p w-breakpoints t)
    (set-window-buffer      w-locals      (gdb-get-buffer-create 'gdb-locals-buffer))
    (set-window-dedicated-p w-locals t)
    (set-window-buffer      w-stack       (gdb-get-buffer-create 'gdb-stack-buffer))
    (set-window-dedicated-p w-stack t)
    (set-window-buffer      w-gdb          gud-comint-buffer)
    (set-window-buffer      w-source       c-buffer)

    ;; put the focus back to the source code.
    (select-window w-source)
    ))

;; wrap the entry point to the debugger so we can maintain pre-debugging window
;; state.
(defadvice gdb (around args activate)
  "Maintains the current window configuration so GDB's windows do not wreck havoc."
  ;; maintain the current state of the windows in the active frame so we can put
  ;; things back after GDB is done.
  (setq global-config-editing (current-window-configuration))

  ;; use the current window's contents as the source code buffer in our layout.
  (let ((c-buffer (window-buffer (selected-window))))
    ad-do-it
    (set-gdb-layout c-buffer)))

;; put the windows back the way they were once GDB exits.
(defadvice gdb-reset (around args activate)
  "Resets the post-debugging window configuration back to what it was before GDB started."
  ad-do-it
  (set-window-configuration global-config-editing))

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

;; make sure we can run Octave as an inferior process.
(autoload 'run-octave "octave-inf" "Interactive Octave mode." t)

;; older versions of Emacs (24.3.y and older) need to be told what prompt to
;; look for.  from here:
;;
;;    https://savannah.gnu.org/bugs/?41099#comment4
(setq inferior-octave-prompt ">> ")

;; make sure we're not attempting to start a GUI.  new versions of octave-mode
;; (Emacs 24.4 and newer) add this to deal with new versions of Octave, though
;; this handles older versions.  additionally, silence the startup messages.
;;
;; NOTE: adding --line-editing here probably won't fix older Octave's management
;;       of closed figures and you need to directly edit the octave-inf.elc file
;;       directly as found here:
;;
;;          https://stackoverflow.com/questions/25436702/how-to-get-octaves-plot-to-work-under-emacs
;;
;;       octave-inf appends any arguments below to "--no-line-editing" and older
;;       versions do not let a later "--line-editing" supersede the earlier
;;       option.
(setq inferior-octave-startup-args '("--no-gui" "-q"))

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

;; ============================ Help Functions Plus ==========================

;; queue loading the help-fns+ package whenever its functions are invoked.
;; these are rarely used so we don't pull the package in unless needed.
(mapcar (lambda (arg)
          (autoload arg "help-fns+" "Improved help functions." t))
        '(describe-buffer
          describe-command
          describe-option
          describe-key-briefly
          describe-option-of-type
          describe-copying
          describe-file
          describe-keymap
          find-function-on-key))

;; ============================== Markdown Mode ==============================

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub-flavored Markdown files" t)

;; handle files with the most specific flavor of Markdown that we can reasonably
;; expect.
(setq auto-mode-alist
      (append '(("README\\.md$" . gfm-mode)
                ("\\.md$"       . markdown-mode)
                ("\\.markdown$" . markdown-mode))
              auto-mode-alist))

;; use Pandoc to export Markdown to other file types, while using MathJax to
;; render LaTeX equations in HTML exports.  while it uses an external library
;; (which could be slow/unavailable) it results in a smaller output.
(setq markdown-command "pandoc --mathjax -t html -s --mathjax=https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")

;; ============================ Python Environment ===========================

(setq python-environment-directory "~/.virtual-environments")

;; ================================= Dockerfile ==============================

;; add syntax highlighting to Dockerfile's.
(require 'dockerfile-mode)

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
