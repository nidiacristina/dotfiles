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

;; =============================== Python Mode ===============================

(autoload 'flymake-python-pyflakes-load "flymake-python-pyflakes"
  "Enable flymake with Pyflakes in Python mode." nil)
(autoload 'jedi:setup "jedi" 
  "Enable jedi for completion in Python mode." t)

;; provide autocompletion on module symbols (foo.<completion>).
(setq jedi:complete-on-dot t)

;; setup overlays that display flymake information in the minibuffer.  this
;; provides a non-intrusive way to see them when we don't want to (or cannot)
;; hover the mouse over a line.
(defun my-python-mode-hook ()
  ;; boostrap pyflakes scanning and Jedi once when we edit Python code.
  (flymake-python-pyflakes-load)
  (jedi:setup)

  ;; show the current line's pyflake info/warning/error after we idle for a bit.
  ;; we set this locally to the buffer so we don't have useless idle timers for
  ;; every buffer after we edit Python code.
  (make-local-variable 'help-at-pt-timer-delay)
  (make-local-variable 'help-at-pt-display-when-idle)

  (setq help-at-pt-timer-delay 0.9)
  (setq help-at-pt-display-when-idle '(flymake-overlay))

  ;; NOTE: we have to manually set the timer otherwise simply setting the
  ;;       variables will have no effect.
  (help-at-pt-set-timer)
  )

(add-hook 'python-mode-hook 'my-python-mode-hook)

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

;; =============================== MediaWiki =================================

(autoload 'mediawiki-mode "mediawiki"
  "Major mode for editing MediaWiki pages." t)

;; ============================ Python Environment ===========================

(setq python-environment-directory "~/.virtual-environments")

;; ========================== Emacs IPython Notebooks ========================

;; setup all of the EIN autoloads.
(require 'ein-loaddefs)

;; ensure that we make working with Python code as easy as possible by tying in
;; Jedi and auto-complete.
(setq ein:use-auto-complete t)
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

;; only configure notebooks to handle Python and text.  the MuMaMo support
;; offers nothing useful at the expense of pulling in unmaintained packages that
;; spew warnings and dominate *Messages*.  likely I'm doing something wrong
;; with the configuration though I can't figure out what...
(setq ein:notebook-modes '(ein:notebook-python-mode
                           ein:notebook-plain-mode))

;; ============================= Chrome Editing ==============================

(autoload 'edit-server-start "edit-server"
  "Start a server to handle text entry edit requests from Chrome" t)

;; specify where the server runs.  this needs to match the browser's
;; configuration.
(setq edit-server-port 9292)

;; edit text boxes in an existing Emacs instance rather than creating a new
;; frame just for the occasion.
(setq edit-server-new-frame nil)

;; handle text boxes from specific URLs with a given mode.  we assume all
;; Github inputs allow Markdown.
(setq edit-server-url-major-mode-alist
      '(("github\\.com" . gfm-mode)
        ("mediawiki"    . mediawiki-mode)))

;; uncomment this if you want to automatically start the edit server and let
;; Chrome connect to the Emacs instance.
;;
;; NOTE: blindly enabling this can be a security risk on multi-user systems!
;;
;; (edit-server-start)

;; =================================== Git ===================================

;; let us start gitsum directly by calling it's entry function.
;;
;; NOTE: loading gitsum this way will *not* patch itself into the git-status
;;       mode map can cannot be invoked via key press.  it is still available
;;       via explicit call of it's functions.
;;
(autoload 'gitsum "gitsum"
  "Interactive patch scheduling with Git ala darcsum." t)

;; load gitsum whenever we enter git-status-mode.  this is the preferred
;; method for loading the package as it updates git-status' key map so
;; gitsum can be called.
(defun my-git-status-mode-hook ()

  ;; NOTE: we don't want to invoke gitsum here as that would bypass git-status'
  ;;       buffer and start gitsum immediately.
  (require 'gitsum)
  )

(add-hook 'git-status-mode-hook 'my-git-status-mode-hook)
