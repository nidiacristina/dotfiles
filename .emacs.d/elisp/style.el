;; Style configuration.

;; wrap buffers as a sane length.
(setq-default fill-column 80)

;; turn on syntax highlighting for every buffer and make it as pretty as
;; possible.
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; keep the current region highlighted until we modify the buffer.  this makes
;; it considerably easier to see where the mark is in most cases.
(transient-mark-mode t)

;; highlight all matching text (not just the one under the point) during
;; search and replace.
(setq search-highlight t)
(setq query-replace-highlight t)

;; =============================== Faces =====================================

;; purple background sans foreground for highlighting whitespace.  note that
;; this is added into whitespace.el's faces namespace.
(defface highlight-whitespace
  '((((class color) (min-colors 256) (background light)) :background "#d700d7") ; purple
    (((class color) (min-colors 256) (background dark))  :background "#d700d7")
    (((class color) (min-colors 16)  (background light)) :background "#cd00cd") ; magenta
    (((class color) (min-colors 16)  (background dark))  :background "#cd00cd")
    (((class color) (min-colors 8)   (background dark))  :background "#cd00cd") ; magenta
    (t :inverse-video t))
  "Face for highlighting erroneous whitespace."
  :group 'whitespace)

;; force specific foreground colors for comments and strings so they are
;; consistent between 4- and 8-bit environments.  without this, we're at
;; the mercy of some *strange* defaults (comments as "chocolate"?!?).
;;
;; NOTE: we don't get fancy and specify colors for each of the bit depths
;;       since we choose colors that exist in all.
;;
(set-face-foreground 'font-lock-comment-face "red")
(set-face-foreground 'font-lock-string-face "green")

;; underline warnings and errors from Flymake rather than provide a potentially
;; clashing face.  this also provides a simple, uncolored underline effect on
;; the terminal where the underline cannot be colored.
(custom-set-faces
 '(flymake-errline  ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

;; =========================== Notifications =================================

;; disable the warning about the dangers of narrowed buffers - we understand
;; what they are and how to widen them.
(put 'narrow-to-region 'disabled nil)

;; under no circumstances should the bell be rung.  period.
(setq ring-bell-function (lambda () nil))

;; don't warn the user about killing text that is read-only.  just do what they
;; want.
(setq kill-read-only-ok t)

;; tighten up the displayed date, time, and load so that we don't waste
;; mode line space.  the default includes the day name which we should
;; either know or could find out elsewhere.
(setq display-time-string-forms
      '(monthname day " " 24-hours ":" minutes ":" seconds " [" load "]"))

;; ensure we can see what column the point is at.
(column-number-mode 1)

;; show the name of the function the point is in for context.
(which-func-mode t)

;; enable highlighting matching parentheses/brackets/braces.
(show-paren-mode t)

;; how far back should emacs look for a matching paranethesis?
(setq show-paren-style 'parenthesis)
(setq blink-matching-paren-distance 51200)

;; make duplicate buffer names unique by appending a suffix derived from the
;; directory containing the visited file.  this replaces "Makefile<2>" with
;; "Makefile|directory-name" which is much easier to identify which file the
;; buffer represents.
(setq uniquify-buffer-name-style 'post-forward)

;; skip straight to the *scratch* buffer after startup.
(setq inhibit-startup-message t)

;; don't implicitly trust buffer local variables.  ask the user for guidance
;; whenever we encounter one.
(setq enable-local-variables 'query)
(setq enable-local-eval 'query)

;; switch all "yes or no" prompts into "y or n" so we don't have to type as
;; much.
(fset 'yes-or-no-p 'y-or-n-p)

;; when possible, preserve the screen position when scrolling.  without this
;; the scrolling up and then down will not necessary be a no-op.
(setq scroll-preserve-screen-position t)

;; never create newlines when navigating through a buffer via the
;; keyboard.  those must be explicitly added by the user.
(setq next-line-add-newlines nil)

;; truncate long lines and indicate that has been done.
(setq-default truncate-lines t)

;; ================================ Whitespace ===============================

;; tabs are evil.  make sure we don't inadvertently create/save files with them
;; in there.  the approach below is derived from here:
;;
;;    https://www.emacswiki.org/emacs/UntabifyUponSave

;; should we encounter a file with tabs, treat them as four spaces.
(setq default-tab-width 4)

;; prevent tabs from being inserted.  only files whose mode explicitly sets
;; this flag can introduce tabs (e.g. makefile-mode).  this prevents Emacs
;; from introducing tabs into editted files.
(setq-default indent-tabs-mode nil)

;; highlight erroneous whitespace that we don't prefer while editing source
;; code.  literal tab characters and trailing whitespace are highlighted
;; obnoxiously so we can remove them, while long lines are rendered more
;; friendly since we can't always do something about them.
(setq whitespace-style '(face empty lines-tail tabs trailing))
(setq whitespace-line-column fill-column)

;; XXX: make this conditional on v24.4 - can set the variables before,
;;      need to update the faces after.
(setq whitespace-space 'highlight-whitespace)
(setq whitespace-tab   'highlight-whitespace)

(global-whitespace-mode t)

;; always terminate files with a newline.
(setq require-final-newline t)

;; disable electric indent (e.g. RET indents the current line, inserts a new
;; line, *AND* indents the next line) on newer versions of Emacs.  v24.4 decided
;; to make this a default which makes less-popular modes obnoxious to use.
;; modes like F90 and MATLAB have typically been configured with minimal effort
;; so they get out of the way so we can work, and usually include avoiding any
;; sort of complex indentation configuration.  disabling this skips investing
;; energy in these packages unless someone is *really* motivated.
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;; ============================ Graphical Emacs ==============================
;;
;; Configure Emacs when run outside of a terminal.  This enables inline image
;; viewing, configures the fringe and cursor for identifying errant whitespace,
;; and saves space by turning off the toolbar.

;; disable the graphical toolbar.  the goal is to not touch the mouse and
;; buttons we'll never use simply waste space.
;;
;; NOTE: we do this regardless of whether Emacs reports graphics capabilities
;;       since a terminal mode Emacs can spawn graphical frames via emacsclient.
;;       this ensures said graphical clients have the toolbar disabled if
;;       they're spawned.
(if (functionp 'tool-bar-mode)
  (tool-bar-mode 0))

(when (display-graphic-p)
  ;; add a distinct marker to the fringe next to empty lines.  note that this
  ;; is a buffer-local variable.
  (setq-default indicate-empty-lines t)

  ;; view images inline if support was compiled in.
  (if (display-images-p)
      (auto-image-file-mode t))

  ;; expand the cursor to width of the glyph beneath it.  this is useful for
  ;; identifying tabs, though only in graphical modes.
  (setq x-stretch-cursor t)

  ;; middle clicks (button 2) paste at the point not at the mouse click.
  (setq mouse-yank-at-point t)
  )

;; ============================= Terminal Emacs ==============================
;;
;; Configure Emacs when run inside of a terminal.  This disables the menu bar
;; at the top to gain an extra line of space.

(when (not (window-system))
  ;; disable the menu bar.
  (menu-bar-mode -1)

  ;; remap GNU Screen's TERM value to something Emacs understands.
  (add-to-list 'term-file-aliases
               '("screen.xterm-256color" . "xterm-256color"))
)
