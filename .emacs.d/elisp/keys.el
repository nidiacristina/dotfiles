;; Keybindings.  This file is grouped into two pieces: 1) global key bindings
;; and 2) mode-specific bindings.

;; =============================== Development ===============================
;;
;;    M-k           Compile the current buffer and view the output.
;;     %            Jump to matching parenthesis/bracket/brace.
;;    C-c o         Open a buffer containing all lines matching a search.
;;    M-i           Read a man page.
;;    C-x .         Jump to a symbol's definition.
;;    C-x ,         Search for all symbols containing the specified search.
;;

;; compile the current buffer and have the compilation's output in another
;; window.
(global-set-key (kbd "M-k") 'compile)

;; mimic vi's parentheses matching behavior.
(global-set-key (kbd "%") 'find-matching-paren)

;; open another buffer with hyperlinks to every line that matches the user
;; specified search term.
(global-set-key (kbd "C-c o") 'occur)

;; read a man page, defaulting to the symbol under the point.
(global-set-key (kbd "M-i") 'man)

;; the xref package is the way of the future so use our interactive shim to
;; maintain the legacy etags interace's behavior of prompting for a symbol.
;; if the package isn't available, then the key is already bound to what we
;; want.
(if (fboundp 'interactive-xref-find-definitions)
      (progn (global-set-key (kbd "M-.") 'interactive-xref-find-definitions)
             (global-set-key (kbd "M-,") 'xref-find-apropos)
             (global-set-key (kbd "M-*") 'xref-pop-marker-stack))
    (progn (global-set-key (kbd "M-,") 'tags-apropos)
           (global-set-key (kbd "M-*") 'pop-tag-mark)))

;; ============================= Formatting ==================================
;;
;; Let us worry about getting ideas into the computer quickly and then have
;; Emacs make them look nice.
;;
;;     C-x M-f      Fill the current region.
;;     C-x M-q      Align the current region.
;;

;; wrap the text in the current region.
(global-set-key (kbd "C-x M-f") 'fill-region)

;; align the current text (e.g. variable assignments aligned about a "=")
(global-set-key (kbd "C-x M-q") 'align)

;; =========================== Buffer Management =============================
;;
;; Make it easy to get to the buffer we're interested in.
;;
;;    C-o           Switch to this window's other-buffer.
;;    C-x M-b       Bury this window's buffer.
;;

;; switch to this window's other buffer.
(global-set-key (kbd "C-o")
                (lambda ()
                  (interactive)
                  (switch-to-buffer (other-buffer))))

;; send this buffer to the end of the list.
(global-set-key (kbd "C-x M-b") 'bury-buffer)

;; ======================= Window and Frame Navigation ======================
;;
;; Provide single key movement through windows/frames using the "o" and "p"
;; keys.
;;
;;    M-o           Activate next window
;;    M-p           Activate previous window
;;    C-M-o         Activate next frame
;;    C-M-p         Activate previous frame
;;

;; make the next/previous window active.
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-p") 'other-window-backward)

;; make the next frame active.
(global-set-key (kbd "C-M-o")
                (lambda ()
                  (interactive)
                  (other-frame 1)))

;; make the previous frame active.
(global-set-key (kbd "C-M-p")
                (lambda ()
                  (interactive)
                  (other-frame -1)))

;; ============================ Buffer Navigation ============================
;;
;; Moving through a buffer.
;;
;;    M-g           Jump to a specific line.
;;    mouse-5       Scroll the buffer up half a page.
;;    mouse-4       Scroll the buffer down half a page.
;;

;; move to a line by line number.
(global-set-key (kbd "M-g") 'goto-line)

;; the mouse wheel should behave as it does in most applications.
(when (display-graphic-p)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-half-page)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-half-page))

;; ============================= Miscellaneous ===============================
;;
;; Grab bag of bindings.  Prevent graphical Emacs from iconifying itself or
;; terminal Emacs from suspending itself.  Prevent an errant key press from
;; exiting the application if there aren't any modified buffers.
;;
;;    C-z           Unbound.
;;    C-x C-z       Unbound.
;;    C-x C-c       Quitting Emacs asks to save buffers first.
;;

;; never iconify or suspend a frame.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; ask before we quit.
(global-set-key (kbd "C-x C-c") 'ask-save-quit)

;; ===========================================================================
;;
;; Everything beyond here are mode-specific key bindings.  These should always
;; come last as they may depend upon the global configuration setup above.

;; ================================ Diff Mode ================================
;;
;;    M-N           Next hunk.
;;    M-P           Previous hunk.

;; diff-mode is a bit zealous in mapping its functionality to keys.  several of
;; the movement functions (diff-file-next/prev, diff-hunk-next/prev) are mapped
;; multiple times and mask global bindings we have set above.  this ensures
;; diff-mode behaves consistently relative to other modes.
(eval-after-load "diff-mode"
  '(progn
     ;; prevent masking of our window movement key bindings.
     (define-key diff-mode-map (kbd "M-o") nil)
     (define-key diff-mode-map (kbd "M-p") nil)

     ;; since it's partner was just unbound, remove the unmatched diff-file-next
     ;; as well.  you know, for symmetry's sake.
     (define-key diff-mode-map (kbd "M-n") nil)

     ;; map movement through hunks to the upper case version.  this ensures that
     ;; we have both forward and backward movement through a patch.  these keys
     ;; are duplicates of M-{ and M-}, aka diff-file-prev and diff-file-next,
     ;; respectively, so we're not losing functionality.
     (define-key diff-mode-map (kbd "M-N") 'diff-hunk-next)
     (define-key diff-mode-map (kbd "M-P") 'diff-hunk-prev)
  ))

;; ============================== Makefile Mode ==============================
;;
;;    M-}           Next dependency.
;;    M-{           Previous dependency.

;; to maintain consistency with our global movement keys, we move the
;; next/previous dependency functions.
(add-hook 'makefile-mode-hook
          (lambda ()
            ;; move the dependency navigation keys to something else typically
            ;; associated with next/previous.
            (local-set-key   (kbd "M-{") 'makefile-previous-dependency)
            (local-set-key   (kbd "M-}") 'makefile-next-dependency)

            ;; remove the binding for our key to move to the previous window.
            (local-unset-key (kbd "M-p"))
            ))

;; ================================ Man Mode =================================
;;
;;                          No new keys are defined.

;; Man-mode doesn't have a keymap so we need to remove the key each time
;; we enter the mode.
(add-hook 'Man-mode-hook
          (lambda ()
            ;; do not mask moving between windows when working with manual
            ;; pages.  we rarely have more than one man page open at a time so
            ;; the default binding for changing to the previous page is less
            ;; than useful.
            (local-unset-key (kbd "M-p"))
            ))

;; ============================== Comint Mode ================================
;;
;;    M-P           Previous input.
;;
;; NOTE: A number of modes inherit from comint-mode, so this key binding affects
;;       the GUD (GDB's shell and I/O), shells, etc.

;; keep our other-window-backward binding and move the previous input binding to
;; the upper case version.  we don't have C-p for the previous input anyway
;; (which is common with readline-enabled applications, so this is close enough.
(define-key comint-mode-map (kbd "M-p") nil)
(define-key comint-mode-map (kbd "M-P") 'comint-previous-input)

;; ============================ Compilation Mode =============================
;;
;;    M-N           Next error.
;;    M-P           Previous error.

;; unmask our buffer navigation and window navigation keys when we're navigating
;; compilation messages.  we move the compilation error navigation keys to the
;; upper case version of the original so they're still available.
;;
;; XXX: for the life of me I can't figure out how to modify compilation-mode-map
;;      and have it be noticed.  as a result, we install a hook that fixes up
;;      each buffer's local key map.
(add-hook 'compilation-mode-hook
      (lambda ()
        ;; unmask the buffer navigation and window movement keys.
        (local-unset-key (kbd "C-o"))
        (local-unset-key (kbd "M-n"))
        (local-unset-key (kbd "M-p"))

        ;; rebind the error navigation functions to something
        ;; similar to the original.
        (local-set-key (kbd "M-N") 'compilation-next-error)
        (local-set-key (kbd "M-P") 'compilation-previous-error)
        ))

;; ============================ Log Edit Mode ================================
;;
;;    M-N           Next log message.
;;    M-P           Previous log message.
;;

(eval-after-load "log-edit"
  '(progn
     ;; unmask our window navigation key and it's companion (for completeness).
     (define-key log-edit-mode-map (kbd "M-p") nil)
     (define-key log-edit-mode-map (kbd "M-n") nil)

     ;; rebind the log message navigation to the uppercase versions.
     (define-key log-edit-mode-map (kbd "M-P") 'log-edit-previous-comment)
     (define-key log-edit-mode-map (kbd "M-N") 'log-edit-next-comment)
     ))

;; =============================== Octave Mode ===============================
;;
;;                          No new keys are defined.

;; octave-mode doesn't have a keymap so we need to remove the key each time we
;; enter the mode.
(add-hook 'octave-mode-hook
          (lambda ()
            ;; ignore octave-mode's function lookup binding as ours is better
            ;; (read: language agnostic).
            (local-unset-key (kbd "M-."))
            ))
