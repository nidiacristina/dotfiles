;; Keybindings.

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
