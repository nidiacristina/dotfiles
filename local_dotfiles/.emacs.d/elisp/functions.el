;; Local function definitions.

;; =============================== Development ===============================

;; move the point to the matching parenthesis/bracket/brace if it's on one,
;; otherwise insert a literal "%".  this emulates the behavior from Vi(m).
(defun find-matching-paren (arg)
  "Find and go to matching paren, if currently on paren. Otherwise, insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; git.el is considered a simple porcelain implementation and does not support
;; commit templates.  this appends it to the log buffer when it exists.
(eval-after-load "git-mode"
  (defadvice git-setup-log-buffer (after insert-commit-template activate compile)
    "Inserts the Git commit template into the log buffer if it exists."
    (with-current-buffer buffer
      (let ((template-path (git-config "commit.template")))
        (when (and template-path (file-exists-p template-path))
          (insert-file-contents template-path))))))

;; gitsum hasn't been updated since it came out in 2008 and does not support
;; commit templates either.  this prepends it to the log buffer when it exists.
(defadvice gitsum-commit (after gitsum-insert-commit-template activate compile)
  "Inserts the Git commit template into the log buffer if it exists."
  (let ((buffer (get-buffer-create "*gitsum-commit*")))
    (with-current-buffer buffer
      (let ((template-path (git-config "commit.template")))
        (when (and template-path (file-exists-p template-path))
          (insert-file-contents template-path))))))

;; describe the Lisp symbol at, or near, the point.  checks the symbol beneath
;; the point to see if its a function or a variable and describes it.  if the
;; point isn't on a symbol, but is in a function call, the surrounding call is
;; described instead.
;;
;;  originally from:
;;
;;    https://www.emacswiki.org/emacs/DescribeThingAtPoint
;;
(defun describe-symbol-at-point ()
  "Show the documentation of the Elisp function and variable near point.

This checks in turn for:
-- a function name where point is
-- a variable name where point is
-- a surrounding function call
"
  (interactive)
  (let (sym)
    (cond
     ;; function at point.
     ;;
     ;; NOTE: stolen from function-at-point so that it only identifies what is
     ;;       at the point.
     ((setq sym (ignore-errors
                  (with-syntax-table emacs-lisp-mode-syntax-table
                    (save-excursion
                      (or (not (zerop (skip-syntax-backward "_w")))
                          (eq (char-syntax (char-after (point))) ?w)
                          (eq (char-syntax (char-after (point))) ?_)
                          (forward-sexp -1))
                      (skip-chars-forward "`'")
                      (let ((obj (read (current-buffer))))
                        (and (symbolp obj) (fboundp obj) obj))))))
      (describe-function sym))
     ;; variable at point.
     ((setq sym (variable-at-point)) (describe-variable sym))
     ;; function in surrounding sexp.
     ((setq sym (function-at-point)) (describe-function sym)))))

;; =========================== Buffer Management =============================

;; I never switch to a non-existent buffer by name to create it (I'll just visit
;; a temporary file on disk) so make it so we can't accidentally do that.
(defadvice switch-to-buffer (before existing-buffer activate compile)
  "When interactive, switch to existing buffers only, unless given a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: "
                      (other-buffer)
                      (null current-prefix-arg)))))

;; ======================= Window and Frame Navigation ======================

;; wrapper to move backward through the window stack.
(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

;; ============================ Buffer Navigation ============================

;; basic scrolling functions.  intended as helpers to make the mouse wheel
;; do what we expect.
(defun scroll-up-half-page ()
  "Scroll up half a page."
  (interactive)
  (scroll-up (/ (window-height) 2)))

(defun scroll-down-half-page ()
  "Scroll down half a page."
  (interactive)
  (scroll-down (/ (window-height) 2)))

;; =============================== Wrappers ==================================

;; wrap saving and quitting with a question.
(defun ask-save-quit ()
  "Asks if the editor should be quit, and if there are unsaved buffers, asks
   to save each buffer before quitting."
  (interactive)
  (cond ((y-or-n-p "Quit editor? ")
         (save-buffers-kill-emacs))))

;; provide a wrapped version of xref-find-definitions that behaves as if it
;; was invoked with the universal argument.
;;
;; NOTE: I'm sure there is a better way of doing this with advice-add though
;;       I'm not enough of an Emacs guru to see it.
(when (fboundp 'xref-find-definitions)
  (defun interactive-xref-find-definitions ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'xref-find-definitions))))

;; ============================= Miscellaneous ===============================

;; provide an alias we can easily type when Emacs gets confused about the
;; buffer's decorations.
(defalias 'rfont 'font-lock-fontify-buffer)

;; convert tabs to spaces throughout the entire buffer.
(defun untabify-buffer ()
  "Convert tabs to spaces in this buffer."
  (interactive)
  (if (not indent-tabs-mode)
      (untabify (point-min) (point-max)))
  nil)

;; visit what the symlink targets rather than clobbering the symlink
;; XXX: how is this different from find-file-visit-truename?
(defun visit-target-instead ()
  "Replace this buffer with a buffer visiting the link target."
  (interactive)
  (if buffer-file-name
      (let ((target (file-symlink-p buffer-file-name)))
        (if target
            (find-alternate-file target)))
    (error "Not visiting a file")))
