# assume there is always an instance of Emacs running on the system.  use that
# to edit files instead of firing up a new instance as needed.
export EDITOR="emacsclient -nw"

# make sure we invoke our editor consistently, even if we call it directly.
alias emacsclient="${EDITOR}"
