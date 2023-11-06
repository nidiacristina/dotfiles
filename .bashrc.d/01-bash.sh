# remove programmatic completion so tab-completing lines with variable
# references aren't expanded so the references are replaced with the variables'
# values.  this means that tab-completing the following:
#
#   $ ln -s ${HOME}/.bashr<TAB>
#
# completes to:
#
#   $ ln -s ${HOME}/.bashrc
#
# and *not*:
#
#   $ ln -s \$\{HOME\}/.bashrc
#
# NOTE: this is a *BIG* hammer to pick a small nit.  that said, I can't
#       figure out how to disable character quoting without expanding
#       variables during tab-completion.
#
shopt -u progcomp
