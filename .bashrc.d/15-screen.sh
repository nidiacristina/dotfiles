# NOTE: this fragment needs to be sourced after editors have been configured.

# setup a directory to keep our screen sessions in that is only accessible to
# the current user.
export SCREENDIR="${HOME}/.screendir"

# if the directory doesn't exist, create it with a suitably restrictive mode.
if [ ! -d "${SCREENDIR}" ]; then
   mkdir "${SCREENDIR}"
   chmod 700 "${SCREENDIR}"
fi

# wrap the screen command so that we can pull up different configurations
# based on the task at hand.  this wrapper supplements the normal options
# and arguments that screen accepts and provides this interface:
#
#   $ screen <config> <session>
#
# this allows instantiation of a pre-defined configuration
# (.screenrc-<config>) with a given session name and aims to simplify common
# development patterns (e.g. window #1 runs Emacs, window #2 is a shell,
# window #3 is top, etc).  when <session> is supplied, it is also provided via
# the EMACS_SERVER environment variable so emacs/emacsclient can communicate
# within the session.
function screen()
{
    CONFIG_NAME=$1
    SESSION_NAME=$2

    # NOTE: we use a full path rather than relying on the shell to avoid
    #       aliases that allow identification of aliases and functions
    #       of the same name.  this is a problem on Fedora 24 where 'which'
    #       is aliased to be "helpful".
    SCREEN=`/usr/bin/which screen`
    SCREENRC="${HOME}/.screenrc"

    if [ -n "${CONFIG_NAME}" ]; then
        SCREENRC="${SCREENRC}-${CONFIG_NAME}"

        # we have a configuration file, specify a name for the screen either
        # using what the user specified or based off of the configuration.
        if [ -n "${SESSION_NAME}" ]; then
        EMACS_SERVER="${SESSION_NAME}"
            SESSION_NAME="-S ${SESSION_NAME}"
        else
            SESSION_NAME="-S ${CONFIG_NAME}"
        fi

        # if the configuration requested doesn't exist, simply pass everything
        # provided to Screen itself.  otherwise, run our constructed command.
        if [ ! -f "${SCREENRC}" ]; then
            # this typically handles command line options passed directly
            # to screen like "-ls" or "-dRR".
            COMMAND="${SCREEN} $*"
        else
            COMMAND="EMACS_SERVER=${EMACS_SERVER} ${SCREEN} -c ${SCREENRC} ${SESSION_NAME}"
        fi
    fi

    eval ${COMMAND}
}

# configure our editors to use the Emacs instance we've setup for this screen
# session.

# emacsclient to point to a specific instances of emacs when it exists.
#
# NOTE: this is only used inside of a new screen instance, not regular
#       interactive shells.
if [ -n "${EMACS_SERVER}" ]; then
    EMACSCLIENT_COMMAND="emacsclient -s '${EMACS_SERVER}'"
    alias emacsclient=${EMACSCLIENT_COMMAND}

    export EDITOR="${EMACSCLIENT_COMMAND}"
fi
