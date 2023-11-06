#!/bin/sh

# simple script to install user configuration files via symbolic links.
# existing configurations are backed up to
# ${HOME}/.user-configuration-backup/YYYYMMDD/.

print_usage()
{
    echo "Usage: $0 [-h] [<install_root>]"
    echo
    echo "Simple installer that backs up existing user configurations in ${HOME}"
    echo "and symbolically links configurations found beneath <install_root>.  Backups"
    echo "of existing configurations are made in a date-specific directory beneath"
    echo "${BASE_BACKUP_PATH} with the form:"
    echo
    echo "    ${BASE_BACKUP_PATH}/YYYYMMDD-<counter>"
    echo
    echo "Where <counter> is the first integer that causes the backup path to not"
    echo "exist during initialization."
    echo
    echo "Care is taken to not lose configuration files and the script will exit"
    echo "prior to taking an action that could lose data.  Specifically, if a suitable"
    echo "backup directory cannot be identified or if backing up a file fails"
    echo "an error message is printed and execution halted."
    echo
    echo "The command line options shown above are described below:"
    echo
    echo "    -h            Display this help message and exit."
    echo
}

# takes two arguments, the type of path to check and a whitespace delimited
# list of paths to check.  the former must be either "file" or "directory"
# to check for paths of said type.  echoes a whitespace delimited list of
# paths that did not exist, which is empty on success.
verify_paths()
{
    PATH_TYPE="$1"
    PATHS_TO_CHECK="$2"

    # whitespace delimited list of paths that are missing.  this is empty
    # when all paths provided exist and are of the expected type.
    MISSING_PATHS=

    # iterate through the paths and identify the ones missing.
    for PATH_TO_CHECK in ${PATHS_TO_CHECK}; do
        RESULT_FLAG="no"

        # check for existence, while respecting what type of path we're
        # expecting.
        if [ "${PATH_TYPE}" = "file" ]; then
            if [ -f "${PATH_TO_CHECK}" ]; then
                RESULT_FLAG="yes"
            fi
        elif [ "${PATH_TYPE}" = "directory" ]; then
            if [ -d "${PATH_TO_CHECK}" ]; then
                RESULT_FLAG="yes"
            fi
        fi

        # add this path to the list if it didn't exist as we expected.
        if [ "${RESULT_FLAG}" = "no" ]; then
            MISSING_PATHS="${MISSING_PATHS} ${PATH_TO_CHECK}"
        fi
    done

    echo "${MISSING_PATHS}"
}

# creates a backup directory and echoes its location on standard output.
# if a suitable directory cannot be found, nothing is output.
create_backup_directory()
{
    # todays date in YYYYMMDD format.
    TODAY_YYYYMMDD=`date +%Y%m%d`

    # maximum number of directories to check in today's backup directory.
    # if all of these exist, we bail and let an adult take care of things.
    NUMBER_BACKUPS="10"

    # create the top-level backups directory if it doesn't already exist.
    mkdir -p ${BASE_BACKUP_PATH}

    # walk through each of today's backups and find the first one that
    # doesn't exist.
    #
    # NOTE: this is susceptible to a time-of-check to time-of-use (TOCTTOU)
    #       attack because the sequence of directories we check is known.
    #       that said, this isn't run outside of a user-controlled directory
    #       nor do we care to jump through hoops to securely create a temporary
    #       directory, populate it, and move it into the correct location.
    #
    FOUND_BACKUP_FLAG="no"
    for BACKUP_COUNTER in `seq 1 ${NUMBER_BACKUPS}`; do
        BACKUP_PATH="${BASE_BACKUP_PATH}/${TODAY_YYYYMMDD}-${BACKUP_COUNTER}"

        if [ -d "${BACKUP_PATH}" ]; then
            continue
        else
            FOUND_BACKUP_FLAG="yes"
            break
        fi
    done

    if [ "${FOUND_BACKUP_FLAG}" = "no" ]; then
        echo "Could not find a suitable backup directory beneath ${BASE_BACKUP_PATH}." >&2
        echo "Tried ${NUMBER_BACKUPS} many sub-directories before giving up.  Remove one of" >&2
        echo "them and try again." >&2
        echo
        return
    fi

    # create the backup directory we're using.
    mkdir ${BACKUP_PATH}

    echo ${BACKUP_PATH}
}

# so we're able to run the same script multiple times without accidentally
# removing a user's configurations we look for a new date-based directory
# appended with a counter.  the specific sub-directory beneath this base
# is determined by create_backup_directory().
BASE_BACKUP_PATH=${HOME}/.user-configuration-backups

# list of directories and files, relative to the repository root, containing
# configuration that should be linked to.
CONFIG_DIRECTORIES=".bashrc.d
                    .emacs.d
                    octave"
CONFIG_FILES=".bashrc
              .cuda-gdbinit
              .emacs
              .gdbinit
              .gitconfig
              .gittemplate
              .octaverc
              .screenrc
              .screenrc-code
              .Xresources
              .xxdiffrc"

while getopts "h" FLAGS
do
    case ${FLAGS} in
        h)
            print_usage
            exit 0
            ;;
    esac
done

# skip over any command line options provided.
shift `expr ${OPTIND} - 1`

# ensure we were called with the right number of arguments.
if [ $# -gt 1 ]; then
    echo "Incorrect number of arguments.  Expected at most 1, but received $#." >&2

    print_usage
    exit 1
fi

INSTALL_ROOT="$1"

# assume we're installing from the current directory if the caller did not tell
# us where to install from.
if [ -z "${CONFIG_ROOT}" ]; then
    CONFIG_ROOT="."
fi

# ensure the directory we're installing from exists.
if [ ! -d "${CONFIG_ROOT}" ]; then
    echo "The supplied configuration root (${CONFIG_ROOT}) does not exist.  Exiting." >&2
    exit 1
fi

# normalize the configuration path so we can correctly create symbolic links
# regardless of where we're run from.
CONFIG_ROOT=`realpath -q ${CONFIG_ROOT}`

# verify everything is present to install.
MISSING_FILES=`verify_paths "file" "${CONFIG_FILES}"`
MISSING_DIRECTORIES=`verify_paths "directory" "${CONFIG_DIRECTORIES}"`

if [ -n "${MISSING_FILES}" -o -n "${MISSING_DIRECTORIES}" ]; then
    echo "Aborting installation due to an incomplete configuration in ${CONFIG_ROOT}." >&2
    echo >&2

    if [ -n "${MISSING_FILES}" ]; then
        echo "The following configuration files are missing:" >&2
        echo >&2
        for MISSING_FILE in ${MISSING_FILES}; do
            echo "    ${MISSING_FILE}" >&2
        done
        echo >&2
    fi
    if [ -n "${MISSING_DIRECTORIES}" ]; then
        echo "The following configuration directories are missing:" >&2
        echo >&2
        for MISSING_DIRECTORY in ${MISSING_DIRECTORIES}; do
            echo "    ${MISSING_DIRECTORY}" >&2
        done
        echo >&2
    fi

    echo "Please restore the missing files and try again." >&2
    exit 1
fi

# create the backup directory.
BACKUP_DIRECTORY=`create_backup_directory`
if [ -z "${BACKUP_DIRECTORY}" ]; then
    exit 1
fi

for CONFIG_PATH in ${CONFIG_DIRECTORIES} ${CONFIG_FILES}; do
    # create each of the full paths we need to install this configuration file.
    SOURCE_CONFIG_PATH="${CONFIG_ROOT}/${CONFIG_PATH}"
    TARGET_CONFIG_PATH="${HOME}/${CONFIG_PATH}"
    BACKUP_CONFIG_PATH="${BACKUP_DIRECTORY}/${CONFIG_PATH}"

    if [ -e "${TARGET_CONFIG_PATH}" ]; then
        # ensure that the directory we're backing up to exists.
        BACKUP_PARENT_DIRECTORY=`dirname "${BACKUP_CONFIG_PATH}"`
        if [ ! -d "${BACKUP_PARENT_DIRECTORY}" ]; then
            mkdir -p "${BACKUP_PARENT_DIRECTORY}"

            #
            # NOTE: we don't check the status here as the move below
            #       will fail if this was required.
            #
        fi

        mv "${TARGET_CONFIG_PATH}" "${BACKUP_CONFIG_PATH}"

        # bail out if we could not backup this configuration.  we don't want to
        # blindly continue and potentially obliterate a user's previous
        # configuration.
        if [ $? -ne 0 ]; then
            echo "Failed to move '${CONFIG_PATH}' into the backup directory (${BACKUP_DIRECTORY})." >&2
            echo "Exiting so as to avoid losing user configuration." >&2
        fi
    fi

    echo "Installing '${CONFIG_PATH}'."
    ln -s ${SOURCE_CONFIG_PATH} ${TARGET_CONFIG_PATH}
done

echo "Success!  Previous configurations are stored beneath '${BACKUP_DIRECTORY}'."
