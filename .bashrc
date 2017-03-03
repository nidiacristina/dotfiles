# location of our configuration fragments.  contains sourceable shell scripts
# whose names are of the form:
#
#   NN-name.sh
#
# all available fragments are sorted and then sourced to build the final
# environment.

FRAGMENTS_DIRECTORY="${HOME}/.bashrc.d"

if [ -d "${FRAGMENTS_DIRECTORY}" ]; then
    # get the full path of each fragment.
    #
    # NOTE: we need the trailing slash to ensure we search fragment
    #       directories that are symbolic links to another directory.
    #
    FRAGMENTS=`find -L "${FRAGMENTS_DIRECTORY}/" -type f -name '[0-9][0-9]-*.sh' -exec readlink -f {} \; | sort`

    for FRAGMENT in ${FRAGMENTS}; do
        . ${FRAGMENT}
    done
fi
