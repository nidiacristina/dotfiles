# support one of Anaconda's Python installers to provide Python3.  we prefer
# Anaconda over Miniconda if both are installed.
if [ -d "${HOME}/anaconda3/bin" ]; then
    export PATH="${HOME}/anaconda3/bin:${PATH}"
elif [ -d "${HOME}/miniconda3/bin" ]; then
    export PATH="${HOME}/miniconda3/bin:${PATH}"
fi
