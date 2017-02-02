# add user-specific binaries into the path when they exist.
if [ -d "${HOME}/bin" ]; then
    export PATH=${PATH}:${HOME}/bin
fi
