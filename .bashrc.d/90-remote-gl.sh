# load TurboVNC and VirtualGL so we can provide a 3D environment, though only
# if they have been installed.
TURBOVNC_DIR=/opt/TurboVNC
VIRTUALGL_DIR=/opt/VirtualGL

if [ -d "${TURBOVNC_DIR}" ]; then
    export PATH=${PATH}:"${TURBOVNC_DIR}"/bin
    export MANPATH=${MANPATH}:"${TURBOVNC_DIR}"/man
fi

if [ -d "${VIRTUALGL_DIR}" ]; then
    export PATH=${PATH}:"${VIRTUALGL_DIR}"/bin
fi
