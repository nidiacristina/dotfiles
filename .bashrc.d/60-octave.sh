# load a non-standard version of Octave into our environment, though only if
# it has been installed.
OCTAVE_DIR=/opt/octave/4.2.1

if [ -d "${OCTAVE_DIR}" ]; then
    export PATH="${OCTAVE_DIR}":/bin:${PATH}
    export MANPATH="${OCTAVE_DIR}":/man:${MANPATH}
fi
