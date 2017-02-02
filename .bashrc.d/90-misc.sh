# use I-Bus as our input method.  this allows us to change languages across
# the entire X session.
export QT_IM_MODULE=ibus
export GTK_IM_MODULE=ibus

# configure Wine to run as a 32-bit instance and use a different root than
# default.
export WINEARCH=win32
export WINEPREFIX="${HOME}/.wine32"
