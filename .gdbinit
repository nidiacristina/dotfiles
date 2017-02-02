# Configuration file for GDB.

# we're experienced enough with GDB to not worry about accidentally doing
# the irreversible.
set confirm off

# improve the presentation of queried data.
set print array on
set print pretty

# most everything I debug has threads hidden in a black box, so disable
# announcements of thread events to reduce clutter.
set print thread-events off

# assume we're working with C if we can't detect something more specific.
set language c

# save our command history between sessions.
set history filename ~/.gdb_history
set history save
