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

# flag indicating whether prototypes have been loaded.
set $loaded_prototypes = 0

# signatures for functions we interactively call on a regular basis.  we wrap
# these in a command so it is loaded at the user's discretion rather than
# unconditionally.  this let's us define signatures with data types that don't
# exist until a shared object is loaded.
define load_prototypes
    # only load the prototypes if we haven't already.
    if $loaded_prototypes == 0
        set $malloc = (void*(*)(size_t)) malloc
        set $dlopen = (void*(*)(char*, int)) dlopen

        set $loaded_prototypes = 1
    end
end

document load_prototypes
Usage: load_prototypes
n
Loads generic prototypes so functions can be invoked like so:

  (gdb) set $buffer = $malloc( 2048 )

end
