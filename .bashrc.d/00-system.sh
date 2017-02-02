# pull in the system's configuration first, and then overlay our changes.
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi
