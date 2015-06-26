# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs
export SVN_EDITOR=vi
alias em='emacsclient -nc "$@" -a ""'
