######################################################################
##   ██████╗ ███╗   ██╗██╗   ██╗    ██████╗  █████╗ ███████╗██╗  ██╗
##  ██╔════╝ ████╗  ██║██║   ██║    ██╔══██╗██╔══██╗██╔════╝██║  ██║
##  ██║  ███╗██╔██╗ ██║██║   ██║    ██████╔╝███████║███████╗███████║
##  ██║   ██║██║╚██╗██║██║   ██║    ██╔══██╗██╔══██║╚════██║██╔══██║
##  ╚██████╔╝██║ ╚████║╚██████╔╝    ██████╔╝██║  ██║███████║██║  ██║
##   ╚═════╝ ╚═╝  ╚═══╝ ╚═════╝     ╚═════╝ ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝
######################################################################
### Note: the font above is ANSI Shadow

# =========================
# ====== WHO'S THAT? ======
# =========================
# Riccardo Mura created and uses this configuration file


###############################
# ============================
# ====== SHELL SETTINGS ======
# ============================
###############################

shopt -s globstar


# ========================
# ====== DEBUGGING =======
# ========================

set +x

# Uncomment this line in order to debug this file
#set -x

# ======================
# ====== SOURCING ======
# ======================

sources_files=(
    /etc/bashrc
    ~/.addenda
    ~/aspnetcore_cert_info
    ~/.cargo/env
)

for source in "${source_files[@]}"
do
    if [ -f "$source" ] ; then
        . "$source"
    fi
done

# Better to handle .Xresources apart
if [ -f ~/.Xresources ] ; then
    xrdb ~/.Xresources
fi

SHELL_DIR=~/.shell
. $SHELL_DIR/{common,bash}/session/*.sh
. $SHELL_DIR/{common,bash}/os/*.sh
. $SHELL_DIR/{common,bash}/environment/*.sh
. $SHELL_DIR/{common,bash}/functions/*.sh
. $SHELL_DIR/{common,bash}/aliases/*.sh
. $SHELL_DIR/{common,bash}/auto/*.sh


# =====================
# ====== INPUTRC ======
# =====================
### This replaces directives usually specified in ~/.inputrc

### Key bindings
bind '"\e[28~":""'
bind '"\C-p": history-search-backward'
bind '"\C-n": history-search-forward'
bind '"\C-f": forward-char'
bind '"\C-b": backward-char'
bind '"\C-H": shell-backward-kill-word' # Ctrl-Backspace in alternative to Ctrl-W

### Deprecated key bindings (kept because who knows...)
#bind '"\e[A": history-search-backward' # up arrow
#bind '"\e[B": history-search-forward' # down arrow
#bind '"\e[C": forward-char' # right arrow
#bind '"\e[D": backward-char' # left arrow

# Say no to annoying beeps/flashes!
bind 'set bell-style none'

### See https://www.topbug.net/blog/2017/07/31/inputrc-for-humans/
bind 'set colored-stats On'
bind 'set completion-ignore-case On'
bind 'set completion-prefix-display-length 3'
bind 'set mark-symlinked-directories On'
bind 'set show-all-if-ambiguous On'
bind 'set show-all-if-unmodified On'
bind 'set visible-stats On'


# =====================
# ====== HISTORY ======
# =====================

HISTSIZE=1000000
HISTFILESIZE=1000000000
HISTTIMEFORMAT='%F %T '

# Avoid duplicates
HISTCONTROL=ignoredups:erasedups
# When the shell exits, append to the history file instead of overwriting it
shopt -s histappend

HISTIGNORE="*mega-login*"

# Autocompletion
bind 'set show-all-if-ambiguous on'
bind 'TAB:menu-complete'

# After each command, append to the history file and reread it
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=


# ====================
# ====== PROMPT ======
# ====================

# Default is PS1="[\u\e[m@\h \W]\$ "
# Simplify prompt definition
_red="\001$(tput setaf 1)\002"
_green="\001$(tput setaf 2)\002"
_blue="\001$(tput setaf 4)\002"
_cyan="\001$(tput setaf 6)\002"
_white="\001$(tput setaf 7)\002"
_bold="\001$(tput bold)\002"
_reset="\001$(tput sgr0)\002"
# Build the prompt
PS1="$_bold$_cyan\[\w\]\n"
PS1+="$_reset$_bold[$_blue\u\[\e[m\]"
PS1+="$_bold@$_red\h$_reset$_bold]"
PS1+="$_reset\$ "
