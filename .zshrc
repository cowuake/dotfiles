##########################################################
##  ███████╗    ███████╗██╗  ██╗███████╗██╗     ██╗
##  ╚══███╔╝    ██╔════╝██║  ██║██╔════╝██║     ██║
##    ███╔╝     ███████╗███████║█████╗  ██║     ██║
##   ███╔╝      ╚════██║██╔══██║██╔══╝  ██║     ██║
##  ███████╗    ███████║██║  ██║███████╗███████╗███████╗
##  ╚══════╝    ╚══════╝╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝
##########################################################
### Note: the font above is ANSI Shadow

# =========================
# ====== WHO'S THAT? ======
# =========================
# Riccardo Mura created and uses this configuration file

# ======================
# ====== SOURCING ======
# ======================

# Source .Xresources if present
if [ -f ~/.Xresources ] ; then
    xrdb ~/.Xresources
fi

# Source global definitions
if [ -f /etc/zshrc ]; then
	. /etc/zshrc
fi

# Search for additional local content (e.g., company-related stuff)
if [ -f ~/.addenda ] ; then
    . ~/.addenda
fi

SHELL_DIR=~/.shell
for dir in $SHELL_DIR/{common,zsh}; do
    if [ -d "$dir" ] ; then
        . "$dir"/aliases/*.sh
        . "$dir"/auto/*.sh
        . "$dir"/functions/*.sh
        . "$dir"/os/*.sh
        . "$dir"/session/*.sh
        . "$dir"/environment/*.sh
    fi
done


###############################
# ============================
# ====== SHELL SETTINGS ======
# ============================
###############################

autoload -U compinit; compinit

# =====================
# ====== INPUTRC ======
# =====================
### This replaces directives usually specified in ~/.inputrc

bindkey '^H' backward-kill-word
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward
bindkey '\e[A' history-search-backward
bindkey '\e[B' history-search-forward

### Key bindings
#bind '"\e[28~":""'
#bind '"\C-p": history-search-backward'
#bind '"\C-n": history-search-forward'
#bind '"\C-f": forward-char'
#bind '"\C-b": backward-char'
#bind '"\C-H": shell-backward-kill-word' # Ctrl-Backspace in alternative to Ctrl-W

### Deprecated key bindings (kept because who knows...)
#bind '"\e[A": history-search-backward' # up arrow
#bind '"\e[B": history-search-forward' # down arrow
#bind '"\e[C": forward-char' # right arrow
#bind '"\e[D": backward-char' # left arrow

# Say no to annoying beeps/flashes!
#bind 'set bell-style none'

### See https://www.topbug.net/blog/2017/07/31/inputrc-for-humans/
#bind 'set colored-stats On'
#bind 'set completion-ignore-case On'
#bind 'set completion-prefix-display-length 3'
#bind 'set mark-symlinked-directories On'
#bind 'set show-all-if-ambiguous On'
#bind 'set show-all-if-unmodified On'
#bind 'set visible-stats On'


# =====================
# ====== HISTORY ======
# =====================

HISTSIZE=1000000
HISTFILESIZE=1000000000
HISTTIMEFORMAT='%F %T '

setopt HIST_SAVE_NO_DUPS

# Avoid duplicates
HISTCONTROL=ignoredups:erasedups
# When the shell exits, append to the history file instead of overwriting it
#shopt -s histappend

HISTIGNORE="*mega-login*"

# Autocompletion
#bind 'set show-all-if-ambiguous on'
#bind 'TAB:menu-complete'

# After each command, append to the history file and reread it
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=


# ====================
# ====== PROMPT ======
# ====================

PROMPT="%B%~%b
%B[%F{blue}%n%f@%F{red}%m%f]%b%% "
