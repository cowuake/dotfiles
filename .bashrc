######################################################################
##   ██████╗ ███╗   ██╗██╗   ██╗    ██████╗  █████╗ ███████╗██╗  ██╗
##  ██╔════╝ ████╗  ██║██║   ██║    ██╔══██╗██╔══██╗██╔════╝██║  ██║
##  ██║  ███╗██╔██╗ ██║██║   ██║    ██████╔╝███████║███████╗███████║
##  ██║   ██║██║╚██╗██║██║   ██║    ██╔══██╗██╔══██║╚════██║██╔══██║
##  ╚██████╔╝██║ ╚████║╚██████╔╝    ██████╔╝██║  ██║███████║██║  ██║
##   ╚═════╝ ╚═╝  ╚═══╝ ╚═════╝     ╚═════╝ ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝
######################################################################
### Note: the font above is ANSI Shadow


# ======================
# ====== SOURCING ======
# ======================

xrdb ~/.Xresources

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# Search for additional local content (e.g., company-related stuff)
if [ -f ~/.addenda ]; then
    . ~/.addenda
fi


# =====================
# ====== INPUTRC ======
# =====================
### This replaces directives usually specified in ~/.inputrc

bind '"\e[28~":""'
#bind '"\e[A": history-search-backward' # up arrow
#bind '"\e[B": history-search-forward' # down arrow
bind '"\C-p": history-search-backward'
bind '"\C-n": history-search-forward'
#bind '"\e[C": forward-char' # right arrow
#bind '"\e[D": backward-char' # left arrow
bind '"\C-f": forward-char'
bind '"\C-b": backward-char'

# Say no to annoying beeps/flashes!
bind 'set bell-style none'

# See https://www.topbug.net/blog/2017/07/31/inputrc-for-humans/
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

# Avoid duplicates
HISTCONTROL=ignoredups:erasedups
# When the shell exits, append to the history file instead of overwriting it
shopt -s histappend

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
PS1+="$_bold$_white[$_blue\u\[\e[m\]"
PS1+="$_bold@$_red\h$_white]"
PS1+="$_reset\$ "


# =========================
# ====== ENVIRONMENT ======
# =========================

if [ -f "/usr/bin/emacs" ]; then
    if ps -aux | grep "emacs  --daemon" > /dev/null 2>&1; then
	EDITOR=emacsclient
    else
	EDITOR=EMACS
    fi
else
    if [ -f "/usr/bin/vim" ]; then
       EDITO=vim
    fi
fi

PATH=$HOME/bin:$PATH # Personal executable scripts
OPENMPI_INSTALL_DIR=/usr/lib64/openmpi
MPICH_INSTALL_DIR=/usr/lib64/mpich
PATH=/usr/lib64/openmpi/bin:$PATH # OPENMPI binaries, e.g., mpicc, mpicxx
PATH=/usr/lib64/mpich/bin:$PATH # MPICH binaries, e.g., mpicc, mpicxx

# Matlab
PATH=$HOME/MATLAB/R2020b/bin:$PATH

# refine by NASA
PATH=$HOME/gitClones/refine/build/bin:$PATH

# Julia threads
export JULIA_NUM_THREADS=$(echo "$(nproc --all) / 2" | bc)


# ==============================
# ====== USEFUL FUNCTIONS ======
# ==============================

function su2-here() {
    export SU2_RUN=$(pwd)/bin
    export SU2_HOME=$(pwd)
    export PATH=$PATH:$SU2_RUN
    export PYTHONPATH=$SU2_RUN:$PYTHONPATH
}

function on-classic-linux() {
    if [[ -f "/etc/os-release" ]]; then
	if [[ `cat /etc/os-release | grep \^NAME` == *"$1"* ]]; then
	    return 0
	else
	    return 1
	fi
    else
	echo "/etc/os-release not found. This is not classic GNU/Linux."
	return 1
    fi
}

function on-fedora() {
    on-classic-linux "Fedora"
}

function on-debian() {
    on-classic-linux "Debian GNU/Linux"
}

function on-tumbleweed() {
    on-classic-linux "openSUSE Tumbleweed"
}

function on-leap() {
    on-classic-linux "openSUSE Leap"
}

function on-guix() {
    if [[ -n "$GUIX_LOCPATH" ]]; then
	return 0
    else
	return 1
    fi
}

function update-distro() {
    OPTION=$1
    SESSION=UPDATE

    if on-classic-linux; then
	if on-fedora; then
	    if [[ $OPTION == "fast" ]]; then
		UPDATE_CMD="sudo dnf upgrade"
	    else
		UPDATE_CMD="sudo dnf upgrade --refresh"
	    fi
	elif on-debian; then
	    UPDATE_CMD="sudo apt update && sudo apt upgrade"
	elif on-tumbleweed; then
	    if [[ $OPTION == "fast" ]]; then
		UPDATE_CMD="sudo zypper dup --allow-vendor-change"
	    else
		UPDATE_CMD="sudo zypper refresh && sudo zypper dup --allow-vendor-change"
	    fi
	elif on-leap; then
	    UPDATE_CMD="sudo zypper up"
	else
	    echo -e "\n\tCommand not set for the distribution in use.\n"
	fi
    elif [[ ! -f "/etc/os-release" ]]; then
	if on-guix; then
	    UPDATE_CMD="guix pull"
	else
	    echo -e "\n\tCommand not set for the distribution in use.\n"
	fi
    fi

    tmux new-session -d -s $SESSION

    tmux send-keys "$UPDATE_CMD" C-m
    tmux attach-session -t $SESSION
}


# =================================================
# ====== ALIASES TO BE USED ON EVERY MACHINE ======
# =================================================

# User specific aliases
alias l="ls -lah"
alias ll="ls -lahtr"
alias lll="ls -lahSr"
alias lf="ls -d */"
alias ..="cd ../"
alias ...="cd ../../"
alias e="emacsclient"
alias ewe="emacs --daemon"
alias ee="emacsclient -nw"
alias et="emacs -nw"
alias gp="gnuplot"
alias sc="screen"
alias bibmerge="bibtool -s -d"
alias mytop="top -u $USER"
alias myhtop="htop -u $USER"
alias h="htop"
alias d="cd ~/Downloads"
alias r="cd ~/gitRepos"
alias c="cd ~/gitClones"
#alias julia='julia --threads $(echo "$(nproc --all) / 2" | bc)'
#alias julia="julia -p $(echo "$(nproc --all) / 2" | bc)"
alias mesh="~/gitRepos/SU2_functionalUtilities/mesh.jl"
alias anaconda2="$HOME/anaconda2/bin/python"
alias anaconda3="$HOME/anaconda3/bin/python"
alias conda2="$HOME/anaconda2/bin/conda"
alias conda3="$HOME/anaconda3/bin/conda"
alias anaconda2-update-consistent="conda2 update anaconda"
alias anaconda3-update-consistent="conda3 update anaconda"
alias anaconda2-update-newest="conda2 update conda && conda2 update --all"
alias anaconda3-update-newest="conda3 update conda && conda3 update --all"
alias rsync="/usr/bin/env rsync --info=progress2 --no-i-r"
alias remotesync="/usr/bin/env rsync -az --info=progress2 --no-i-r"
alias rsync-old="/usr/bin/env rsync --no-i-r"
alias remotesync-old="rsync-old -az --no-i-r"
alias space="du -h --max-depth=1 | sort -h"
alias feh="feh --scale-down --auto-zoom"
alias quake2="$HOME/gitClones/yquake2/release/quake2"
alias quake3="$HOME/gitClones/ioq3/build/release-linux-x86_64/ioquake3.x86_64"
alias matlab="matlab & disown matlab"
alias matlabcli="matlab -nodesktop -nosplash"
alias vi="vim"
alias battery-state="sudo tlp-stat -b"
alias t="tmux"
alias ta="tmux attach-session"
alias v="sudo vpnc"
alias vd="sudo vpnc-disconnect"

alias ff="FreeFem++-mpi_openmpi"

alias generate-ssh-key="ssh-keygen -o -a 100 -t ed25519 -f ~/.ssh/id_ed25519"

alias reswap="sudo swapoff -a && sudo swapon -a"
alias freecache="sudo su -c 'sync; echo 1 > /proc/sys/vm/drop_caches'"

alias governor="cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor"
alias governor-performance="sudo cpupower frequency-set -g performance"
alias governor-ondemand="sudo cpupower frequency-set -g ondemand"
alias governor-conservative="sudo cpupower frequency-set -g conservative"
alias governor-schedutil="sudo cpupower frequency-set -g schedutil"
alias governor-powersave="sudo cpupower frequency-set -g powersave"
alias cpu-boost="cat /sys/devices/system/cpu/cpufreq/boost"
alias cpu-boost-off="echo 0 | sudo tee /sys/devices/system/cpu/cpufreq/boost"
alias cpu-boost-on="echo 1 | sudo tee /sys/devices/system/cpu/cpufreq/boost"
alias cpu-frequency="lscpu | grep MHz"
alias update-firmwares="fwupdmgr get-devices && fwupdmgr refresh --force && fwupdmgr update"
#alias ompi="$OPENMPI_INSTALL_DIR/bin/mpirun -n $((`nproc --all`/2))"
#alias mpich="$MPICH_INSTALL_DIR/bin/mpirun -n $((`nproc --all`/2))"cc
#alias mpi="mpirun -n $((`nproc --all`/2))"

alias gnome-internal-keyboard="gsettings set org.gnome.desktop.input-sources xkb-options \"['lv3:ralt_switch', 'altwin:swap_alt_win', 'compose:ralt', 'ctrl:swapcaps_hyper']\""
alias gnome-external-keyboard="gsettings set org.gnome.desktop.input-sources xkb-options \"['compose:ralt']\""

alias gc="git commit"
alias gin="git pull"
alias gout="git push"
alias config="/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME"

SU2_PREFIX=$HOME
alias su2-build="./meson.py build --buildtype=release --prefix=$SU2_PREFIX && ./ninja -C build install"
alias su2-build-debug="./meson.py build --buildtype=debug --prefix=$SU2_PREFIX && ./ninja -C build install"
alias su2-rebuild="./meson.py build --reconfigure --buildtype=release --prefix=$SU2_PREFIX && ./ninja -C build install"
alias su2-rebuild-debug="./meson.py build --reconfigure --buildtype=debug --prefix=$SU2_PREFIX && ./ninja -C build install"


# ===========================================================
# ====== PERL ONE-LINERS (I.E., ADDITIONAL ALIASES...) ======
# ===========================================================

# Not escaped version: perl -e 'rename$_,y/ /_/drfor<"* *">'
alias adjust-filenames="perl -e 'rename\$_,y/ /_/drfor<\"* *\">'"


# ============================================================
# ====== FEDORA ADDITIONS (I.E., ADDITIONAL ALIASES...) ======
# ============================================================

if [[ -f "/etc/os-release" && on-fedora ]]; then
    alias fedora-dnf-reset="sudo dnf clean all && sudo dnf makecache --refresh -v"
    alias fedora-enable-chez-scheme="sudo dnf copr enable superboum/chez-scheme"
    alias fedora-enable-tdlib-fresh="sudo dnf copr enable carlis/tdlib-fresh"
    alias fedora-enable-tlprepo="sudo dnf in https://repo.linrunner.de/fedora/tlp/repos/releases/tlp-release.fc$(rpm -E %fedora).noarch.rpm"
    alias fedora-enable-rpmfusion="sudo dnf in https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm \
      https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm"
    alias fedora-my-kernels="dnf list installed kernel"
    alias fedora-my-packages="dnf repoquery --qf '%{name}' --userinstalled \
      | grep -v -- '-debuginfo$' \
      | grep -v '^\(kernel-modules\|kernel\|kernel-core\|kernel-devel\)$'"
    alias fedora-my-repositories="dnf repolist enabled"
fi


alias rhel-awake-subscription="sudo subscription-manager remove --all; \
      		   	       sudo subscription-manager unregister; \
			       sudo subscription-manager clean; \
			       sudo subscription-manager register; \
			       sudo subscription-manager attach --auto"
