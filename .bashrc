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

# ======================
# ====== SOURCING ======
# ======================

xrdb ~/.Xresources

# Source global definitions
if [ -f /etc/bashrc ] ; then
    . /etc/bashrc
fi

# Search for additional local content (e.g., company-related stuff)
if [ -f ~/.addenda ] ; then
    . ~/.addenda
fi


###############################
# ============================
# ====== SHELL SETTINGS ======
# ============================
###############################

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

# =======================
# ====== WELCOMING ======
# =======================

cat <<EOF
██╗    ██╗███████╗██╗      ██████╗ ██████╗ ███╗   ███╗███████╗    ██████╗  █████╗  ██████╗██╗  ██╗
██║    ██║██╔════╝██║     ██╔════╝██╔═══██╗████╗ ████║██╔════╝    ██╔══██╗██╔══██╗██╔════╝██║ ██╔╝
██║ █╗ ██║█████╗  ██║     ██║     ██║   ██║██╔████╔██║█████╗      ██████╔╝███████║██║     █████╔╝
██║███╗██║██╔══╝  ██║     ██║     ██║   ██║██║╚██╔╝██║██╔══╝      ██╔══██╗██╔══██║██║     ██╔═██╗
╚███╔███╔╝███████╗███████╗╚██████╗╚██████╔╝██║ ╚═╝ ██║███████╗    ██████╔╝██║  ██║╚██████╗██║  ██╗
 ╚══╝╚══╝ ╚══════╝╚══════╝ ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚══════╝    ╚═════╝ ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝
EOF

# =========================
# ====== ENVIRONMENT ======
# =========================

### GIT
GIT_REPOS_DIR=$HOME/gitRepos
GIT_CLONES_DIR=$HOME/gitClones

### MACHINE RESOURCES
CPU_CORES=$(cat /proc/cpuinfo | grep -m 1 "cpu cores" | awk '{print $4}')
CPU_THREADS=$(nproc --all)
CPU_THREADS_PER_CORE=$(echo "$CPU_THREADS / $CPU_CORES" | bc)

### TEXT EDITOR
if [ -f "/usr/bin/emacs" ] ; then
    if ps -aux | grep "emacs  --daemon" > /dev/null 2>&1 ; then
	EDITOR=emacsclient
    else
	EDITOR=EMACS
    fi
else
    if [ -f "/usr/bin/vim" ] ; then
       EDITO=vim
    fi
fi

### ADDITIONAL EXECUTABLES
PATH=$HOME/bin:$PATH # Personal executable scripts
PATH=$HOME/.julia/bin:$PATH # Julia binaries
OPENMPI_INSTALL_DIR=/usr/lib64/openmpi
MPICH_INSTALL_DIR=/usr/lib64/mpich
PATH=/usr/lib64/openmpi/bin:$PATH # OPENMPI binaries, e.g., mpicc, mpicxx
PATH=/usr/lib64/mpich/bin:$PATH # MPICH binaries, e.g., mpicc, mpicxx

### MATLAB
PATH=$HOME/MATLAB/R2020b/bin:$PATH

# refine by NASA
PATH=$GIT_CLONES_DIR/refine/build/bin:$PATH

### JULIA
export JULIA_NUM_THREADS=$CPU_CORES


#################################
# ==============================
# ====== USEFUL FUNCTIONS ======
# ==============================
#################################

# ================================
# ====== Generic Facilities ======
# ================================
function random-files() {
    # Set number of files to be created
    if [ $# -eq 0 ]; then N=1; else N=$1; fi
    # Confirm the operation to be performed
    echo "Going to create $N files with random names."
    # LOOP
    for i in $(seq 1 $N)
    do
	# Choose a random name
	name="$(random | tr '\\/' '(' | tr '-' ')')"
	# Generate the file
	touch "$name"
	# Confirm file creation
	echo "Created file $i: $(pwd)/$name"
    done
}

# ===============================
# ====== Distro Facilities ======
# ===============================
function on-classic-linux() {
    if [[ -f "/etc/os-release" ]] ; then
	if [[ `cat /etc/os-release | grep \^NAME` == *"$1"* ]] ; then
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

    if on-classic-linux ; then
	if on-fedora ; then
	    if [[ $OPTION == "fast" ]] ; then
		UPDATE_CMD="sudo dnf upgrade"
	    else
		UPDATE_CMD="sudo dnf upgrade --refresh"
	    fi
	elif on-debian ; then
	    UPDATE_CMD="sudo apt update && sudo apt upgrade"
	elif on-tumbleweed ; then
	    if [[ $OPTION == "fast" ]] ; then
		UPDATE_CMD="sudo zypper dup --allow-vendor-change"
	    else
		UPDATE_CMD="sudo zypper refresh && sudo zypper dup --allow-vendor-change"
	    fi
	elif on-leap ; then
	    UPDATE_CMD="sudo zypper up"
	else
	    echo -e "\n\tCommand not set for the distribution in use.\n"
	fi
    elif [[ ! -f "/etc/os-release" ]] ; then
	if on-guix ; then
	    UPDATE_CMD="guix pull"
	else
	    echo -e "\n\tCommand not set for the distribution in use.\n"
	fi
    fi

    tmux new-session -d -s $SESSION

    tmux send-keys "$UPDATE_CMD" C-m
    tmux attach-session -t $SESSION
}


####################################################
# =================================================
# ====== ALIASES TO BE USED ON EVERY MACHINE ======
# =================================================
####################################################

# =======================
# ====== Utilities ======
# =======================
alias ..="cd ../"
alias ...="cd ../../"
alias ....="cd ../../../"
alias battery-state="sudo tlp-stat -b"
alias bibmerge="bibtool -s -d"
alias e="emacsclient"
alias ee="emacsclient -nw"
alias et="emacs -nw"
alias ewe="emacs --daemon"
alias feh="feh --scale-down --auto-zoom"
alias freecache="sudo su -c 'sync; echo 1 > /proc/sys/vm/drop_caches'"
alias generate-ssh-key="ssh-keygen -o -a 100 -t ed25519 -f ~/.ssh/id_ed25519"
alias gp="gnuplot"
alias h="htop"
alias l="ls -lah"
alias lf="ls -d */"
alias ll="ls -lahtr"
alias lll="ls -lahSr"
alias myhtop="htop -u $USER"
alias mytop="top -u $USER"
alias o="xdg-open"
alias open="xdg-open"
alias random="apg -MSNCL -m 5 -x 10 | head -n 1"
alias reboot="systemctl reboot"
alias remotesync-old="rsync-old -az --no-i-r"
alias remotesync="/usr/bin/env rsync -az --info=progress2 --no-i-r"
alias reswap="sudo swapoff -a && sudo swapon -a"
alias rsync-old="/usr/bin/env rsync --no-i-r"
alias rsync="/usr/bin/env rsync --info=progress2 --no-i-r"
alias sc="screen"
alias space="du -h --max-depth=1 | sort -h"
alias t="tmux"
alias ta="tmux attach-session"
alias temp="sensors"
alias tf="tail -f"
alias update-firmwares="fwupdmgr get-devices && fwupdmgr refresh --force && fwupdmgr update"
alias vc="sudo vpnc"
alias vd="sudo vpnc-disconnect"
alias vi="vim"

# =================
# ====== Git ======
# =================
alias config="/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME"
alias gc="git commit"
alias gin="git pull"
alias gout="git push"

# =======================
# ====== Hardware  ======
# =======================
alias cpu-boost-off="echo 0 | sudo tee /sys/devices/system/cpu/cpufreq/boost"
alias cpu-boost-on="echo 1 | sudo tee /sys/devices/system/cpu/cpufreq/boost"
alias cpu-boost="cat /sys/devices/system/cpu/cpufreq/boost"
alias cpu-frequency="lscpu | grep MHz"
alias governor-conservative="sudo cpupower frequency-set -g conservative"
alias governor-ondemand="sudo cpupower frequency-set -g ondemand"
alias governor-performance="sudo cpupower frequency-set -g performance"
alias governor-powersave="sudo cpupower frequency-set -g powersave"
alias governor-schedutil="sudo cpupower frequency-set -g schedutil"
alias governor="cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor"

# =============================================
# ====== Frequently Accessed Directories ======
# =============================================
alias c="cd $GIT_CLONES_DIR"
alias d="cd ~/Downloads"
alias m="cd ~/Music"
alias p="cd ~/Pictures"
alias r="cd $GIT_REPOS_DIR"
alias v="cd ~/Videos"

# =====================
# ====== Desktop ======
# =====================
alias gnome-external-keyboard="gsettings set org.gnome.desktop.input-sources xkb-options \"['compose:ralt']\""
alias gnome-internal-keyboard="gsettings set org.gnome.desktop.input-sources xkb-options \"['lv3:ralt_switch', 'altwin:swap_alt_win', 'compose:ralt', 'ctrl:swapcaps_hyper']\""

# ====================
# ====== Python ======
# ====================
alias anaconda2="$HOME/anaconda2/bin/python"
alias anaconda3="$HOME/anaconda3/bin/python"
alias conda2="$HOME/anaconda2/bin/conda"
alias conda3="$HOME/anaconda3/bin/conda"

alias anaconda2-update-consistent="conda2 update anaconda"
alias anaconda3-update-consistent="conda3 update anaconda"
alias anaconda2-update-newest="conda2 update conda && conda2 update --all"
alias anaconda3-update-newest="conda3 update conda && conda3 update --all"

# =====================================================
# ====== Games and other (invasive) applications ======
# =====================================================
alias matlab="matlab & disown matlab"
alias matlabcli="matlab -nodesktop -nosplash"
alias quake2="$GIT_CLONES_DIR/yquake2/release/quake2"
alias quake3="$GIT_CLONES_DIR/ioq3/build/release-linux-x86_64/ioquake3.x86_64"

# ==================================
# ====== Temporary Facilities ======
# ==================================
alias ff="FreeFem++-mpi_openmpi"
alias mesh="~/$GIT_REPOS_DIR/SU2_functionalUtilities/mesh.jl"

# ===========================================================
# ====== PERL ONE-LINERS (I.E., ADDITIONAL ALIASES...) ======
# ===========================================================
# Not escaped version: perl -e 'rename$_,y/ /_/drfor<"* *">'
alias adjust-filenames="perl -e 'rename\$_,y/ /_/drfor<\"* *\">'"

# ============================================================
# ====== FEDORA ADDITIONS (I.E., ADDITIONAL ALIASES...) ======
# ============================================================

if [[ -f "/etc/os-release" && on-fedora ]] ; then
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

# ==========================================================
# ====== RHEL ADDITIONS (I.E., ADDITIONAL ALIASES...) ======
# ==========================================================
alias rhel-awake-subscription="sudo subscription-manager remove --all; \
      		   	       sudo subscription-manager unregister; \
			       sudo subscription-manager clean; \
			       sudo subscription-manager register; \
			       sudo subscription-manager attach --auto"


#######################################
# ====================================
# ====== SU2 / SLURM FACILITIES ======
# ====================================
#######################################

#function su2-here() {
#    export SU2_RUN=$(pwd)/bin
#    export SU2_HOME=$(pwd)
#    export PATH=$PATH:$SU2_RUN
#    export PYTHONPATH=$SU2_RUN:$PYTHONPATH
#}

SLURM_SBATCH_EXT=".sl"
SLURM_JOB_NAME_ROOT="slurmjob"
alias slurm-launch-all='for file in $(find . -type f -name "*$SLURM_SBATCH_EXT"); \
      			    do sbatch "$file"; done'
alias slurm-kill-all='for file in $(find . -type f -name "$SLURM_JOB_NAME_ROOT*"); \
      	       		  do scancel $(echo $file | tr -d -c 0-9); done'

function slurm-launch-su2() {
    if [[ $1 == "" ]] ; then
	echo "You must specify at least the job name."
	return
    fi

    if [[ $2 == "" ]] ; then
	echo "file '$2' does not exist."
	return
    fi

    JOBNAME=$1
    CFG_FILE=$2
    PARALLEL_OPTION=$3
    MPI_OPTIONS=$6

    if [[ $PARALLEL_OPTION == "" ]] ; then
	#echo "No parallelization option specified. Using pure MPI."
	N_CORES=$CPU_CORES
	N_THREADS=1
	PARALLEL_OPTION=MPI
    elif [[ ${PARALLEL_OPTION,,} == "mpi" ]] ; then
	#echo "Using pure MPI"
	N_CORES=$CPU_CORES
	N_THREADS=1
    elif [[ ${PARALLEL_OPTION,,} == "threading" ]] ; then
	#echo "Using multithreading only (OpenMP)."
	N_CORES=1
	N_THREADS=$CPU_THREADS
    elif [[ ${PARALLEL_OPTION,,} == "hybrid" ]] ; then
	#echo "Using the hybrid approach (MPI + OpenMP)"
	N_CORES=$CPU_CORES
	N_THREADS=$CPU_THREADS_PER_CORE
    elif [[ ${PARALLEL_OPTION,,} == "custom" ]] ; then
	N_CORES=$4
	N_THREADS=$5
    else
	echo "No valid parallelization option specified."
	return
    fi

    echo "SLURM jobname: $1"
    echo "SU2 config file: $2"
    echo "Parallelization strategy: $PARALLEL_OPTION"
    echo "The job will be launched with $N_CORES processes and$ $N_THREADS threads per process"

    HOST=$(hostname)
    FILE=auto_job.sl
    rm $FILE ; touch $FILE

    TEXT=$(cat <<EOF
#!/usr/bin/env bash

#SBATCH --job-name=$JOBNAME
#SBATCH --nodes=1
#SBATCH --nodelist=$HOST
#SBATCH --output slurmjob-%j.out
#SBATCH --error slurmjob-%j.err

export LD_LIBRARY_PATH=/usr/lib64/openmpi/lib:\$LD_LIBRARY_PATH

date > time_begin.txt
mpirun $MPI_OPTIONS -n $N_CORES SU2_CFD -t $N_THREADS $CFG_FILE
date > time_end.txt
EOF
    )
    echo "$TEXT" > $FILE
    #echo "$TEXT"
    sbatch $FILE
}

function slurm-launch-su2-mpi() { slurm-launch-su2 $1 $2 "mpi" ; }
function slurm-launch-su2-hybrid() { slurm-launch-su2 $1 $2 "hybrid" ; }
function slurm-launch-su2-threading() { slurm-launch-su2 $1 $2 "threading" ; }
function slurm-launch-su2-custom() { slurm-launch-su2 $1 $2 "custom" $3 $4 $5 ; }

function slurm-dummy-job() {
    HOST=$(hostname)
    JOBNAME="DUMMY_$HOST"
    FILE=~/dummy_job.sl

    if [[ $1 != "" ]] ; then TIME=$1 ; else TIME=2d ; fi
    if [ -f $FILE ] ; then rm $FILE ; fi ; touch $FILE

    # Only print job IDs and names, with a reasonable formatting to stay on the safe side
    TEMP=$(squeue --format="%.10i  %30j" | grep $JOBNAME)
    if [[ $TEMP != "" ]] ; then scancel $(echo $TEMP | awk '{print $1}') ; fi

    TEXT=$(cat <<EOF
#!/bin/bash
#
#SBATCH --job-name=$JOBNAME
#SBATCH -N 1
#SBATCH --nodelist=$HOST
#SBATCH --exclusive
#
sleep 2d
EOF
    )
    echo "$TEXT" > $FILE

    sbatch $FILE
}

function slurm-dummy-job-remote() {
    HOSTNAME=$1
    if [[ $HOSTNAME != "" ]] ; then
	if ssh $USER@$HOSTNAME "$(typeset -f slurm-dummy-job); slurm-dummy-job" ; then
	    echo "Command submitted to host $HOSTNAME"
	else
	    echo "Impossibile to reach host $HOSTNAME"
	fi
    fi
}

### Better formatting for squeue output
alias squeue="squeue --format='%.10i %.10P %20j %10u %.10T %.10M %20N'"

SU2_PREFIX=$HOME

alias su2-build="./meson.py build --buildtype=release -Dwith-omp=true \
      --prefix=$SU2_PREFIX && ./ninja -C build install"

alias su2-build-debug="./meson.py build --buildtype=debug -Dwith-omp=true \
      --prefix=$SU2_PREFIX && ./ninja -C build install"

alias su2-rebuild="./meson.py build --reconfigure --buildtype=release -Dwith-omp=true \
      --prefix=$SU2_PREFIX && ./ninja -C build install"

alias su2-rebuild-debug="./meson.py build --reconfigure --buildtype=debug -Dwith-omp=true \
      --prefix=$SU2_PREFIX && ./ninja -C build install"

alias su2-clean-dir="find ./* -type f -not -name '*.cfg' -not -name '*.su2' -not -name '*.sl' -delete"
alias su2-wipe-build="./meson.py setup --wipe build/"
