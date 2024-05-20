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


###############################
# ============================
# ====== SHELL SETTINGS ======
# ============================
###############################

shopt -s globstar

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

# =======================
# ====== WELCOMING ======
# =======================

function welcome() {
cat <<EOF
██╗    ██╗███████╗██╗      ██████╗ ██████╗ ███╗   ███╗███████╗    ██████╗  █████╗  ██████╗██╗  ██╗
██║    ██║██╔════╝██║     ██╔════╝██╔═══██╗████╗ ████║██╔════╝    ██╔══██╗██╔══██╗██╔════╝██║ ██╔╝
██║ █╗ ██║█████╗  ██║     ██║     ██║   ██║██╔████╔██║█████╗      ██████╔╝███████║██║     █████╔╝
██║███╗██║██╔══╝  ██║     ██║     ██║   ██║██║╚██╔╝██║██╔══╝      ██╔══██╗██╔══██║██║     ██╔═██╗
╚███╔███╔╝███████╗███████╗╚██████╗╚██████╔╝██║ ╚═╝ ██║███████╗    ██████╔╝██║  ██║╚██████╗██║  ██╗
 ╚══╝╚══╝ ╚══════╝╚══════╝ ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚══════╝    ╚═════╝ ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝
EOF
}

welcome

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

### OPERATING SYSTEM
KERNEL=$(uname -srv)

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

### ADDITIONS TO $PATH
source_dirs=(
    $HOME/bin                # personal executable files
    $HOME/.julia/bin/        # Julia binaries
    /usr/lib64/openmpi/bin   # OPENMPI binaries, e.g., mpicc, mpicxx
    /usr/lib64/mpich/bin     # MPICH binaries, e.g., mpicc, mpicxx
    /opt/mssql-tools/bin     # Microsoft SQL Server CLI utilities
    /opt/mssql-tools18/bin   # Microsoft SQL Server CLI utilities (release 18)
)

for source in "${source_dirs[@]}"
do
    if [ -d "$source" ] ; then
        PATH="$source":$PATH
    fi
done

OPENMPI_INSTALL_DIR=/usr/lib64/openmpi
MPICH_INSTALL_DIR=/usr/lib64/mpich

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
    if [ $# -eq 0 ] ; then N=1; else N=$1; fi
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
        if [[ $1 == "" ]] ; then
            echo "/etc/os-release not found. This is not classic GNU/Linux."
        fi
        return 1
    fi
}

function on-guix() {
    if [[ -n "$GUIX_LOCPATH" ]] ; then
        return 0
    else
        return 1
    fi
}

function update-distro() {
    OPTION=$1
    SESSION=UPDATE
    FOUND_DISTRO=0

    if on-classic-linux ; then
        # NOTE: In a future Fedora release, yum might stop being an alias for dnf
        if on-classic-linux "Fedora Linux" ; then # for Fedora, CentOS [Stream], RHEL
            if [[ $OPTION == "fast" ]] ; then
                UPDATE_CMD="sudo yum upgrade"
            else
                UPDATE_CMD="sudo yum upgrade --refresh"
            fi
            FOUND_DISTRO=1
        elif on-classic-linux "Debian GNU/Linux" ; then
            UPDATE_CMD="sudo apt update && sudo apt upgrade"
            FOUND_DISTRO=1
        elif on-classic-linux "openSUSE Tumbleweed" ; then
            if [[ $OPTION == "fast" ]] ; then
                UPDATE_CMD="sudo zypper dup --allow-vendor-change"
            else
                UPDATE_CMD="sudo zypper refresh && sudo zypper dup --allow-vendor-change"
            fi
            FOUND_DISTRO=1
        elif on-classic-linux "openSUSE Leap" ; then
            UPDATE_CMD="sudo zypper up"
            FOUND_DISTRO=1
        fi
    elif [[ ! -f "/etc/os-release" ]] ; then
        if on-guix ; then
            UPDATE_CMD="guix pull"
        else
            echo -e "\n\tCommand not set for the distribution in use.\n"
        fi
        FOUND_DISTRO=1
    fi

    # Execute update in a Tmux session if the distro in use is supported
    if [ $FOUND_DISTRO = 1 ] ; then
        tmux new-session -d -s $SESSION
        tmux send-keys "$UPDATE_CMD" C-m
        tmux attach-session -t $SESSION
    else
        echo "update-distro: the function does not support the distro in use."
    fi
}

if on-classic-linux "Fedora Linux" ; then
    function fedora-dotnet-trust-dev-certs() {
        CERTS_DIR=~/.certs

        dotnet dev-certs https -ep $CERTS_DIR/by_dotnet.crt --format PEM
        certutil -d sql:$HOME/.pki/nssdb -A -t "P,," -n localhost -i $CERTS_DIR/by_dotnet.crt
        certutil -d sql:$HOME/.pki/nssdb -A -t "C,," -n localhost -i $CERTS_DIR/by_dotnet.crt
        sudo cp $CERTS_DIR/by_dotnet.crt /etc/pki/tls/certs/localhost.pem
        sudo update-ca-trust

        EASYRSA_DIR=~/.easyrsa

        # Install easy-rsa if not already installed
        if rpm -q easy-rsa | grep -q 'not installed' ; then
            sudo dnf in easy-rsa
        fi

        if ! [ -d $EASYRSA_DIR ] ; then
            mkdir $EASYRSA_DIR
            chmod 700 $EASYRSA_DIR
        fi

        cd $EASYRSA_DIR
        cp -r /usr/share/easy-rsa/3/* .
        ./easyrsa init-pki

        cat << EOF > vars
set_var EASYRSA_REQ_COUNTRY    "US"
set_var EASYRSA_REQ_PROVINCE   "Texas"
set_var EASYRSA_REQ_CITY       "Houston"
set_var EASYRSA_REQ_ORG        "Development"
set_var EASYRSA_REQ_EMAIL      "local@localhost.localdomain"
set_var EASYRSA_REQ_OU         "LocalDevelopment"
set_var EASYRSA_ALGO           "ec"
set_var EASYRSA_DIGEST         "sha512"
EOF

        ./easyrsa build-ca nopass
        sudo cp ./pki/ca.crt /etc/pki/ca-trust/source/anchors/easyrsaca.crt
        sudo update-ca-trust

        if ! [ -d ./req ] ; then
            mkdir req
        fi

        cd req
        openssl genrsa -out localhost.key
        openssl req -new -key localhost.key -out localhost.req \
            -subj /C=US/ST=Texas/L=Houston/O=Development/OU=LocalDevelopment/CN=localhost
        cd ../

        ./easyrsa import-req ./req/localhost.req localhost
        ./easyrsa sign-req server localhost

        cd

        mkdir $CERTS_DIR
        cp $EASYRSA_DIR/pki/issued/localhost.crt $CERTS_DIR/localhost.crt
        cp $EASYRSA_DIR/req/localhost.key $CERTS_DIR/localhost.key
        cd $CERTS_DIR
        openssl pkcs12 -export -out localhost.pfx -inkey localhost.key -in localhost.crt

        echo
        echo "The following environmental variables should be exported:"
        echo "ASPNETCORE_Kestrel__Certificates__Default__Password"
        echo "ASPNETCORE_Kestrel__Certificates__Default__Path (ponting to $CERTS_DIR/localhost.pfx"
    }
fi

# ============================
# ====== PDF Facilities ======
# ============================
function pdf-rm-annots() {
    OLD_FNAME=$1
    if [[ $OLD_FNAME != "" && $OLD_FNAME != " " ]] ; then
        NEW_FNAME=$(echo $OLD_FNAME | sed -e 's/\.[^.]*$/_no-annots&/')
        pdftk $OLD_FNAME output - uncompress \
            | sed '/^\/Annots/d' \
            | pdftk - output $NEW_FNAME compress \
            && echo "SUCCESS! Produced file $NEW_FNAME."
    fi
}

# ==========================================
# ====== Git facilities for lazy devs ======
# ==========================================

function gitLastCommitHash() {
    BRANCH=$1
    git log -n 1 $BRANCH --pretty=format:"%H"
}

function gitCherryPickLastCommit() {
    BRANCH=$1
    HASH=$(gitLastCommitHash $BRANCH)
    git cherry-pick $HASH
}

# ==================================================
# ====== Frequent file conversions (shorties) ======
# ==================================================
function convert-webm-to-mp4() {
    if [[ "${1: -5}" == ".webm" ]] ; then
        OLD_NAME=$1
        NEW_NAME="${OLD_NAME%.*}.mp4"
        ffmpeg -fflags +genpts -i $OLD_NAME -r 24 $NEW_NAME
    else
        echo "Not a webm file!"
    fi
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
alias battery_state="sudo tlp-stat -b"
alias bibmerge="bibtool -s -d"
alias cpp="cp -a" # copy in archive mode (preserves permissions)
alias disks="df -hT"
alias e="emacsclient"
alias ee="emacsclient -nw"
alias et="emacs -nw"
alias ewe="emacs --daemon"
alias feh="feh --scale-down --auto-zoom"
alias freecache="sudo su -c 'sync; echo 1 > /proc/sys/vm/drop_caches'"
alias generate_ssh_key="ssh-keygen -o -a 100 -t ed25519 -f ~/.ssh/id_ed25519"
alias gp="gnuplot"
alias h="htop"
alias hosts_up="nmap -sP 192.168.1.*"
alias kernel="echo '$KERNEL'"
alias k="echo '$KERNEL'"
alias install_julia='sudo bash -ci "$(curl -fsSL https://raw.githubusercontent.com/abelsiqueira/jill/main/jill.sh)"'
alias install_rustup="curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
alias is_login_shell="shopt login_shell | cut -f 2"
alias ls="\ls --color"
alias l="ls -ahl"
alias ld="lsd -ahl"
alias lf="ls -d */"
alias ll="ls -ahlrt"
alias lld="lsd -ahlrt"
alias lll="ls -ahlrS"
alias llld="lsd -ahlrS"
alias lower="find . -depth -exec prename 'y/A-Z/a-z/' {} \;"
alias mem="echo Available memory: $(free -h | grep Mem | awk '{print $7}')"
alias mnt="findmnt"
alias myhtop="htop -u $USER"
alias mytop="top -u $USER"
alias o="xdg-open"
alias open="xdg-open"
alias random="apg -MSNCL -m 5 -x 10 | head -n 1"
alias reboot="systemctl reboot"
alias remotesync_old="rsync-old -az --no-i-r"
alias remotesync="/usr/bin/env rsync -az --info=progress2 --no-i-r"
alias reswap="sudo swapoff -a && sudo swapon -a"
alias rsync_old="/usr/bin/env rsync --no-i-r"
alias rsync="/usr/bin/env rsync --info=progress2 --no-i-r"
alias sc="screen"
alias space="du -h --max-depth=1 | sort -h"
alias steam_optimus="__NV_PRIME_RENDER_OFFLOAD=1 __GLX_VENDOR_LIBRARY_NAME=nvidia steam"
alias t="tmux"
alias ta="tmux attach-session"
alias temp="sensors | grep 'CPU\|GPU\|Composite'"
alias fan="sensors | grep -i rpm"
alias tf="tail -f"
alias update_distro="update-distro"
alias ud="update_distro"
alias update_firmwares="fwupdmgr get-devices && fwupdmgr refresh --force && fwupdmgr update"
alias uf="update_firmwares"
alias ur="rustup update"
alias upper="find . -depth -exec prename 'y/a-z/A-Z/' {} \;"
alias vc="sudo vpnc"
alias vd="sudo vpnc-disconnect"
alias vi="vim"
alias x="exit"

# =================
# ====== Git ======
# =================
alias config="/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME"

GIT_ADD_COMMAND="add"
GIT_BRANCH_COMMAND="branch"
GIT_CLEAN_COMMAND="clean -d -f"
GIT_CHECKOUT_COMMAND="checkout"
GIT_CHERRYPICK_COMMAND="cherry-pick"
GIT_COMMIT_COMMAND="commit"
GIT_DIFF_COMMAND="diff"
GIT_FETCH_COMMAND="fetch"
GIT_PULL_COMMAND="pull"
GIT_PUSH_COMMAND="push"
GIT_REBASE_COMMAND="rebase"
GIT_RESTORE_COMMAND="restore"
GIT_SHOW_COMMAND="show"
GIT_STATUS_COMMAND="status"

alias gdd="git $GIT_ADD_COMMAND"
alias gbr="git $GIT_BRANCH_COMMAND"
alias gcl="git $GIT_CLEAN_COMMAND"
alias gckt="git $GIT_CHECKOUT_COMMAND"
alias gcp="git $GIT_CHERRYPICK_COMMAND"
alias gcmt="git $GIT_COMMIT_COMMAND"
alias gdf="git $GIT_DIFF_COMMAND"
alias gftch="git $GIT_FETCH_COMMAND"
alias gpl="git $GIT_PULL_COMMAND"
alias gpsh="git $GIT_PUSH_COMMAND"
alias grb="git $GIT_REBASE_COMMAND"
alias grst="git $GIT_RESTORE_COMMAND"
alias gshw="git $GIT_SHOW_COMMAND"
alias gst="git $GIT_STATUS_COMMAND"

alias cdd="config $GIT_ADD_COMMAND"
alias cbr="config $GIT_BRANCH_COMMAND"
alias ccl="config $GIT_CLEAN_COMMAND"
alias cckt="config $GIT_CHECKOUT_COMMAND"
alias ccp="config $GIT_CHERRYPICK_COMMAND"
alias ccmt="config $GIT_COMMIT_COMMAND"
alias cdf="config $GIT_DIFF_COMMAND"
alias cftch="config $GIT_FETCH_COMMAND"
alias cpl="config $GIT_PULL_COMMAND"
alias cpsh="config $GIT_PUSH_COMMAND"
alias crb="config $GIT_REBASE_COMMAND"
alias crst="config $GIT_RESTORE_COMMAND"
alias cshw="config $GIT_SHOW_COMMAND"
alias cst="config $GIT_STATUS_COMMAND"

alias update_clones='\
    for d in $GIT_CLONES_DIR/*/ ; \
    do (cd $d; echo "### Entered $d"; git pull; echo ""); done'
alias uc="update_clones"

# =======================
# ====== Hardware  ======
# =======================
alias cpu_boost_off="echo 0 | sudo tee /sys/devices/system/cpu/cpufreq/boost"
alias cpu_boost_on="echo 1 | sudo tee /sys/devices/system/cpu/cpufreq/boost"
alias cpu_boost="cat /sys/devices/system/cpu/cpufreq/boost"
alias cpu_frequency="lscpu | grep MHz"
alias cpu_governor_conservative="sudo cpupower frequency-set -g conservative"
alias cpu_governor_ondemand="sudo cpupower frequency-set -g ondemand"
alias cpu_governor_performance="sudo cpupower frequency-set -g performance"
alias cpu_governor_powersave="sudo cpupower frequency-set -g powersave"
alias cpu_governor_schedutil="sudo cpupower frequency-set -g schedutil"
alias cpu_governor="cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor"

# =============================================
# ====== Frequently Accessed Directories ======
# =============================================
alias c="cd $GIT_CLONES_DIR"
alias d="cd ~/Downloads"
alias m="cd ~/Music"
alias p="cd ~/Pictures"
alias q="cd $HOME"
alias r="cd $GIT_REPOS_DIR"
alias v="cd ~/Videos"

# =====================
# ====== Desktop ======
# =====================
alias gnome_hhkb_mode="\
    gsettings set org.gnome.desktop.input-sources \
    xkb-options \"['compose:rsuper', 'compose:rwin']\""
alias gnome_thinkpad_mode="\
    gsettings set org.gnome.desktop.input-sources \
    xkb-options \"['altwin:swap_alt_win', 'compose:ralt', 'ctrl:swapcaps_hyper']\""

# ====================
# ====== Python ======
# ====================
alias anaconda2="$HOME/anaconda2/bin/python"
alias anaconda3="$HOME/anaconda3/bin/python"
alias conda2="$HOME/anaconda2/bin/conda"
alias conda3="$HOME/anaconda3/bin/conda"

alias anaconda2_update_consistent="conda2 update anaconda"
alias anaconda3_update_consistent="conda3 update anaconda"
alias anaconda2_update_newest="conda2 update conda && conda2 update --all"
alias anaconda3_update_newest="conda3 update conda && conda3 update --all"

# =====================================================
# ====== Games and other (invasive) applications ======
# =====================================================
alias matlab="matlab & disown matlab"
alias matlabcli="matlab -nodesktop -nosplash"
alias rtcw="$GIT_CLONES_DIR/iortcw/SP/build/release-linux-x86_64/iowolfsp.x86_64"
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
alias adjust_filenames="perl -e 'rename\$_,y/ /_/drfor<\"* *\">'"

# ============================================================
# ====== DEBIAN ADDITIONS (I.E., ADDITIONAL ALIASES...) ======
# ============================================================

if on-classic-linux "Debian GNU/Linux" ; then
    alias debian_upgrade_release="sudo apt update && sudo apt upgrade && sudo apt full-upgrade"
fi

# ============================================================
# ====== FEDORA ADDITIONS (I.E., ADDITIONAL ALIASES...) ======
# ============================================================

if on-classic-linux "Fedora Linux" ; then
    alias fedora_dnf_reset="sudo dnf clean all && sudo dnf makecache --refresh -v"
    alias fedora_enable_chez_scheme="sudo dnf copr enable superboum/chez-scheme"
    alias fedora_enable_tdlib_fresh="sudo dnf copr enable carlis/tdlib-fresh"
    alias fedora_enable_tlprepo="\
        sudo dnf in https://repo.linrunner.de/fedora/tlp/repos/releases/tlp-release.fc$(rpm -E %fedora).noarch.rpm"
    alias fedora_enable_rpmfusion="\
        sudo dnf in https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm \
        https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm"
    alias fedora_my_kernels="dnf list installed kernel"
    alias fedora_my_packages="\
        dnf repoquery --qf '%{name}' --userinstalled \
        | grep -v -- '-debuginfo$' \
        | grep -v '^\(kernel-modules\|kernel\|kernel-core\|kernel-devel\)$'"
    alias fedora_my_repositories="dnf repolist enabled"
    alias kk="rpm -qa kernel"
    FEDORA_VERSION_ID=$(cat /etc/os-release | grep VERSION_ID | cut -d '=' -f2)
    FEDORA_VERSION_ID_NEXT=$((FEDORA_VERSION_ID + 1))
    alias fedora_next_download="sudo dnf system-upgrade download --releasever=$FEDORA_VERSION_ID_NEXT"

    function fedora-change-ssh-port() {
        if [[ $1 == "" ]] ; then
            echo "You must specify the port number."
            return
        fi

        NEW_PORT=$1
        OLD_PORT=$(sudo grep "^Port" /etc/ssh/sshd_config | awk '{print $2}')
        sudo sed -i "s/^Port $OLD_PORT/Port $NEW_PORT/g" /etc/ssh/sshd_config

        sudo semanage port -d -p tcp $OLD_PORT
        if [[ $(sudo semanage port -l | grep ssh | grep $NEW_PORT) == "" ]] ; then
            sudo semanage port -a -t ssh_port_t -p tcp $NEW_PORT
            echo "Port $NEW_PORT added to the list of SELinux allowed ports."
        else
            echo "Port $NEW_PORT is already in the list of SELinux allowed ports."
        fi

        sudo firewall-cmd --remove-port=$OLD_PORT/tcp --permanent
        sudo firewall-cmd --add-port=$NEW_PORT/tcp --permanent
        sudo firewall-cmd --reload
        sudo systemctl restart sshd
    }
fi

# ==========================================================
# ====== RHEL ADDITIONS (I.E., ADDITIONAL ALIASES...) ======
# ==========================================================
alias rhel_awake_subscription="\
    sudo subscription-manager remove --all; \
    sudo subscription-manager unregister; \
    sudo subscription-manager clean; \
    sudo subscription-manager register; \
    sudo subscription-manager attach --auto"

##################################################
# ===============================================
# ====== WSL (Windows Subsystem for Linux) ======
# ===============================================
##################################################

function on-wsl() {
    if [[ $(uname -r) == *"WSL"* ]] ; then
        return 0
    else
        return 1
    fi
}


if [[ on-wsl && $SSH_AGENT_PID = "" ]]; then
    eval `ssh-agent -s` > /dev/null 2>&1;
    ssh-add ~/.ssh/id_ed25519 > /dev/null 2>&1;
fi;

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
alias slurm_launch_all='\
    for file in $(find . -type f -name "*$SLURM_SBATCH_EXT"); \
    do sbatch "$file"; done'
alias slurm_kill_all='\
    for file in $(find . -type f -name "$SLURM_JOB_NAME_ROOT*"); \
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

alias su2_build="\
    ./meson.py build --buildtype=release -Dwith-omp=true \
    --prefix=$SU2_PREFIX && ./ninja -C build install"

alias su2_build_debug="\
    ./meson.py build --buildtype=debug -Dwith-omp=true \
    --prefix=$SU2_PREFIX && ./ninja -C build install"

alias su2_rebuild="\
    ./meson.py build --reconfigure --buildtype=release -Dwith-omp=true \
    --prefix=$SU2_PREFIX && ./ninja -C build install"

alias su2_rebuild_debug="\
    ./meson.py build --reconfigure --buildtype=debug -Dwith-omp=true \
    --prefix=$SU2_PREFIX && ./ninja -C build install"

alias su2_clean_dir="find ./* -type f -not -name '*.cfg' -not -name '*.su2' -not -name '*.sl' -delete"
alias su2_wipe_build="./meson.py setup --wipe build/"


#######################################
# ===================================
# ====== AUTO-GENERATED STUFF  ======
# ===================================
#######################################

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


# See https://stackoverflow.com/questions/67938486/after-installing-npm-on-wsl-ubuntu-20-04-i-get-the-message-usr-bin-env-bash
if [ -x "$(command -v ng | grep -v /mnt/c/)" ] ; then
    # Load Angular CLI autocompletion.
    source <(ng completion script)
fi

if [ -x "$(command -v fnm)" ] ; then
    # fnm
    export PATH=~/.fnm:$PATH
    eval "`fnm env`"
fi

