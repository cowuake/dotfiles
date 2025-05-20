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
alias install_fnm='curl -fsSL https://fnm.vercel.app/install | bash -s -- --install-dir "$HOME/bin" --skip-shell'
alias install_jenv='git clone https://github.com/jenv/jenv.git ~/.jenv'
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
GIT_COUNT_COMMITS_COMMAND="rev-list --count HEAD"
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
alias gcm="git $GIT_COUNT_COMMITS_COMMAND"
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
alias ccm="config $GIT_COUNT_COMMITS_COMMAND"
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


# ====================
# ====== Docker ======
# ====================
alias dlc="docker container list"
alias dli="docker image list"
alias dlv="docker volume list"
alias dcd="docker compose down --remove-orphans --volumes"
alias dp="docker system prune --volumes --force"

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
