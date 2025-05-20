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
