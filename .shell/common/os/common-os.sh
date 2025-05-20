##############################################
# ===========================================
# ====== Distro Facilities (GNU/Linux) ======
# ===========================================
##############################################

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
        elif on-classic-linux "Pop!" ; then
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
