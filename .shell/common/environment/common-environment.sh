# ==================
# ====== PATH ======
# ==================

export PATH=$PATH:/usr/local/go/bin

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
    $HOME/.julia/bin         # Julia binaries
    $HOME/.jenv/bin          # jenv
    $HOME/go/bin
    /usr/lib64/openmpi/bin   # OPENMPI binaries, e.g., mpicc, mpicxx
    /usr/lib64/mpich/bin     # MPICH binaries, e.g., mpicc, mpicxx
    /opt/mssql-tools/bin     # Microsoft SQL Server CLI utilities
    /opt/mssql-tools18/bin   # Microsoft SQL Server CLI utilities (release 18)
)

for source in "${source_dirs[@]}"
do
    if [ -d "$source" ] ; then
        export PATH="$source":$PATH
    fi
done

if [ -f ~/.jenv/bin/jenv ] ; then
    eval "$(jenv init -)" > /dev/null 2>&1
    jenv enable-plugin export > /dev/null 2>&1
fi

OPENMPI_INSTALL_DIR=/usr/lib64/openmpi
MPICH_INSTALL_DIR=/usr/lib64/mpich

### MATLAB
PATH=$HOME/MATLAB/R2020b/bin:$PATH

# refine by NASA
PATH=$GIT_CLONES_DIR/refine/build/bin:$PATH

### JULIA
export JULIA_NUM_THREADS=$CPU_CORES

### GO
export GOPATH=$HOME/go

### TEXT EDITOR
# if [ -f "/usr/bin/emacs" ] ; then
#     if ps -aux | grep "emacs  --daemon" > /dev/null 2>&1 ; then
# 	EDITOR=emacsclient
#     else
# 	EDITOR=EMACS
#     fi
# else
#     if [ -f "/usr/bin/vim" ] ; then
#        EDITOR=vim
#     fi
# fi
