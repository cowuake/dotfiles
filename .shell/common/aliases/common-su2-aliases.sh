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
