if [ ! -d "~/.cfg" ]; then
    cp -rT $(pwd) ~/
    mv ~/.git ~/.cfg
fi

