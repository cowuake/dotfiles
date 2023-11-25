if [[ $(lsusb | grep 'HHKB\|PFU') != "" ]]; then
    gsettings set org.gnome.desktop.input-sources xkb-options \
	      "['compose:rsuper', 'compose:rwin']"
else
    gsettings set org.gnome.desktop.input-sources xkb-options \
	      "['altwin:swap_alt_win', 'compose:ralt', 'ctrl:swapcaps_hyper']"
fi

