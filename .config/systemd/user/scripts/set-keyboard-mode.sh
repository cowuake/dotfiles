if [[ $(lsusb | grep 'HHKB\|PFU') != "" ]]; then
    gsettings set org.gnome.desktop.input-sources xkb-options \
	      "['compose:rsuper', 'compose:rwin']"
else
    gsettings set org.gnome.desktop.input-sources xkb-options \
	      "['lv3:ralt_switch', 'altwin:swap_alt_win', 'compose:ralt', 'ctrl:swapcaps_hyper']"
fi

