for d in stow/*/; do
    pkg=$(basename $d)
    [ "$1" == "-D" ] && stow -D -d stow -t ~ $pkg || stow -S -d stow -t ~ $pkg
done
