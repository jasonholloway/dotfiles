source $BYOBU_PREFIX/share/byobu/profiles/tmux

# below makes emacsclient work with truecolor
set -g default-terminal "tmux-direct"
set -sg terminal-features ",*:RGB"
