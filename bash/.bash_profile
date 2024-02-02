#
# ~/.bash_profile
#

[[ -f ~/.profile ]] && . ~/.profile
[[ -f ~/.env ]] && . ~/.env
[[ -f ~/.bashrc ]] && . ~/.bashrc

[ "$(tty)" = "/dev/tty1" ] && exec sway
