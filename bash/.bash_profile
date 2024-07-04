#
# ~/.bash_profile
#

# ~/.bash_profile runs in login shells (i.e. when bash is spawned by
# login in a TTY). It does *not* run in ordinary interactive
# (i.e. connected to a terminal) shells. ~/.bashrc is run for
# interactive shells only. Login shells are usually interactive, so
# don't really need to run ~/.bashrc here?

#~/.profile contains environment variables common to Bourne-compatible
# shells (not bash specific) - this is only run at login, interactive
# shells will inherit the login shells environment.


[[ -f ~/.profile ]] && . ~/.profile
[[ -f ~/.bashrc ]] && . ~/.bashrc

[ "$(tty)" = "/dev/tty1" ] && exec sway --unsupported-gpu
