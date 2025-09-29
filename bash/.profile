#!/bin/sh
export EDITOR="emacs"
export SUDO_EDITOR="emacs"
export VISUAL="emacs"
export BROWSER="firefox"

# Set XDG directories
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export XDG_STATE_HOME=$HOME/.local/state
export CODE=$HOME/code


# Use XDG directories for various programs
export HISTFILE=$XDG_DATA_HOME/bash/history
export HISTSIZE=-1
export HISTFILESIZE=-1

export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket
export GNUPGHOME=$XDG_DATA_HOME/gnupg
export SSB_HOME=$XDG_DATA_HOME/zoom
export CUDA_CACHE_PATH=$XDG_CACHE_HOME/nv
export RUSTUP_HOME=$XDG_DATA_HOME/rustup
export CARGO_HOME=$XDG_DATA_HOME/cargo
export NPM_CONFIG_USERCONFIG=$XDG_CONFIG_HOME/npm/npmrc
export DENO_INSTALL_ROOT=$XDG_DATA_HOME/deno

export RYE_HOME=$XDG_CONFIG_HOME/rye
export PYTHON_HISTORY=$XDG_STATE_HOME/python/history
export PYTHONPYCACHEPREFIX=$XDG_CACHE_HOME/python
export PYTHONUSERBASE=$XDG_DATA_HOME/python
export IPYTHONDIR=$XDG_CONFIG_HOME/jupyter
export JUPYTER_CONFIG_DIR=$XDG_CONFIG_HOME/jupyter
export PYENV_ROOT=$XDG_DATA_HOME/pyenv


# Other non-XDG home dirs
export JULIA_DEPOT_PATH=$HOME/.julia
export JULIAUP_DEPOT_PATH=$HOME/.juliaup
# export GOPATH=$DEV/go


# Try to get LightDM to use XDG directories (doesn't seem to work)
export USERXSESSION=$XDG_CACHE_HOME/x11/xsession
export USERXSESSIONRC=$XDG_CACHE_HOME/x11/xsessionrc
export ALTUSERXSESSION=$XDG_CACHE_HOME/x11/Xsession
export ERRFILE=$XDG_CACHE_HOME/x11/xsession-errors


# N.B. /tmp is an in-memory tmpfs on Arch. change to /var/tmp if OOM
export TMPDIR=/tmp


export PATH=$XDG_CONFIG_HOME/emacs/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$RYE_HOME/shims:$PATH
export PATH=$CARGO_HOME/bin:$PATH
export PATH=$DENO_INSTALL_ROOT/bin:$PATH
export PATH=$JULIAUP_DEPOT_PATH/bin:$PATH
export PATH=$JULIA_DEPOT_PATH/bin:$PATH # For Pkg apps

# Various hacks etc.
# export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/cuda/extras/CUPTI/lib64
export MAKEFLAGS="-j$(expr $(nproc) - 2)"

export JULIA_PKG_USE_CLI_GIT=true
export JULIA_PKG_DEVDIR=$CODE

# This is needed to use Jax from Julia through PythonCall
# see: https://github.com/JuliaPy/PyCall.jl/issues/722
# https://github.com/JuliaPy/PyCall.jl/issues/990
# export LD_PRELOAD=/usr/lib64/libstdc++.so.6


# GUIX_PROFILE="$HOME/.guix-profile"
# . "$GUIX_PROFILE/etc/profile"
# export GUIX_LOCPATH=$GUIX_PROFILE/lib/locale

# GUIX_PROFILE="$HOME/.config/guix/current"
# . "$GUIX_PROFILE/etc/profile"


# >>> juliaup initialize >>>

# !! Contents within this block are managed by juliaup !!

case ":$PATH:" in
    *:/home/rviljoen/.juliaup/bin:*)
        ;;

    *)
        
        ;;
esac

# <<< juliaup initialize <<<
