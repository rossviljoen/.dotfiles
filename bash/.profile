#!/bin/sh
export EDITOR="emacs"
export VISUAL="emacs"


# Set XDG directories
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export XDG_STATE_HOME=$HOME/.local/state
export DEV=$HOME/dev


# Use XDG directories for various programs
export HISTFILE=$XDG_DATA_HOME/bash/history
export HISTSIZE=-1
export HISTFILESIZE=-1

export GNUPGHOME=$XDG_DATA_HOME/gnupg
export SSB_HOME=$XDG_DATA_HOME/zoom
export CUDA_CACHE_PATH=$XDG_CACHE_HOME/nv
export RUSTUP_HOME=$XDG_DATA_HOME/rustup
export CARGO_HOME=$XDG_DATA_HOME/cargo
export NPM_CONFIG_USERCONFIG=$XDG_CONFIG_HOME/npm/npmrc

export PYTHON_HISTORY=$XDG_STATE_HOME/python/history
export PYTHONPYCACHEPREFIX=$XDG_CACHE_HOME/python
export PYTHONUSERBASE=$XDG_DATA_HOME/python
export IPYTHONDIR=$XDG_CONFIG_HOME/jupyter
export JUPYTER_CONFIG_DIR=$XDG_CONFIG_HOME/jupyter
export PYENV_ROOT=$XDG_DATA_HOME/pyenv


# Other non-XDG home dirs
export JULIA_DEPOT_PATH=$DEV/julia
export GOPATH=$DEV/go


# Try to get LightDM to use XDG directories (doesn't seem to work)
export USERXSESSION=$XDG_CACHE_HOME/x11/xsession
export USERXSESSIONRC=$XDG_CACHE_HOME/x11/xsessionrc
export ALTUSERXSESSION=$XDG_CACHE_HOME/x11/Xsession
export ERRFILE=$XDG_CACHE_HOME/x11/xsession-errors


# N.B. /tmp is an in-memory tmpfs on Arch. change to /var/tmp if OOM
export TMPDIR=/tmp
export PATH=$PATH:$XDG_CONFIG_HOME/emacs/bin:$HOME/.local/bin


# Various hacks etc.
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/cuda/extras/CUPTI/lib64
export MAKEFLAGS="-j$(nproc)"

# This is needed to use Jax from Julia through PythonCall
# see: https://github.com/JuliaPy/PyCall.jl/issues/722
# https://github.com/JuliaPy/PyCall.jl/issues/990
# export LD_PRELOAD=/usr/lib64/libstdc++.so.6
