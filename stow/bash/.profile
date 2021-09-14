export EDITOR="emacs"
export VISUAL="emacs"

# Set XDG directories
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export PROG=$HOME/prog

# Use XDG directories for various programs
export HISTFILE=$XDG_DATA_HOME/bash/history
export GNUPGHOME=$XDG_DATA_HOME/gnupg
export SSB_HOME=$XDG_DATA_HOME/zoom
export IPYTHONDIR=$XDG_CONFIG_HOME/jupyter
export JUPYTER_CONFIG_DIR=$XDG_CONFIG_HOME/jupyter
export CUDA_CACHE_PATH=$XDG_CACHE_HOME/nv

# Other non-XDG home dirs
export JULIA_DEPOT_PATH=$PROG/julia
export GOPATH=$PROG/go

# Try to get LightDM to use XDG directories (doesn't seem to work)
export USERXSESSION=$XDG_CACHE_HOME/x11/xsession
export USERXSESSIONRC=$XDG_CACHE_HOME/x11/xsessionrc
export ALTUSERXSESSION=$XDG_CACHE_HOME/x11/Xsession
export ERRFILE=$XDG_CACHE_HOME/x11/xsession-errors

export PATH=$PATH:$XDG_CONFIG_HOME/emacs/bin

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/cuda/extras/CUPTI/lib64
export MAKEFLAGS="-j$(nproc)"
