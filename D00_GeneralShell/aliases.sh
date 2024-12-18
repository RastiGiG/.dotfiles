#----------------------------------------------------------------------------------------------------------------------
#     _    _ _
#    / \  | (_) __ _ ___  ___  ___
#   / _ \ | | |/ _` / __|/ _ \/ __|
#  / ___ \| | | (_| \__ \  __/\__ \
# /_/   \_\_|_|\__,_|___/\___||___/
#
# - Collection of Aliases used in Bash and Zsh -
# Source:         - .dotfiles/000_OrgFiles/Aliases.org
# Target:         - .dotfiles/D00_Aliases/aliases.sh
# Author Email:   - randomly.ventilates@simplelogin.co
# Author GitHub:  - https://github.com/RastiGiG/
#----------------------------------------------------------------------------------------------------------------------

#-----------------------------------------SAFETY ALIASES
# A bunch of safety aliases
alias cp='cp -vi'                         # confirm before overwriting something
alias rm='rm -vi --preserve-root'         # confirm before deleting accidently, disable changes to root
alias mv='mv -vi'                         # confirm before moving/renaming
alias ln='ln -vi'                         # confirm before overwriting

alias chown='chown -v --preserve-root'    # disable changes to root
alias chgrp='chgrp -v --preserve-root'    # disable changes to root
alias chmod='chmod -v --preserve-root'    # disable changes to root

alias rmdir='rmdir -v'                    # verbose
alias mkdir='mkdir -v'                    # verbose
alias mkd='mkdir -p'                      # also creates parent directories

#-----------------------------------------BETTER DEFAULTS
# Better defaults for ls
alias ls='ls --quoting-style=shell -pq\
         --time-style=iso --color=auto\
         --group-directories-first\
         --show-control-chars -v'

# Colors for grep
alias grep='grep --colour=auto'
alias egrep='egrep --colour=auto'
alias fgrep='fgrep --colour=auto'

#-----------------------------------------READING AND LISTING
# Reading and Listing aliases
alias la='ls -al'                         # detailed version of ls listing hidden files
alias ll='ls -l'                          # detailed version of ls listing
alias lq='ls --quoting-style=literal'     # don't quote files/dirs with whitespaces
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias np='nano -w PKGBUILD'
alias more=less                           # more is pretty much useless noadays

#-----------------------------------------EMACS DAEMON
# Aliases to use emacs as a daemon
alias ed="emacs --daemon"                     # Start daemon
alias ec="emacsclient -tc"                    # Connect to daemon
alias ek="emacsclient --eval '(kill-emacs)'"  # Kill Daemon

alias smca='smartctl -a'             # Show device info
alias smcts='smartctl -t short'      # Start a short selftest
alias smctl='smartctl -t long'       # Start a long selftest
alias smcstl='smartctl -l selftest'  # Start a long selftest

alias bfsshow="sudo btrfs filesystem show"

alias bfsstatus="sudo btrfs scrub status"
alias bfsstart="sudo btrfs scrub start"

alias snapshot="sudo btrfs subvolume snapshot"
alias subvollist="sudo btrfs subvolume list"

alias nrsd='sudo nixos-rebuild switch --flake ~/.dotnix#descartes'

alias hmsr='home-manager switch --flake ~/.dotnix#rastibasti'
alias hmss='home-manager switch --flake ~/.dotnix#sebastian'

alias dc='docker compose'

alias pc='podman-compose'

alias ldg="ledger"
alias ldgsd="ledger --sort='-date'"

# Alternate Configuration File for ABCDE - A Better CD Encoder
alias abcde="abcde -c $HOME/.config/abcde/abcde.conf"
