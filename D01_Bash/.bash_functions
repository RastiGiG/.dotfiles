#----------------------------------------------------------------------------------------------------------------------
#  ____    _    ____  _   _ _____ _   _ _   _  ____ _____ ___ ___  _   _ ____  
# | __ )  / \  / ___|| | | |  ___| | | | \ | |/ ___|_   _|_ _/ _ \| \ | / ___| 
# |  _ \ / _ \ \___ \| |_| | |_  | | | |  \| | |     | |  | | | | |  \| \___ \ 
# | |_) / ___ \ ___) |  _  |  _| | |_| | |\  | |___  | |  | | |_| | |\  |___) |
# |____/_/   \_\____/|_| |_|_|    \___/|_| \_|\____| |_| |___\___/|_| \_|____/ 
#                                                                              
# - Collection of Functions for Bash -
# Source:         - .dotfiles/000_OrgFiles/BashFuncs.org
# Target:         - .dotfiles/D01_Bash/.bash_functions
# Author Email:   - randomly.ventilates@simplelogin.co
# Author GitHub:  - https://github.com/RastiGiG/
#
# These Functions were adapted from:
# - TerminalForLife:   https://github.com/terminalforlife
#
#----------------------------------------------------------------------------------------------------------------------

colors() {
      local fgc bgc vals seq0

      printf "Color escapes are %s\n" '\e[${value};...;${value}m'
      printf "Values 30..37 are \e[33mforeground colors\e[m\n"
      printf "Values 40..47 are \e[30;43mbackground colors\e[m\n"
      printf "Value  1 gives a  \e[1mbold-faced look\e[m\n\n"

      # foreground colors
      for fgc in {30..37}; do
              # background colors
              for bgc in {40..47}; do
                      fgc=${fgc#37} # white
                      bgc=${bgc#40} # black

                      vals="${fgc:+$fgc;}${bgc}"
                      vals=${vals%%;}

                      seq0="${vals:+\e[${vals}m}"
                      printf "  %-9s" "${seq0:-(default)}"
                      printf " ${seq0}TEXT\e[m"
                      printf " \e[${vals:+${vals+$vals;}}1mBOLD\e[m\n"
              done
              echo; echo
      done
}

# small function to get an overview of the disk usage in the current directory
overview ()
{
    du -h --max-depth=1 | sed -r '
       $d; s/^([.,0-9]+[KMGTPEZY]\t)\.\//\1/
     ' | sort -hr | column
}

Err(){
    printf '\e[91mError\e[0m: %s\n' "$1" 1>&2
    return 1
}

AskYN() {
    while :; do
        read -p "$1 (Y/N) "
        case ${REPLY,,} in
            y|yes)
                return 0 ;;
            n|no)
                return 1 ;;
            '')
                Err 'Response required - Answer again' ;;
            *)
                Err 'Invalid response - Answer again' ;;
        esac
    done
}

# arcext - archive extractor
# usage: arcext <file>
arcext ()
{
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1   ;;
            *.tar.gz)    tar xzf $1   ;;
            *.bz2)       bunzip2 $1   ;;
            *.rar)       unrar x $1     ;;
            *.gz)        gunzip $1    ;;
            *.tar)       tar xf $1    ;;
            *.tbz2)      tar xjf $1   ;;
            *.tgz)       tar xzf $1   ;;
            *.zip)       unzip $1     ;;
            *.Z)         uncompress $1;;
            *.7z)        7z x $1      ;;
            *)           echo "'$1' cannot be extracted via ex()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

#Functions to automatically evaluate shasums
sha256()
{
    echo "$1 $2" | sha256sum --check
}

sha512()
{
    echo "$1 $2" | sha512sum --check
}

sha1()
{
    echo "$1 $2" | sha1sum --check
}
sha224()
{
    echo "$1 $2" | sha224sum --check
}

sha384()
{
    echo "$1 $2" | sha384sum --check
}
