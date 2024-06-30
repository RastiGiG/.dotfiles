#----------------------------------------------------------------------------------------------------------------------
#  ____    _    ____  _   _ _____ _   _ _   _  ____ _____ ___ ___  _   _ ____
# | __ )  / \  / ___|| | | |  ___| | | | \ | |/ ___|_   _|_ _/ _ \| \ | / ___|
# |  _ \ / _ \ \___ \| |_| | |_  | | | |  \| | |     | |  | | | | |  \| \___ \
# | |_) / ___ \ ___) |  _  |  _| | |_| | |\  | |___  | |  | | |_| | |\  |___) |
# |____/_/   \_\____/|_| |_|_|    \___/|_| \_|\____| |_| |___\___/|_| \_|____/
#
# - Collection of Functions for Bash -
# Source:         - .dotfiles/000_OrgFiles/BashFuncs.org
# Target:         - .dotfiles/D01_Bash/bash_functions.sh
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
