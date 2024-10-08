#----------------------------------------------------------------------------------------------------------------------
#  ____    _    ____  _   _
# | __ )  / \  / ___|| | | |
# |  _ \ / _ \ \___ \| |_| |
# | |_) / ___ \ ___) |  _  |
# |____/_/   \_\____/|_| |_|
#
# - Bash, the Bourne Again Shell -
# Source:         - .dotfiles/00_OrgFiles/BashConfig.org
# Target:         - .dotfiles/D01_Bash/.bashrc
# Author Email:   - randomly.ventilates@simplelogin.co
# Author GitHub:  - https://github.com/RastiGiG/
#
# Parts of this Configuration were adapted from:
# - TerminalForLife:   https://github.com/terminalforlife
#
#----------------------------------------------------------------------------------------------------------------------

#----------------------------BASICS
# If not run in interactive mode do nothing and return
# Return himBHs -> see man bash -> SHELL BUILTIN COMMANDS -> set subsection for the meaning
[[ $- != *i* ]] && return

# Source global definitions
if [ -f /etc/bashrc ]; then
      . /etc/bashrc
fi

# Control Access to Local XServer
xhost +local:root > /dev/null 2>&1

# Adjust completions for sudo
complete -cf sudo

# export QT_SELECT=4

#----------------------------SHELL OPTIONS

# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.  #65623
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
# default: enabled (for interactive shells)
shopt -s checkwinsize

# Enable Parameter Expansion, Command Substitution etc for Prompts
# default: enabled
shopt -s promptvars

# Use $PATH to locate arguments supplied to 'source'
# default: enabled
shopt -s sourcepath

# default: enabled (for interactive shells)
shopt -s expand_aliases

# Enable history appending instead of overwriting.
shopt -s histappend

# Safe multiline commands to one line in history
shopt -s cmdhist

# Literate History, multiline comments with literal '\n'
# requires: cmdhist
shopt -s lithist

# Bash attemps spelling correction on dirs
shopt -s dirspell

# Replace directory names with results of word expansion
shopt -s direxpand

# Pattern '**' matches everything in Path Expansion
shopt -s globstar

# Allow '#' comments in interactive shells
shopt -s interactive_comments

# Case insensitive matches for '[[' or 'case'
shopt -s nocasematch

# Bash will attempt hostcompletion when @ is involved
# default: enabled
shopt -s hostcomplete

# Enable Programmable Completion
# default: enabled
shopt -s progcomp

# Default since Bash 4.2.
shopt -s complete_fullquote

#----------------------------PROMPT
# Safe certain ANSI color escape sequences.
PROMPT_PARSER() {
    # Initialize Variables
    local GitSymbs GitStatus GitTopDir GitCheck GitBranch CurrentGitBranch\
        X Line Desc Buffer ModifiedFiles TTLCommits NFTTL

    # Set Colors
    # Legend:
    # "C" only - Normal
    # "CB"     - Bold Variant
    # "C_B"    - Bright Variant
    # "CB_B"   - Bright and Bold Variant
    # "C_D"    - Dark Variant
    # "C_DB"   - Darkend Bright Variant
    local C_Cyan='\033[36m' C_BCyan='\033[96m' CB_Cyan='\033[01;36m' CB_BCyan='\033[01;96m'\
          C_Cyan_Back='\033[46m' C_BCyan_Back='\033[106m'
          C_Red='\e[31m' C_BRed='\e[91m' CB_Red='\e[01;31m' CB_BRed='\e[01;91m'\
          C_Green='\e[32m' C_BGreen='\e[92m' CB_Green='\e[01;32m' CB_BGreen='\e[01;92m'\
          C_DGreen='\e[02;32m' C_DBGreen='\e[02;92m' C_Green_Back='\e[42m'\
          C_BGreen_Back='\e[102m'\
          C_Blue='\e[34m' C_BBlue='\e[94m' C_DBlue='\e[02;34m' C_DBBlue='\e[02;94m'\
          CB_Blue='\e[01;34m' CB_BBlue='\e[01;94m' C_Blue_Back='\e[44m'\
          C_BBlue_Back='\e[104m'\
          C_Yellow='\e[33m' C_BYellow='\e[93m' C_DYellow='\e[02;33m' C_DBYellow='\e[02;93m'\
          C_Magenta='\033[35m' C_BMagenta='\033[95m' C_DMagenta='\033[2;35m'\
          C_DBMagenta='\033[02;95m' CB_Magenta='\033[01;35m' CB_BMagenta='\033[01;95m'\
          C_Magenta_Back='\033[45m' C_BMagenta_Back='\033[105m' CD_BMagenta_Back='\033[02;105m'\
          C_Grey='\e[37m' C_White='\e[97m' CB_Grey='\e[01;37m' CB_White='\e[01;97m'\
          C_Black='\033[30m' C_Black_Back='\033[40m' CB_Black='\033[01;30m' \
          C_Reset='\e[0m'

    # Evaluate Exit Status (safed to arg1, see below)
    X="$1 "
    # if the smallest match for X is 0 (no error), set X to be an empty string
    (( ${X% } == 0 )) && X=

    # SSH - Prompt for Working Remotely
    # If I'm on a remote server, just use a barebones prompt, with the exit
    # status, if non-zero, and a note saying you're working remotely.
    if [[ -n $SSH_CLIENT ]]; then
        if [[ -n $X ]]; then
            PS1="\n\[$C_Grey\]<remote>\[$C_Reset\] \[${CB_BMagenta}\][\u@\h\[${C_Reset}\]\[$C_BRed\]\n$X\[$C_Reset\] \[$CB_BMagenta\]\$\[$C_Reset\] "
        else
            PS1="\n\[$C_Grey\]<remote>\[$C_Reset\] \[${CB_BMagenta}\][\u@\h\[${C_Reset}\]\[$CB_BMagenta\]\n\$\[$C_Reset\] "
        fi

        return
    fi

    # GUIX - Prompt for working with guix development Shells
    # If I'm in a guix shell, add the [dev] string to the Prompt
    # status, if non-zero, and a note saying you're working remotely.
    if [ -n "$GUIX_ENVIRONMENT" ]; then
  	      # if [[ $PS1 =~ (.*)"\\$" ]]; then
  	      # 	PS1="\n\[${CB_BGreen}${BASH_REMATCH[1]}\]\n\$\[$C_Reset\] "
  	      # else
  	      # fi
  	      PS1="\n\[(${CB_BGreen}\u@\h\]\[${C_Reset}\]\[${C_White}\])-(\[${CB_Blue}\]\W\[${C_Reset}\]\[${CB_BGreen}\]${C_White})\n<dev> \[${CB_BGreen}->\]\[$C_Reset\] "

        return
    fi

    # PYVENV - Python Virtualenv Prompt to show if virtualenv is active
    # If I'm on a remote server, just use a barebones prompt, with the exit
    # status, if non-zero, and a note saying you're working remotely.
    if [[ -n $VIRTUAL_ENV ]]; then
        if [[ -n $X ]]; then
            PS1="\n\[${C_Cyan_Back}${C_Black}\] ${VIRTUAL_ENV##*/} <VIRTUAL> \[${C_Reset}\]| \[${C_Cyan}\]\W\[${C_Reset}\]\n\[$C_BRed\]${X}\[$C_Reset\]\[$C_Cyan\]\$\[$C_Reset\] "
        else
            PS1="\n\[${C_Cyan_Back}${C_Black}\] ${VIRTUAL_ENV##*/} <VIRTUAL> \[${C_Reset}\]| \[${CB_Cyan}\]\W\[${C_Reset}\]\n\[$C_Cyan\]\$\[$C_Reset\] "
        fi

        return
    fi

    # GIT - Prompt customization for Working in Git Repos

    # The first check was added as a result of Issue #3 and a recent (April -
    # 2022) change to git(1) which was pushed in response to a CVE.
    GitCheck=`git rev-parse --is-inside-work-tree 2>&1`
    if [[ $GitCheck == 'fatal: unsafe repository '* ]]; then
        Desc="${C_BRed}!!  ${C_Grey}Unsafe repository detected."
    elif [[ $GitCheck == 'fatal: '* ]]; then
        # Don't want to catch all fatals straight away, because not being in a
        # git(1) repository is a 'fatal' error -- stupid git(1).
        #
        # This lets me catch specific unwanted fatal errors, as well as general
        # fatal errors which are one of the specific ones.
        if [[ $GitCheck != 'fatal: not a git repository '* ]]; then
            Desc="${C_BRed}!!  ${C_Grey}Unrecognised fatal error detected."
        fi
    elif [[ $GitCheck == true ]]; then
        # Custom Symbols for git
        GitSymbs=(
            '≎' # 0: Clean
            '≍' # 1: Uncommitted changes
            '≭' # 2: Unstaged changes
            '≺' # 3: New file(s)
            '⊀' # 4: Removed file(s)
            '≔' # 5: Initial commit
            '∾' # 6: Branch is ahead
            '⮂' # 7: Fix conflicts
            '-' # 8: Removed file(s)
        )

        # Store current Git Status
        GitStatus=`git status 2>&1`
        # Store Toplevel Directory
        GitTopDir=`git rev-parse --show-toplevel 2>&1`
        # Store basename of Toplevel Directory
        GitTopDirBase=${GitTopDir##*/}
        # Store Name of GIT-Subdir in current Repo
        GitDir=`git rev-parse --git-dir 2>&1`

        # Change Description if in GIT-Subdir
        if [[ $GitDir == . || $GitDir == "${PWD%%/.git/*}/.git" ]]; then
            Desc="${C_BRed}∷  ${C_Grey}Looking under the hood..."
        else
            if [[ -n $GitTopDir ]]; then
                # Get the current branch name.
                IFS='/' read -a A < "$GitTopDir/.git/HEAD"
                CurrentGitBranch=${A[${#A[@]}-1]}
            fi

            # The following is in a very specific order of priority.
            if [[ -z $(git rev-parse --branches 2>&1) ]]; then
                Desc="${C_BCyan}${GitSymbs[5]}  ${C_Grey}Branch '${CurrentGitBranch:-?}' awaits its initial commit."
            else
                while read -ra Line; do
                    if [[ ${Line[0]}${Line[1]}${Line[2]} == \(fixconflictsand ]]; then
                        Desc="${C_BCyan}${GitSymbs[7]}  ${C_Grey}Branch '${CurrentGitBranch:-?}' has conflict(s)."
                        break
                    elif [[ ${Line[0]}${Line[1]} == Untrackedfiles: ]]; then
                        NFTTL=0
                        while read -a Line; do
                            [[ ${Line[0]} == ?? ]] && (( NFTTL++ ))
                        done <<< "$(git status --short 2>&1)"
                        printf -v NFTTL "%'d" $NFTTL

                        Desc="${C_BCyan}${GitSymbs[3]}  ${C_Grey}Branch '${CurrentGitBranch:-?}' has $NFTTL new file(s)."
                        break
                    elif [[ ${Line[0]} == deleted: ]]; then
                        Desc="${C_BCyan}${GitSymbs[8]}  ${C_Grey}Branch '${CurrentGitBranch:-?}' detects removed file(s)."
                        break
                    elif [[ ${Line[0]} == modified: ]]; then
                        readarray Buffer <<< "$(git --no-pager diff --name-only 2>&1)"
                        printf -v ModifiedFiles "%'d" ${#Buffer[@]}
                        Desc="${C_BCyan}${GitSymbs[2]}  ${C_Grey}Branch '${CurrentGitBranch:-?}' has $ModifiedFiles modified file(s)."
                        break
                    elif [[ ${Line[0]}${Line[1]}${Line[2]}${Line[3]} == Changestobecommitted: ]]; then
                        Desc="${C_BCyan}${GitSymbs[1]}  ${C_Grey}Branch '${CurrentGitBranch:-?}' has changes to commit."
                        break
                    elif [[ ${Line[0]}${Line[1]}${Line[3]} == Yourbranchahead ]]; then
                        printf -v TTLCommits "%'d" "${Line[7]}"
                        Desc="${C_BCyan}${GitSymbs[6]}  ${C_Grey}Branch '${CurrentGitBranch:-?}' leads by $TTLCommits commit(s)."
                        break
                    elif [[ ${Line[0]}${Line[1]}${Line[2]} == nothingtocommit, ]]; then
                        printf -v TTLCommits "%'d" "$(git rev-list --count HEAD 2>&1)"

                        Desc="${C_BCyan}${GitSymbs[0]}  ${C_Grey}Branch '${CurrentGitBranch:-?}' is $TTLCommits commit(s) clean."
                        break
                    fi
                done <<< "$GitStatus"
            fi
        fi
    fi

    # Set the Default Prompt here
    if [[ -n $Desc ]]; then
        PS1="\n\[${C_Green_Back}${C_Black}\] ${GitTopDirBase} \[${C_Reset}\]| \[${C_Green}\]\W\[${C_Reset}\]\n \[${C_Reset}\]${Desc}\[${C_Reset}\]\n\[$C_BRed\]${X}\[$C_Reset\]\[$C_Green\]\$ \[$C_Reset\]"
    else
        PS1="\n\[${CB_Magenta}\][\u@\h\[${C_Reset}\] \[${CB_Blue}\]\w\[${C_Reset}\]\[${CB_Magenta}\]]\n\[${C_Reset}\]\[$C_BRed\]${X}\[$C_Reset\]\[${CB_Magenta}\]\$ \[${C_Reset}\]"
    fi
}

# Set the Prompt Command (Safe Exit Status to variable X)
PROMPT_COMMAND='PROMPT_PARSER $?'

#----------------------------HISTORY
# HISTORY SETTINGS
HISTSIZE=10000
# Move History to .cache
HISTFILE=$HOME/.cache/shell/history
# Don't put duplicate lines or lines starting with spaces into the history
HISTCONTROL='ignoreboth'
# Add Time String to History
HISTTIMEFORMAT='%Y-%m-%d %T '
# Ignore Commands
HISTIGNORE="exit *:clear *:qpdf --encrypt*:history"

#--------------ENVIRONMENT VARIABLES

# Set the Default File for Remind
export DOTREMINDERS="$HOME/Calendar/00_reminders.rem"

# Set GPG Teletype to match $tty
GPG_TTY=$(tty)
export GPG_TTY

#----------------------------COLORFUL TERMINAL
# Change the window title of X terminals
# case ${TERM} in
#     xterm*|rxvt*|Eterm*|aterm|kterm|gnome*|interix|konsole*)
#         PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\007"'
#         ;;
#     screen*)
#         PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\033\\"'
#         ;;
# esac

use_color=true

# Set colorful PS1 only on colorful terminals.
# dircolors --print-database uses its own built-in database
# instead of using /etc/DIR_COLORS.  Try to use the external file
# first to take advantage of user additions.  Use internal bash
# globbing instead of external grep binary.
safe_term=${TERM//[^[:alnum:]]/?}   # sanitize TERM
match_lhs=""
[[ -f ~/.dir_colors   ]] && match_lhs="${match_lhs}$(<~/.dir_colors)"
[[ -f /etc/DIR_COLORS ]] && match_lhs="${match_lhs}$(</etc/DIR_COLORS)"
[[ -z ${match_lhs}    ]] \
    && type -P dircolors >/dev/null \
    && match_lhs=$(dircolors --print-database)
[[ $'\n'${match_lhs} == *$'\n'"TERM "${safe_term}* ]] && use_color=true

#  if ${use_color} ; then
#      # Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
#      if type -P dircolors >/dev/null ; then
#          if [[ -f ~/.dir_colors ]] ; then
#              eval $(dircolors -b ~/.dir_colors)
#          elif [[ -f /etc/DIR_COLORS ]] ; then
#              eval $(dircolors -b /etc/DIR_COLORS)
#          fi
#      fi
#
#      if [[ ${EUID} == 0 ]] ; then
#          PS1='\[\033[01;31m\][\h\[\033[01;36m\] \W\[\033[01;31m\]]\$\[\033[00m\] \\n'
#      else
#          PS1='\[\033[01;32m\][\u@\h\[\033[01;37m\] \W\[\033[01;32m\]]\$\[\033[00m\n\]'
#      fi
#
#      # Some where moved to funcs
#      # alias ls='ls --color=auto'
#      # alias grep='grep --colour=auto'
#      # alias egrep='egrep --colour=auto'
#      # alias fgrep='fgrep --colour=auto'
#  else
#      if [[ ${EUID} == 0 ]] ; then
#          # show root@ when we don't have colors
#          PS1='\u@\h \W \$\n'
#      else
#          PS1='\u@\h \w \$\n'
#      fi
#  fi

unset use_color safe_term match_lhs sh

#----------------------------MANPAGES

# Pretty-print man(1) pages. See Termcap / Terminfo

# Start blinking
# export LESS_TERMCAP_mb=$'\E[1;92m'
export LESS_TERMCAP_mb=$(tput bold; tput setaf 2) # green

# Start bold
# export LESS_TERMCAP_md=$'\E[1;93m'
export LESS_TERMCAP_md=$(tput bold; tput setaf 2) # green

# Start stand out
#export LESS_TERMCAP_so=$'\E[1;93m'
export LESS_TERMCAP_so=$(tput bold; tput setaf 3) # yellow

# End stand out
# export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_se=$(tput rmso; tput sgr0)

# Start Underline
# export LESS_TERMCAP_us=$'\E[1;92m'
export LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 1) # red

# End Underline
# export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_ue=$(tput sgr0)

# End bold, blinking, standout, underline
# export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_me=$(tput sgr0)

#----------------------------EXTERNAL FILES

# Load Bash Functions
BSHFuncs="$HOME/.dotfiles/D01_Bash/.bash_functions"
[[ -f $BSHFuncs && -r $BSHFuncs ]] && . "$BSHFuncs"

# Load Bash Completion
UsrBashComp='/usr/share/bash-completion/bash_completion'
[[ -f $UsrBashComp && -r $UsrBashComp ]] && . "$UsrBashComp"

# Make Bash show the available options first
bind 'set show-all-if-ambiguous on'
# Set up tab to cycle completion options and show options first
bind 'TAB:menu-complete'

# Load Profile
SHProf="$HOME/.dotfiles/D00_GeneralShell/.profile"
[[ -f $SHProf && -r $SHProf ]] && . "$SHProf"

unset SHProf BSHFuncs UsrBashComp

#----------------------------EXTERNAL PROGRAMS AND SCRIPTS

# RANDOM COLOR SCRIPT
# requires shell color scripts to be installed:
# https://gitlab.com/dwt1/shell-color-scripts/-/tree/master
# colorscript random           # disabled for now, slows down loading
