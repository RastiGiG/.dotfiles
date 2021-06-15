#  _________  _   _ _____ _   ___     __
# |__  / ___|| | | | ____| \ | \ \   / /
#   / /\___ \| |_| |  _| |  \| |\ \ / / 
#  / /_ ___) |  _  | |___| |\  | \ V /  
# /____|____/|_| |_|_____|_| \_|  \_/   
#                                       
# 

# Default Apps
export EDITOR="nvim"
export CODEEDITOR="emacs"
export READER="zathura"
export VISUAL="nvim"
# export TERMINAL="alacritty"
export BROWSER="brave"
# export VIDEO="mpv"
# export IMAGE="sxiv"
export COLORTERM="truecolor"
# export OPENER="xdg-open"
export PAGER="less"
# export WM="bspwm"

# Start blinking
export LESS_TERMCAP_mb=$(tput bold; tput setaf 2) # green
# Start bold
export LESS_TERMCAP_md=$(tput bold; tput setaf 2) # green
# Start stand out
export LESS_TERMCAP_so=$(tput bold; tput setaf 3) # yellow
# End standout
export LESS_TERMCAP_se=$(tput rmso; tput sgr0)
# Start underline
export LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 1) # red
# End Underline
export LESS_TERMCAP_ue=$(tput sgr0)
# End bold, blinking, standout, underline
export LESS_TERMCAP_me=$(tput sgr0)
