#!/usr/bin/bash
#----------------------------------------------------------------------------------------------------------------------
#  ____       _               
# / ___|  ___| |_ _   _ _ __  
# \___ \ / _ \ __| | | | '_ \ 
#  ___) |  __/ |_| |_| | |_) |
# |____/ \___|\__|\__,_| .__/ 
#                      |_|    
# 
# - Setup Shell Script -
# Script Name:    - setup
# Source:         - .dotfiles/00_OrgFiles/00-Setup.org
# Target:         - .dotfiles/bin/setup_dotfiles.sh
# Author Email:   - randomly.ventilates@simplelogin.co
# Author GitHub:  - https://github.com/RastiGiG/
# Created:        - 2024-01-15
# Last Changed:   - 2024-01-15
# Dependencies:   - bash, ln, stow
#----------------------------------------------------------------------------------------------------------------------    

      # Handle Exit Codes
      set -eo pipefail

#######################################################
#                Helper Variables                     #
#######################################################

# Programs
link="ln -s "

STOW="stow "
STOWIGNORE="--ignore='Archive' --ignore='Backup' --ignore='*bak'"
STOWFLAGS="${STOWIGNORE} -t ${HOME}/ "

DIRPREFIX="${HOME}/.dotfiles/"

# Directories linking into $HOME
declare -a directory_list_home=(
    "D00_GeneralShell"
    "D01_Bash"
    "D02_Zsh"
    "D04_Keymapping"
    "D05_Emacs"
    "D06_Git"
)

# Directories linking into .config
declare -a directory_list_config=(
      "C02_VimConfiguration"
      "D09_Zathura"
      "D10_WindowManagers"
      "D11_RunLaunchers"
      "D12_DisplaySettings"
      "D13_NotificationDaemon"
      "D14_Terminals"
      "D15_StatusBars"
      "D17_GUIApps"
      "D18_PackageManagers"
)

#######################################################
#                Main Function                        #
#######################################################

main() {
      # Add remove flag if option 'clean' is specified
      RMFLAG=""
      if [[ -n "${1}" ]]; then
  	      if [[ "${1}" = "clean" ]]; then
  		      RMFLAG="-D"
  	      else
  		      echo "You have given a positional argument."
  		      echo "This is only necessary, if you want to remove the config files in this directory."
  		      echo "The usage is:"
  		      echo "> 'setup_dotfiles.sh' to install the configurations or"
  		echo "> 'setup_dotfiles.sh clean' to remove them."
  	      fi
      fi

      # Stow ignores Git files, especially those beginning with .gitconfig
      if [[ ! -f "${HOME}/.gitconfig" ]]; then
  	      echo ".gitconfig found missing. Linked from .dotfiles directory"
  	      ln -s ${HOME}/.dotfiles/D06_Git/.gitconfig ${HOME}/
      fi

      # Stow ignores Git files, especially those beginning with .gitconfig
      if [[ ! -f "${HOME}/.ssh/config" ]]; then
  	      echo ".ssh/config found missing. Copied base version over"
  	      mkdir -p ${HOME}/.ssh
  	      cp  ${HOME}/.dotfiles/D03_SSH/.ssh/config ${HOME}/.ssh/config
      fi

      # Add setup script to PATH
      DIR="${DIRPREFIX}bin/"
      TARGET="${HOME}/.local/bin/"
      CMDOPTS="${STOWIGNORE} -v ${RMFLAG} -d ${DIR} -t ${TARGET}"
      echo "Running: stow ${CMDOPTS} ."
      stow "${STOWIGNORE}" -v "${RMFLAG}" -d "${DIR}" -t "${TARGET}" .

      # Link files to $HOME
    for directory in "${directory_list_home[@]}"; do
  	      DIR="${DIRPREFIX}${directory}/"
  	      TARGET="${HOME}/"
  	      CMDOPTS="${STOWIGNORE} -v ${RMFLAG} -d ${DIR} -t ${TARGET}"
  	      echo "Running: stow ${CMDOPTS} ."
  	      stow "${STOWIGNORE}" --ignore='.config' -v "${RMFLAG}" -d "${DIR}" -t "${TARGET}" .
      done

      # Link files to .config
    for directory in "${directory_list_config[@]}"; do
  	      DIR="${DIRPREFIX}${directory}/.config/"
  	      TARGET="${HOME}/.config/"
  	      CMDOPTS="${STOWIGNORE} -v ${RMFLAG} -d ${DIR} -t ${TARGET}"
  	      echo "Running: stow ${CMDOPTS} ."
  	      stow "${STOWIGNORE}"  -v "${RMFLAG}" -d "${DIR}" -t "${TARGET}" .
      done
}

[[ "${BASH_SOURCE[0]}" == "${0}" ]] && main "$@"
