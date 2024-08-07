#----------------------------------------------------------------------------------------------------------------------
#  ____  _   _ _____ _     _       ____  ____   ___  _____ ___ _     _____
# / ___|| | | | ____| |   | |     |  _ \|  _ \ / _ \|  ___|_ _| |   | ____|
# \___ \| |_| |  _| | |   | |     | |_) | |_) | | | | |_   | || |   |  _|
#  ___) |  _  | |___| |___| |___  |  __/|  _ <| |_| |  _|  | || |___| |___
# |____/|_| |_|_____|_____|_____| |_|   |_| \_\\___/|_|   |___|_____|_____|
#
# - Common Profile settings for login shells
# Source:         - .dotfiles/00_OrgFiles/ShellProfile.org
# Target:         - .dotfiles/D00_GeneralShell/.profile
# Author Email:   - randomly.ventilates@simplelogin.co
# Author GitHub:  - https://github.com/RastiGiG/
#
#
#----------------------------------------------------------------------------------------------------------------------

#----------------------------EXTERNAL FILES

# Load Environmental Variables
SHEnvVar="$HOME/.dotfiles/D00_GeneralShell/.envvar"
[[ -f $SHEnvVar && -r $SHEnvVar ]] && . "$SHEnvVar"

# Load Shell Functions
SHFuncs="$HOME/.dotfiles/D00_GeneralShell/shell_functions.sh"
[[ -f $SHFuncs && -r $SHFuncs ]] && . "$SHFuncs"

# Load Shell Functions
PrSHFuncs="$HOME/.dotfiles/D00_GeneralShell/shell_functions.sh"
[[ -f $PrSHFuncs && -r $PrSHFuncs ]] && . "$SHFuncs"

# Load Shell Aliases
SHAlias="$HOME/.dotfiles/D00_GeneralShell/aliases.sh"
[[ -f $SHAlias && -r $SHAlias ]] && . "$SHAlias"

# Load Private Shell Aliases
PrSHAlias="$HOME/.dotfiles-private/Configurations/C1-Shell/private_aliases.sh"
[[ -f $PrSHAlias && -r $PrSHAlias ]] && . "$PrSHAlias"

unset SHEnvVar SHFuncs PrSHFuncs SHAlias PrSHAlias
