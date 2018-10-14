#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# make the prompt green
GREEN="\[$(tput setaf 2)\]"
RESET="\[$(tput sgr0)\]"
export PS1="${GREEN}[\u @ \W]:${RESET} "

alias ls='ls --color=auto'
alias grep='grep --colour=auto'
alias mirrorlist='sudo reflector --verbose -l 200 -n 20 -p http --sort rate --save /etc/pacman.d/mirrorlist'
alias pacman='sudo pacman'
alias pm='sudo pacman'
alias rm='rm -v'

# open an instance of emacs that recognizes the compose key and fcitx
# the locale should already be enabled on the system (as per the arch wiki)
# for just the compose key, you can use 'XMODIFIERS=@im=none emacs'
alias emacs-compose='LC_CTYPE=zh_TW.UTF-8 XMODIFIERS=@im=fcitx emacs'

export PATH=~/.local/bin:$HOME/.cargo/bin:$HOME/bin:$PATH
PYTHONPATH="${PYTHONPATH}:/usr/lib/python3.6/site-packages/:/usr/lib/python3.6/site-packages/prompt_toolkit/:~/.local/lib/python3.6/site-packages/:/usr/lib/python3.6/site-packages/IPython/"
export PYTHONPATH

export EDITOR=/usr/bin/nvim
export BROWSER=/usr/bin/qutebrowser
export PAGER=less

# Ignore duplicates
HISTCONTROL=ignoredups
HISTSIZE=10000
HISTIGNORE="ls:cd:cal*:pass*:mirrorlist:nvim:vim:history*"

# Autocompletion for pass
# (not necessary now that I'm using fish)
# source ~/.password-store/.pass.bash-completion

