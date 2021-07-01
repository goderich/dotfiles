# Disable greeting message
set fish_greeting

# Set environment variables
set -gx EDITOR /usr/bin/nvim
set -gx BROWSER /usr/bin/qutebrowser
set -gx PAGER /usr/bin/less

# opam configuration
source /home/iwaka/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
