#!/usr/bin/env fish

function tm
    set -l session_name "goderich"
    if not tmux has-session -t $session_name
        tmux new-session -s $session_name -n 'files' -d 'clifm'
        tmux new-window -n 'volume' 'alsamixer'
        tmux new-window -n 'shell'
        tmux select-window -t 1
    end
    tmux attach -t $session_name
end
