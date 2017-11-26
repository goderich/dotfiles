# disable greeting message

set fish_greeting

# aliases

function ls
    command ls --color=auto $argv
end

function pacman
    command sudo pacman $argv
end

function pm
    command sudo pacman $argv
end

function rm
    command rm -v $argv
end

function mirrorlist
    command sudo reflector --verbose -l 200 -n 20 -p http --sort rate --save /etc/pacman.d/mirrorlist
end

#function !!
#  eval $history[1] $argv
#end function
#    
#function sudo
#  if test $argv
#    if test $argv = "!!"
#      eval /usr/bin/sudo $history[1]
#    else
#      eval /usr/bin/sudo $argv
#    end
#  end
#end
