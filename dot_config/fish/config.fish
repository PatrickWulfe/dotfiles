function fish_greeting
    pfetch
end

starship init fish | source

fish_vi_key_bindings

# set -x DOCKER_HOST unix:///run/user/1000/docker.sock
set -x CHROME_EXECUTABLE google-chrome-stable
# set -Ux JAVA_OPTS '' # '-XX:+IgnoreUnrecognizedVMOptions --add-modules java.se.ee'
# set -Ux JAVA_HOME /usr/lib/jv
set -Ux JAVA_HOME $(readlink -f /usr/bin/javac | sed "s:/bin/javac::")
set -Ux ANDROID_SDK_ROOT /home/patrickw/dev/sdks/android
set -Ux NODE_REPL_MODE strict
# set -Ux JUCE_HOME /home/patrick/dev/src/juce/JUCE
set EDITOR emacs

alias vi nvim
alias vim nvim
alias efshc "$EDITOR $HOME/.config/fish/README.org"

alias envmc "$EDITOR $HOME/.config/nvim/init.vim"

alias cdt "cd /tmp"
alias cdc "cd $HOME/.config"
alias cdd "cd $HOME/dev/"
alias cds "cd $HOME/dev/src"
alias cdfi "cd $IMPORT_LOC"

alias pub "dart pub"

# alias dfg "/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME"

# alias uwu "sudo apt update && sudo apt upgrade"
