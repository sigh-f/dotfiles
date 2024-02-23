# -*- sh -*-
# Configuration Utilities

#
# OS specifics + package management
#

DEVD="$HOME/dev"

is_os() { [ "$(uname -s | tr '[:upper:]' '[:lower:]')" = "$1" ] ; }
is_macos() { is_os "darwin" ; }
is_linux() { is_os "linux" ; }

is_macos && export BASH_SILENCE_DEPRECATION_WARNING=1

init_homebrew() {
    is_macos && brew_prefix="/opt/homebrew"
    is_linux && brew_prefix="/home/linuxbrew/.linuxbrew"

    if [ -x "$brew_prefix/bin/brew" ]; then
        eval "$("$brew_prefix"/bin/brew shellenv)"
    fi
}
init_homebrew

# ------------------------>
# General Setup (builtins)

shopt -s histappend
export HISTSIZE=50000

export EDITOR="vim" && export VISUAL="vim"
if command -v emacs &>/dev/null; then
    export EDITOR="emacs" && export VISUAL="emacs"
fi

alias e='$EDITOR'
alias ec="emacsclient -nw"
alias g="grep -isE"
alias l="ls -lA --color=always"
alias r='source $HOME/.bashrc'

if command -v fzf &>/dev/null; then

    # Fuzzy edit.
    # TODO pass in initial search
    # $1: Directory to s
    fe() {
        dir=${1:-*}
        # shellcheck disable=2086
        # The default '*' is set to perform glob expansion.
        files=$(find $dir -path '*/.*' -prune -o -print | fzf --multi --select-1 --exit-0) || return $?
        # shellcheck disable=2086
        # Word splitting is required here to pass multiple arguments.
        $EDITOR $files
    }

    fp() {
        d=$(find $DEVD -type d -maxdepth 1 -mindepth 1 | fzf --select-1 --preview 'ls -l {1}')
        cd $d
        # cd $d
    }

    fpe() {
        fp
        $EDITOR .
    }

    # Recursive directory search with support for alternating between narrowing
    # the search space, and filtering results.
    # $1: Initial search pattern
    # ${@:2}: Dir(s) to search. If none provided, use $(pwd)
    fgr() {
        dirs="${@:2}" && [ -z "$2" ] && dirs=$(pwd)
        matches=$(: | fzf --ansi --disabled --query "$1" --delimiter : \
                          --height 100% --multi --select-1 --prompt '1. grep> ' \
                          --bind "start:reload(grep -insR --exclude-dir='.*' {q} $dirs)+unbind(ctrl-s)" \
                          --bind "change:reload:sleep 0.1; grep -insR --exclude-dir='.*' {q} $dirs || true" \
                          --bind "ctrl-r:unbind(change,ctrl-r)+change-prompt(2. fzf> )+enable-search+rebind(ctrl-s)+transform-query(echo {q} > /tmp/fzf-fgr-grep; cat /tmp/fzf-fgr-fzf)"\
                          --bind "ctrl-s:unbind(ctrl-s)+change-prompt(1. grep> )+disable-search+reload(grep -insR --exclude-dir='.*' {q} $dirs || true)+rebind(change,ctrl-r)+transform-query(echo {q} > /tmp/fzf-fgr-fzf; cat /tmp/fzf-fgr-grep)" \
                          --preview 'bat --color=always {1} --theme=gruvbox-dark --highlight-line {2}' \
                          --preview-window 'up,70%,border-bottom,+{2}+3/3,~3')

        [ -z "$matches" ] && return
        while read match ; do
            emacs_args+="+$(echo "$match" | cut -d: -f2) $(echo "$match" | cut -d: -f1) "
        done < <(echo "$matches")
        emacs $emacs_args
    }

fi

# --->
# Fzf

if command -v fzf &>/dev/null; then
    [ -f "$HOME/.fzf.bash" ] && source ~/.fzf.bash # completions
    export FZF_DEFAULT_OPTS="\
        --border \
        --bind ctrl-v:preview-half-page-down,alt-v:preview-half-page-up \
        --bind ctrl-f:page-down,ctrl-b:page-up
        --header-first \
        --height=40% \
        --layout=reverse \
        --marker='> ' \
        --no-hscroll \
        --no-mouse \
        --pointer=' >' \
        --prompt='$ ' \
        --tabstop=4 \
        --color=bg+:#292e42 \
        --color=bg:#24283b \
        --color=border:#414868 \
        --color=fg+:#c6d0f5 \
        --color=fg:#a9b1d6 \
        --color=header:#7892bf,header:bold \
        --color=hl+:#4c9e8a \
        --color=hl:#4c9e8a \
        --color=info:#4c9e8a \
        --color=marker:#e0af68 \
        --color=pointer:#c0caf5 \
        --color=prompt:#7aa2f7 \
        --color=spinner:#f2d5cf \
    "

    export FZF_ALT_C_OPTS="--header='CD'"
    export FZF_CTRL_R_OPTS="--header='BASH HISTORY'"
    export FZF_CTRL_T_OPTS="--header='FILES' --bind 'enter:become($EDITOR {})'"
fi

# --->
# Git

alias ga="git add"
alias gb="git branch"
alias gc="git commit"
alias gcm="git commit -m"
alias gca="git commit --amend"
alias gd="git diff"
alias gdc="git diff --cached"
alias gl="git log"
alias gs="git status"

if command -v fzf &>/dev/null; then
    # Fuzzy git branch (checkout).
    fgb() {
        branch=$(git branch 2>/dev/null | sed -r 's/[* ]+//g' | \
                     fzf --prompt='git checkout ' --header='GIT BRANCHES' --exit-0 \
                         --preview="git log --color=always \
                         --format='%C(auto)%h%d %s %C(black)%C(bold)%cr% C(auto)%an' {}") || return $?
        git checkout "$branch"
    }
fi

# ---->
# Tmux

if command -v tmux &>/dev/null; then
    alias tma="tmux attach -t"
    alias tmk="tmux kill-session -t"
    alias tml="tmux list-sessions"
    alias tmn="tmux new-session -As"
fi

# ------------------------------------------------------------------------->
# Support for persistent on-the-fly modifications to the shell environment.

mkdir -p "$HOME/.config/bash"
# Used to store on-the-fly changes; effectively part of bashrc.
export MUTABLE_CONFIG="$HOME/.config/bash/mutable.sh"
[ -f "$MUTABLE_CONFIG" ] && . "$MUTABLE_CONFIG"

alias mc-clear='> $MUTABLE_CONFIG'

# Create a new alias in the mutable config
# $1: name of the alias
# $2: value of the alias
# $3: optional comment (for tagging)
mca() {
    [ -z "$1" ] && echo "missing the name of the alias" && return
    [ -z "$2" ] && echo "missing the value of the alias" && return
    echo "alias $1=\"$2\" # $3" >> "$MUTABLE_CONFIG"
    . "$MUTABLE_CONFIG"
}

# Create a new alias to cd into $(pwd)
# $1: name of the alias
mcd() {
    [ -z "$1" ] && echo "missing the name of the alias" && return 1
    mca "$1" "cd $(pwd)" "user-tagged-dir"
}

if command -v fzf &>/dev/null; then
    # Fuzzy select a mutable alias
    fmc() {
        alias=$(fzf --header='MUTABLE ALIASES' --select-1 --exit-0 < "$MUTABLE_CONFIG") || return $?
        eval "$(echo "$alias" | sed  "s/\"/'/g" | awk -F"'" '{print $2}')"
    }
fi

# ------>
# Prompt

# Expects $1 to be a color var & $2 to be a font-style var (see below)
# blue_fg="\[\e[0;30m\]"
# r_fg="\[\e[0;34m\]"
# m_fg="\[\e[0;35m\]"
# cyan_fg="\[\e[0;36m\]"
# endcolor="\[\e[0;0;0m\]"
# green_fg="\[\e[0;32m\]"
# red_fg="\[\e[0;31m\]"
# yellow_fg="\[\e[0;33m\]"
# print_style() { printf "\e[%s;3%sm" "${2:-0}" "$1" ; }
print_style() { printf "\[\e[%s;3%sm\]" "${2:-0}" "$1" ; }
# reset_style() { printf "\e[0;0;0m\]" ; }
reset_style() { printf "\[\e[0;0;0m\]" ; }
is_nerd_font() { fc-list : family | grep -iE --quiet ".*nerd-font.*|.*nerd_font.*|.*nerd font.*" ; }

BLACK=0
RED=1
GREEN=2
YELLOW=3
BLUE=4

BOLD=1
FAINT=2
ITALIC=3
UNDERLINE=4

FILE_ICON="!"
GIT_ICON="@"
if is_nerd_font; then
    FILE_ICON=""
    GIT_ICON=""
fi

_prompt_previous_exit_section() {
    prevexit=$?
    [ $prevexit -eq 0 ] && return
    echo "$(print_style $RED $BOLD)[$prevexit]$(reset_style) "
}
_prompt_git_section() {
    ! git branch &>/dev/null && return
    display_color=$GREEN
    if ! git diff --exit-code &>/dev/null; then
        display_color=$YELLOW
        changed_file_display=" ($(git status -s -uno | wc -l | xargs) $FILE_ICON)"
    fi
    branch=$(git branch 2>/dev/null | sed '/^[^*]/d' | sed -r 's/[* ]+//g')
    echo "$(print_style $display_color)$GIT_ICON $branch$changed_file_display$(reset_style) "
}

_prompt() {
    PS1=$(_prompt_previous_exit_section)
    PS1+=$(_prompt_git_section)
    PS1+="$(print_style $BLUE)\w$(print_style $GREEN) $ $(reset_style)"
}
PROMPT_COMMAND=_prompt

# ------------------->
# Local Configuration
# Consider everything in $HOME/.config/bash as additional shell configuration.
# Source these last so that any conflicting local configuration takes precedence.

for local_config in "$HOME/.config/bash/"*; do
    # Don't load the mutable environment twice in case the local config
    # overrides behavior
    [ "$local_config" = "$MUTABLE_CONFIG" ] && continue
    [ -f "$local_config" ] && . "$local_config"
done
