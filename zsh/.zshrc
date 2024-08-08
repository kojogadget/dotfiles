# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
eval "$(starship init zsh)"
eval "$(zoxide init zsh)"

export FZF_DEFAULT_COMMAND="fd --hidden --strip-cwd-prefix --exclude .git"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="fd --type=d --hidden --strip-cwd-prefix --exclude .git"

export FZF_CTRL_T_OPTS="--preview 'bat -n --color=always --line-range :500 {}'"
export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always {} | head -200'"

alias ll='eza -l --color=always --group-directories-first --icons --git --no-user --no-time --no-permissions'
alias la='eza -la --color=always --group-directories-first --icons --git'

alias cd='z'

alias vi='nvim'
alias vim='nvim'

bindkey '^p' history-search-backward
bindkey '^n' history-search-forward
bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
bindkey '^[b' backward-word
bindkey '^[f' forward-word
bindkey '^[[27;5;46~' insert-last-word
bindkey -s '^f' 'tmux-sessionizer\n'

HISTFILE=~/.zsh_history
setopt HIST_SAVE_NO_DUPS appendhistory

autoload -U compinit; compinit
_comp_options+=(globdots)

zmodload zsh/complist
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

fpath+=$HOME/.zsh/zsh_functions

# Colored stats and other settings
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'm:{a-z}={A-Z}'

# Symlinked directories marked
zstyle ':completion:*' mark-directories yes
zstyle ':completion:*' accept-exact '*(N)'

# Show all matches at once
zstyle ':completion:*' menu select=0

# Ignore case for completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

_fzf_compgen_path() {
    fd --hidden --exclude .git . "$1"
}

_fzf_compgen_dir() {
    fd --type=d --hidden --exclude .git . "$1"
}

# Advanced customization of fzf options via _fzf_comprun function
# - The first argument to the function is the name of the command.
# - You should make sure to pass the rest of the arguments to fzf.
_fzf_comprun() {
    local command=$1
    shift

    case "$command" in
        cd)           fzf --preview 'eza --tree --color=always {} | head -200' "$@" ;;
        export|unset) fzf --preview "eval 'echo $'{}"         "$@" ;;
        ssh)          fzf --preview 'dig {}'                   "$@" ;;
        *)            fzf --preview "bat -n --color=always --line-range :500 {}" "$@" ;;
    esac
}


[[ -s "$HOME/Projects/builds/fzf-git.sh/fzf-git.sh" ]] && source "$HOME/Projects/builds/fzf-git.sh/fzf-git.sh"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
