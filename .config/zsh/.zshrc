setopt autocd extendedglob nomatch menucomplete interactive_comments
setopt appendhistory
unsetopt BEEP
stty stop undef # Disable ctrl-s to freeze terminal
zle_highlight=('paste:none')

autoload -Uz compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zmodload zsh/complist
compinit
_comp_options+=(globdots)

autoload -Uz colors && colors

colorscript random

bindkey "^p" up-line-or-beginning-search # Up
bindkey "^n" down-line-or-beginning-search # Down
bindkey "^k" up-line-or-beginning-search # Up
bindkey "^j" down-line-or-beginning-search # Down

source "$ZDOTDIR/zsh-functions"

zsh_add_file "zsh-aliases"
zsh_add_file "zsh-exports"
zsh_add_file "zsh-prompt"
zsh_add_file "zsh-vi-mode"

zsh_add_plugin "zsh-users/zsh-autosuggestions"
zsh_add_plugin "zsh-users/zsh-syntax-highlighting"
zsh_add_plugin "hlissner/zsh-autopair"
zsh_add_completion "esc/conda-zsh-completion" false

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
