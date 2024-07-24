#!/usr/bin/env bash

if [[ $# -eq 1 ]]; then
    selected=$1
else
    selected=`cat ~/.local/scripts/tmux-cht-languages ~/.local/scripts/tmux-cht-command | fzf`
fi

if [[ -z $selected ]]; then
    exit 0
fi

read -p "Enter Query: " query

if grep -qs "$selected" ~/.local/scripts/tmux-cht-languages; then
    query=`echo $query | tr ' ' '+'`
    tmux neww bash -c "echo \"curl cht.sh/$selected/$query/\" & curl cht.sh/$selected/$query & while [ : ]; do sleep 1; done"
else
    tmux neww bash -c "echo \"curl cht.sh/$selected~$query/\" & curl cht.sh/$selected~$query & while [ : ]; do sleep 1; done"
fi
