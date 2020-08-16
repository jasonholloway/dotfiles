source $HOME/.zprofile

# Lines configured by zsh-newuser-install
HISTFILE=$HOME/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install

export ZSH=$HOME/.oh-my-zsh

# The following lines were added by compinstall

# zstyle ':completion:*' completer _complete _ignored
# zstyle :compinstall filename '/home/jason/.zshrc'

# End of lines added by compinstall
ZSH_THEME="robbyrussell"
HYPHEN_INSENSITIVE="true"
COMPLETION_WAITING_DOTS="true"
# ENABLE_CORRECTION="true"

plugins=(git ssh-agent docker dotnet)

source $ZSH/oh-my-zsh.sh

alias em="emacsclient -t -s $HOME/.emacs.d/server/server"

choose_project() {
  prefix="$HOME/src"
  query="$(ls ""$prefix"" | fzy -q ""$1"" -l 20)"

  if [ $? -eq 0 -a ! -z "$query" ]; then
    pushd "${prefix}/${query}"
    clear
  fi

  zle reset-prompt
}

zle -N choose_project
bindkey '^[jp' choose_project


git_choose_branch() {
  buffer="$BUFFER"
  branch="$(git branch --all --format='%(refname:short)' | fzy -q ""$1"" -l 20)"

  if [ $? -eq 0 -a ! -z "$branch" ]; then
    if [ ! -z "$buffer" ]; then
      zle reset-prompt
      zle -U "$branch"
      zle reset-prompt
    else
      zle reset-prompt
      git checkout "$branch"
      zle reset-prompt
    fi
  fi
}

zle -N git_choose_branch
bindkey '^[jb' git_choose_branch


git_edit_dirty() {
  file="$(git ls-files -m | uniq | fzy -q ""$1"" -l 20)"

  if [ $? -eq 0 -a ! -z "$file" ]; then
    em --alternate-editor=vim "$file"
  fi

  zle reset-prompt
}

zle -N git_edit_dirty
bindkey '^[jd' git_edit_dirty


git_edit_all() {
  file="$(git ls-files | fzy -q ""$1"" -l 20)"

  if [ $? -eq 0 -a ! -z "$file" ]; then
    em --alternate-editor=vim "$file"
  fi

  zle reset-prompt
}
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

zle -N git_edit_all
bindkey '^[je' git_edit_all
