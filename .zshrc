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
# zstyle :compinstall filename '/home/jason/.zshrc'

# autoload -Uz compinit
# compinit
# # End of lines added by compinstall

ZSH_THEME="robbyrussell"
HYPHEN_INSENSITIVE="true"
COMPLETION_WAITING_DOTS="true"
# ENABLE_CORRECTION="true"

plugins=(
  git
  ssh-agent
  docker
  dotnet
  taskwarrior
  zsh-syntax-highlighting
  kubectl
  nvm
)

source $ZSH/oh-my-zsh.sh


export PATH="$HOME/src/scripts/bin:$PATH"

alias em="emacsclient -t -s $HOME/.emacs.d/server/server"
alias t="task"
alias g="git"
alias v="vars"
alias vr="vars run"
alias vg="vars get"


choose_project() {
  prefix="$HOME/src"
  query="$(
    find $prefix -maxdepth 4 -type d -name '.git' \
    | sed -n '/node_module/d; s/\/.git.*$//; s|'"$prefix"'/||p' \
    | uniq \
    | fzy -q ""$1"" -l 30)"

  # query="$(ls ""$prefix"" | fzy -q ""$1"" -l 20)"

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
  file=$(grep -Rl '<<<<' $(git ls-files -m | uniq) | fzy -q ""$1"" -l 20)

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

zle -N git_edit_all
bindkey '^[je' git_edit_all


vars_get() {
  target="$(vars list | sed -n '/^T/p' | cut -d, -f2 | fzy -q ""$1"" -l 20)"

  if [ $? -eq 0 -a ! -z "$query" ]; then
    vars get $target
  fi

  zle reset-prompt
}

zle -N vars_get
bindkey '^[jg' vars_get

vars_run() {
  block="$(vars list | sed -n '/^B/p' | cut -d, -f2 | fzy -q ""$1"" -l 20)"

  if [ $? -eq 0 -a ! -z "$query" ]; then
    vars run $block
  fi

  zle reset-prompt
}

zle -N vars_run
bindkey '^[jr' vars_run

# NVM
# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"


# De-Windowize Path 
if [[ $(uname -o) == Msys ]]; then
  export PATH=$(echo ${PATH} | awk -v RS=: -v ORS=: '/c\// {next} {print}' | sed 's/:*$//')
fi

# KREW
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"


# added by travis gem
[ ! -s /home/jason/.travis/travis.sh ] || source /home/jason/.travis/travis.sh
