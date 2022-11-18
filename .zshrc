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
  kubectl
  nvm
  gentoo-zsh-completions
  zsh-syntax-highlighting
  aws
)

source $ZSH/oh-my-zsh.sh

export PATH="$PATH:$HOME/.bin:${HOME}/.krew/bin:${HOME}/.local/bin"

alias g="git"
alias d=docker
alias tf=terraform
alias s='systemctl'
alias j='journalctl -xe'

if [[ $SWAYSOCK ]]
then alias em="emacsclient -c -s $HOME/.emacs.d/server/server"
else alias em="emacsclient -t -s $HOME/.emacs.d/server/server"
fi



export NVM_DIR="$HOME/.nvm"
export VISUAL=vim
export EDITOR="$VISUAL"
export TERM=xterm-256color


[ -f $HOME/src/vars/.zsh ] && source $HOME/src/vars/.zsh
[ -f $HOME/.fzf.zsh ] && source $HOME/.fzf.zsh


choose_project() {
  prefix="$HOME/src"
  query="$(
    find $prefix -maxdepth 4 -type d -name '.git' \
    | sed -n '/node_module/d; s/\/.git.*$//; s|'"$prefix"'/||p' \
    | sort -u \
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
  branch=$(
      git branch --all --format='%(refname:short)' \
          | sed 's/^origin\///' \
          | uniq -u \
          | fzy -q ""$1"" -l 20
      )

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
  file="$(
    {
      git ls-files 2>/dev/null \
      || find . -maxdepth 1 | sed 's_\./__; /^\.$/d' | sort
    } | fzy -q ""$1"" -l 30
  )"

  success=$?

  if [ -d "$file" ]; then
    cd "$file"
  elif [ $success -eq 0 -a ! -z "$file" ]; then
    em --alternate-editor=vim "$file"
  fi

  zle reset-prompt
}

zle -N git_edit_all
bindkey '^[je' git_edit_all

if [[ $TERM =~ ^foot ]]
then 
    # something to support switching shell instances
    precmd() {
        print -Pn "\e]133;A\e\\"
    }

    # relays pwd changes to foot
    function osc7 {
        local LC_ALL=C
        export LC_ALL

        setopt localoptions extendedglob
        input=( ${(s::)PWD} )
        uri=${(j::)input/(#b)([^A-Za-z0-9_.\!~*\'\(\)-\/])/%${(l:2::0:)$(([##16]#match))}}
        print -n "\e]7;file://${HOSTNAME}${uri}\e\\"
    }
    add-zsh-hook -Uz chpwd osc7
fi

# De-Windowize Path 
if [[ $(uname -o) == Msys ]]; then
  export PATH=$(echo ${PATH} | awk -v RS=: -v ORS=: '/c\// {next} {print}' | sed 's/:*$//')
fi

# KREW
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
