# Path to your oh-my-zsh installation.
ZSH=/usr/share/oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="juanghurtado"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# cache
ZSH_CACHE_DIR=$HOME/.oh-my-zsh-cache
if [[ ! -d $ZSH_CACHE_DIR ]]; then
    mkdir $ZSH_CACHE_DIR
fi

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
#ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git github autojump colored-man history-substring-search)

# User configuration

#------------------------------
# Variables
#------------------------------

export BROWSER="google-chrome-stable"
export EDITOR="/usr/local/bin/emc -nw"
export TERM=xterm-256color

#------------------------------
# Keybindings
#------------------------------

bindkey "^[[3~" delete-char #Del key
bindkey "^[[1;5C" forward-word # control + right arrow
bindkey "^[[1;2C" end-of-line # shift + right arrow
bindkey "^[[1;5D" backward-word # control + left arrow
bindkey "^[[1;2D" beginning-of-line # shift + left arrow
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward
bindkey "^[[3~" delete-char #Del key
bindkey "^[[3;5~" kill-word # control + delete

#------------------------------
# Alias stuff
#------------------------------

alias ls="ls --color=auto"
alias ll="ls --color -lh"
alias emct="/usr/local/bin/emc -nw"
alias yupgrade="yaourt -Syua"
alias fcache="fc-cache -vf"

#------------------------------
# Window title
#------------------------------

export NETKIT_HOME=/opt/netkit

export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH=$NETKIT_HOME/bin:$PATH
export PATH=$HOME/Android/Sdk/platform-tools:$PATH
export PATH=$HOME/Android/Sdk/tools:$PATH

export JAVA_HOME=/usr/lib/jvm/java-7-jdk
export GEOSERVER_HOME=/opt/geoserver-2.7.2
# export MANPATH="/usr/local/man:$MANPATH"

export PGDATA=/var/lib/postgres/data

source $ZSH/oh-my-zsh.sh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh


#autoload -U colors zsh/terminfo
#colors

#PROMPT='%B%F{yellow} > %(?..%F{yellow}[%F{red}%?%F{yellow}] )%b%f%k'
#RPROMPT='${vcs_info_msg_0_} %B%F{yellow}%~%0'

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
