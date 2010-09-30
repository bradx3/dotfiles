###
# Autoload zsh modules when they are referenced
###
zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -a zsh/zprof zprof
zmodload -ap zsh/mapfile mapfile


###
# setup options
###
# use share_history instead of setopt APPEND_HISTORY         # appends history to .zsh_history
setopt AUTO_CD                # cd if no matching command
setopt AUTO_PARAM_SLASH       # adds slash at end of tabbed dirs
setopt CHECK_JOBS             # check bg jobs on exit
setopt CORRECT                # corrects spelling
setopt CORRECT_ALL            # corrects spelling
setopt EXTENDED_GLOB          # globs #, ~ and ^
setopt EXTENDED_HISTORY       # saves timestamps on history
setopt GLOB_DOTS              # find dotfiles easier
setopt HASH_CMDS              # save cmd location to skip PATH lookup
setopt HIST_EXPIRE_DUPS_FIRST # expire duped history first
setopt HIST_NO_STORE          # don't save 'history' cmd in history
setopt INC_APPEND_HISTORY     # append history as command are entered
setopt LIST_ROWS_FIRST        # completion options left-to-right, top-to-bottom
setopt LIST_TYPES             # show file types in list
setopt MARK_DIRS              # adds slash to end of completed dirs
setopt NUMERIC_GLOB_SORT      # sort numerically first, before alpha
setopt PROMPT_SUBST           # sub values in prompt (though it seems to work anyway haha)
setopt RM_STAR_WAIT           # pause before confirming rm *
setopt SHARE_HISTORY          # share history between open shells


###
# Setup vars
###
PATH=~/bin:~/.gem/ruby/1.8/bin:/usr/local/mysql/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/sbin/:/opt/local/lib/postgresql83/bin/:$PATH

#PATH=~/projects/readingeggs/vendor/plugins/cucumber/bin:$PATH
export PATH
TZ="Australia/Sydney"

HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
HOSTNAME="`hostname`"
REPORTTIME=120 # print elapsed time when more than 10 seconds

export PAGER='less'
export SHELL="/bin/zsh"
export RUBYLIB="~/projects/scripts/lib"

export EDITOR="emacs -q"
export GIT_EDITOR="emacs -q"

if [[ `uname` == "Darwin" ]] then
#[[ -f "/usr/bin/mate_wait" ]] then
  export CLICOLOR=1
  export LSCOLORS=gxfxcxdxbxegedabagacad 
else
  alias ls='ls --color'
  export LS_COLORS="di=36;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=:ow=:"
fi

###
# Emacs shortcut keys
###
bindkey -e


###
# ssh host completion
###
zstyle -e ':completion:*:(ssh|scp):*' hosts 'reply=(
  ${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) \
       /dev/null)"}%%[# ]*}//,/ }
  ${=${(f)"$(cat /etc/hosts(|)(N) <<(ypcat hosts 2>/dev/null))"}%%\#*}
)'


###
# Aliases
### 

# general helpers
alias l="ls -laFhG"
alias m="mate ."
alias mw="mate -w"
alias myip="ifconfig | grep 192.168 || ifconfig | grep 10.32"
alias psg="ps ax | grep -i "
alias wget="wget -c"
alias top="top -o cpu"
alias mtop="top -o rsize"
alias sr="screen -r"
alias all_rw="sudo find . -type d -exec sudo chmod 0777 {} \; && sudo find . -type f -exec sudo chmod 0666 {} \;"
alias port="nice port"
alias less="less -r"

#db helpers
alias mystart="sudo /opt/local/share/mysql5/mysql/mysql.server start"
alias mystop="sudo /opt/local/share/mysql5/mysql/mysql.server stop"
alias pgstart="sudo /opt/local/etc/LaunchDaemons/org.macports.postgresql83-server/postgresql83-server.wrapper start"
alias pgstop="sudo /opt/local/etc/LaunchDaemons/org.macports.postgresql83-server/postgresql83-server.wrapper stop"

# rails helpers 
alias ss="thin start"
alias ssd="thin start"
alias mdmu="rake db:migrate VERSION=0; rake db:migrate; rake db:test:clone"
alias mb="rake db:migrate && rake db:test:clone"
alias test_timer="rake TIMER=true 2>/dev/null | grep \" - \" | sort -r | head -n 20"
alias s="bundle exec spec -f p"
alias c="bundle exec cucumber -f Cucumber::Formatter::ImmediateFeedback"
alias cr="bundle exec cucumber --format rerun --out rerun.txt"
alias pc="bundle exec cucumber -f pretty"
alias sc="bundle exec cucumber -p selenium"
#alias tc="rm coverage.data; rcov --aggregate coverage.data --rails --exclude osx\/objc,gems\/,spec\/,features\/,lib\/tasks\/,lib\/unfuddle\/,.gem\/ -o coverage /Users/bwilson/.gem/ruby/1.8/bin/cucumber -- "
alias tc="bundle exec rcov --rails --exclude osx\/objc,gems\/,spec\/,features\/,lib\/tasks\/,lib\/unfuddle\/ -o ~/tmp/rcov /Users/bwilson/.gem/ruby/1.8/bin/cucumber -- "
alias rt="ctags -e **/*.rb"
alias rg="rake routes | grep -i"
alias rake="rake --trace"

# svn helpers
alias sst="svn st"
alias sup="svn up"
alias sci="svn ci -m "
alias si="svn propedit svn:ignore"
alias remove_missing='svn rm `svn st | grep \! | tr -d "! "`'
alias add_missing='svn add `svn st | grep \? | tr -d "? "`'
alias srp="svn propset svn:ignore '*.log' log/ && svn propset svn:ignore '*.db' db/ && svn propset svn:ignore 'schema.rb' db/"

# git helpers
alias gst='git status'
alias gl='git pull origin $(parse_git_branch)'
alias glr='git pull --rebase origin $(parse_git_branch)'
alias gp='git push origin $(parse_git_branch) && gf'
alias gf='git fetch'
alias gd='git diff | mate'
alias gc='git commit -v'
alias gca='git commit -v -a'
alias gb='git branch'
alias gba='git branch -a'
alias gpcd='git push && cap deploy'
alias gr="git reset --hard HEAD"
alias grn="git log --format=oneline  --abbrev-commit --no-merges"

#heroku helpers
alias hp="git push heroku master"
alias hl="heroku logs"

###
# get the name of the branch we are on
###
parse_git_branch() {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
}
function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}

###
# Called before prompt shown
###
function precmd {
  PS1="[$PR_MAGENTA%n$PR_NO_COLOR@$PR_GREEN%U%m%u$PR_NO_COLOR:$PR_CYAN%2c $PR_RED($(parse_git_branch))$PR_NO_COLOR]%(!.#.$) "
}

RPS1="\$(rvm-prompt)$PR_MAGENTA(%D{%I:%M %p %d-%m-%y})$PR_NO_COLOR"


# some functions
function pdfman () {
    man -t $1 | open -a /Applications/Preview.app -f
}

case $TERM in
    *xterm*|ansi)
		function settab { print -Pn "\e]1;%n@%m: %~\a" }
		function settitle { print -Pn "\e]2;%n@%m: %~\a" }
		function chpwd { settab;settitle }
		settab;settitle
        ;;
esac

# if we're sshing in from emacs/tramp
if [ "$TERM" = "dumb" ]                                                                                            
then                                                                                                               
  unsetopt zle                                                                                                     
  unsetopt prompt_cr                                                                                               
  unsetopt prompt_subst                                                                                            
  #unfunction precmd                                                                                                
  #unfunction preexec                                                                                               
  PS1='$ '                                                                                                         
else
    autoload colors zsh/terminfo
    if [[ "$terminfo[colors]" -ge 8 ]]; then
	colors
    fi
    for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
	eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
	eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
	(( count = $count + 1 ))
    done
    PR_NO_COLOR="%{$terminfo[sgr0]%}"
fi               


if [ -s ~/.profile ] ; then
    source ~/.profile
fi

# # RVM
if [ -s ~/.rvm/scripts/rvm ] ; then 
    source ~/.rvm/scripts/rvm ;
#    rvm system;
fi


###
# Bunch of stuff I haven't figured out if I need yet
###
autoload -U compinit
compinit
bindkey '^r' history-incremental-search-backward
bindkey "^[[5~" up-line-or-history
bindkey "^[[6~" down-line-or-history
bindkey "^[[H" beginning-of-line
bindkey "^[[1~" beginning-of-line
bindkey "^[[F"  end-of-line
bindkey "^[[4~" end-of-line
bindkey ' ' magic-space    # also do history expansion on space
bindkey '^I' complete-word # complete on tab, leave expansion to _expand

zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/$HOST

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' menu select=1 _complete _ignored _approximate
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*:processes' command 'ps -axw'
zstyle ':completion:*:processes-names' command 'ps -awxho command'
# Completion Styles
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
# list of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate _force_rehash

# allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'
    
# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions
#
#NEW completion:
# 1. All /etc/hosts hostnames are in autocomplete
# 2. If you have a comment in /etc/hosts like #%foobar.domain,
#    then foobar.domain will show up in autocomplete!
zstyle ':completion:*' hosts $(awk '/^[^#]/ {print $2 $3" "$4" "$5}' /etc/hosts | grep -v ip6- && grep "^#%" /etc/hosts | awk -F% '{print $2}') 
# formatting and messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# command for process lists, the local web server details and host completion
#zstyle ':completion:*:processes' command 'ps -o pid,s,nice,stime,args'
#zstyle ':completion:*:urls' local 'www' '/var/www/htdocs' 'public_html'
zstyle '*' hosts $hosts

# Filename suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.o' '*?.c~' \
    '*?.old' '*?.pro'
# the same for old style completion
#fignore=(.o .c~ .old .pro)

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:scp:*' tag-order \
   files users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:scp:*' group-order \
   files all-files users hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order \
   users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:ssh:*' group-order \
   hosts-domain hosts-host users hosts-ipaddr
zstyle '*' single-ignored show


