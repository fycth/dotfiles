
. ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

POWERLEVEL9K_DISABLE_RPROMPT=true

export MANPAGER='less'
export LESS="-ismWXr"
export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'


# fj i [F]ind in [Journal]
# it searches for a term in journal files
# and all this with preview!
function fj() {
  if [ ! "$#" -gt 0 ]; then echo "Need a string to search for!"; return 1; fi
  rg --files-with-matches --no-messages "$1" ~/journal | fzf --preview "highlight -O ansi -l {} 2> /dev/null | rg --colors 'match:bg:yellow' --ignore-case --pretty --context 10 '$1' || rg --ignore-case --pretty --context 10 '$1' {}"
}

alias sed=gsed

alias maven="command mvn"
function color_maven() {
    local BLUE="\x1b[0;34m"
    local RED="\x1b[0;31m"
    local GREEN="\x1b[0;32m"
    local YELLOW="\x1b[1;33m"
    local WHITE="\x1b[1;37m"
    local LIGHT_RED="\x1b[1;31m"
    local LIGHT_GREEN="\x1b[1;32m"
    local LIGHT_BLUE="\x1b[1;34m"
    local LIGHT_CYAN="\x1b[1;36m"
    local NO_COLOUR="\x1b[0m"
    maven $* | sed \
        -e "s/Tests run: \([^,]*\), Failures: \([^,]*\), Errors: \([^,]*\), Skipped: \([^,]*\)/${LIGHT_GREEN}Tests run: \1$NO_COLOUR, Failures: $RED\2$NO_COLOUR, Errors: $YELLOW\3$NO_COLOUR, Skipped: $LIGHT_BLUE\4$NO_COLOUR/g" \
        -e "s/\(\[\{0,1\}WARN\(ING\)\{0,1\}\]\{0,1\}.*\)/$YELLOW\1$NO_COLOUR/g" \
        -e "s/\(\[ERROR\].*\)/$RED\1$NO_COLOUR/g" \
        -e "s/\(\(BUILD \)\{0,1\}FAILURE.*\)/$RED\1$NO_COLOUR/g" \
        -e "s/\(\(BUILD \)\{0,1\}SUCCESS.*\)/$LIGHT_GREEN\1$NO_COLOUR/g" \
        -e "s/\(\[INFO\].*\)/$GREEN\1$NO_COLOUR/g"

        MAVEN_STATUS=$PIPESTATUS

        return $MAVEN_STATUS
}
alias mvn=color_maven
alias octave='octave --no-gui-libs'

alias top='/usr/bin/top -o cpu'
alias tmuxnyx='tmuxinator start nyx-shells'

# Some OS X-only stuff.
if [[ "$OSTYPE" == darwin* ]]; then
  alias ruby=/usr/local/opt/ruby/bin/ruby
  alias vim=/usr/local/opt/neovim/bin/nvim
#  alias vi=vim

  alias git=/usr/local/bin/git

  # Short-cuts for copy-paste.
  alias c='pbcopy'
  alias p='pbpaste'

  # Remove all items safely, to Trash (`brew install trash`).
  alias rm='trash'

  alias pip=pip3
  alias python=python3

  # Case-insensitive pgrep that outputs full path.
  alias pgrep='pgrep -fli'

  # Lock current session and proceed to the login screen.
  alias lock='/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend'

  # Sniff network info.
  alias sniff="sudo ngrep -d 'en1' -t '^(GET|POST) ' 'tcp and port 80'"

  alias emacs="/usr/local/opt/emacs-mac/bin/emacs"
  export EMACS="/usr/local/opt/emacs-mac/bin/emacs"
  export EMACSCLIENT="/usr/local/opt/emacs-mac/bin/emacsclient"
  export PATH="$PATH:${HOME}/dotfiles/bin"

# Go development
   export GOROOT="$/usr/local/opt/go/libexec"
   PATH="$PATH:${GOROOT}/bin"
else
  # Process grep should output full paths to binaries.
  alias pgrep='pgrep -fl'
fi

# Pretty print json
alias json='python -m json.tool'

# Show man page in Preview.app.
# $ manp cd
function manp {
  local page
  if (( $# > 0 )); then
    for page in "$@"; do
      man -t "$page" | open -f -a Preview
    done
  else
    print 'What manual page do you want?' >&2
  fi
}

function kp {
# mnemonic: [K]ill [P]rocess
# show output of "ps -ef", use [tab] to select one or multiple entries
# press [enter] to kill selected processes and go back to the process list.
# or press [escape] to go back to the process list. Press [escape] twice to exit completely.

  local pid=$(ps -ef | sed 1d | eval "fzf ${FZF_DEFAULT_OPTS} -m --header='[kill:process]'" | awk '{print $2}')

  if [ "x$pid" != "x" ]
  then
    echo $pid | xargs kill -${1:-9}
    kp
  fi
}

function fp {
# mnemonic: [F]ind [P]ath
# list directories in $PATH, press [enter] on an entry to list the executables inside.
# press [escape] to go back to directory listing, [escape] twice to exit completely

  local loc=$(echo $PATH | sed -e $'s/:/\\\n/g' | eval "fzf ${FZF_DEFAULT_OPTS} --header='[find:path]'")

  if [[ -d $loc ]]; then
    echo "$(rg --files $loc | rev | cut -d"/" -f1 | rev)" | eval "fzf ${FZF_DEFAULT_OPTS} --header='[find:exe] => ${loc}' >/dev/null"
    fp
  fi
}

# Find files and exec commands at them.
# $ find-exec .coffee cat | wc -l
# # => 9762
function find-exec() {
  find . -type f -iname "*${1:-}*" -exec "${2:-file}" '{}' \;
}

# Count code lines in some directory.
# $ loc py js css
# # => Lines of code for .py: 3781
# # => Lines of code for .js: 3354
# # => Lines of code for .css: 2970
# # => Total lines of code: 10105
function loc() {
  local total
  local firstletter
  local ext
  local lines
  total=0
  for ext in $@; do
    firstletter=$(echo $ext | cut -c1-1)
    if [[ firstletter != "." ]]; then
      ext=".$ext"
    fi
    lines=`find-exec "*$ext" cat | wc -l`
    lines=${lines// /}
    total=$(($total + $lines))
    echo "Lines of code for ${fg[blue]}$ext${reset_color}: ${fg[green]}$lines${reset_color}"
  done
  echo "${fg[blue]}Total${reset_color} lines of code: ${fg[green]}$total${reset_color}"
}

# Show how much RAM application uses.
# $ ram safari
# # => safari uses 154.69 MBs of RAM.
function ram() {
  local sum
  local items
  local app="$1"
  if [ -z "$app" ]; then
    echo "First argument - pattern to grep from processes"
  else
    sum=0
    for i in `ps aux | grep -i "$app" | grep -v "grep" | awk '{print $6}'`; do
      sum=$(($i + $sum))
    done
    sum=$(echo "scale=2; $sum / 1024.0" | bc)
    if [[ $sum != "0" ]]; then
      echo "${fg[blue]}${app}${reset_color} uses ${fg[green]}${sum}${reset_color} MBs of RAM."
    else
      echo "There are no processes with pattern '${fg[blue]}${app}${reset_color}' are running."
    fi
  fi
}

# $ size dir1 file2.js
function size() {
  # du -sh "$@" 2>&1 | grep -v '^du:' | sort -nr
  du -shck "$@" | sort -rn | awk '
      function human(x) {
          s="kMGTEPYZ";
          while (x>=1000 && length(s)>1)
              {x/=1024; s=substr(s,2)}
          return int(x+0.5) substr(s,1,1)
      }
      {gsub(/^[0-9]+/, human($1)); print}'
}

# $ aes-enc file.zip
function aes-enc() {
  openssl enc -aes-256-cbc -e -in $1 -out "$1.aes"
}

# $ aes-dec file.zip.aes
function aes-dec() {
  openssl enc -aes-256-cbc -d -in $1 -out "${1%.*}"
}

# Converts a.mkv to a.m4v.
function mkv2mp4() {
  for file in "$@"; do
    ffmpeg -i $file -map 0 -c copy "${file%.*}.m4v"
  done
}

function mkv2mp4_1() {
  for file in "$@"; do
    ffmpeg -i $file -map 0:0 -map 0:1 -c copy -c:s mov_text "${file%.*}.m4v"
  done
}

function mkv2mp4_2() {
  for file in "$@"; do
    ffmpeg -i $file -map 0:0 -map 0:2 -c copy -c:s mov_text "${file%.*}.m4v"
  done
}

function mkv2mp4_3() {
  for file in "$@"; do
    ffmpeg -i $file -map 0:0 -map 0:3 -c copy -c:s mov_text "${file%.*}.m4v"
  done
}

# Load 8 cores at once.
function maxcpu() {
  dn=/dev/null
  yes > $dn & yes > $dn & yes > $dn & yes > $dn &
  yes > $dn & yes > $dn & yes > $dn & yes > $dn &
}

# show todos on a git repo
function todos() {
  (todo_list) | while IFS= read -r todo; do printf "%s\n" "$(file_path):$(line_number) $(line_author) $(message)"; done
}

function todo_list() {
  grep -InR 'TODO' ./* \
    --exclude-dir=node_modules \
    --exclude-dir=public \
    --exclude-dir=vendor \
    --exclude-dir=compiled \
    --exclude-dir=git-hooks
}

function line_author() {
  LINE=$(line_number "$todo")
  FILE=$(file_path "$todo")
  tput setaf 6
  printf "%s" "$(git log --pretty=format:"%cN" -s -L "$LINE","$LINE":"$FILE" | head -n 1)"
  tput sgr0
}

function file_path() {
  printf "%s" "$todo" | cut -d':' -f 1
}

function line_number() {
  printf "%s" "$todo" | cut -d':' -f 2
}

function message() {
  printf "%s" "$todo" | cut -d':' -f 3 | xargs
}
# end of todos

PATH="$PATH:/Users/as/.local/bin"

gopaths=(
  "${HOME}/.go"
)
for ((i = 1; i <= ${#gopaths[@]}; i++))
do
  ITEM=${gopaths[$i]}
  GOPATH="$GOPATH:${ITEM}"
  PATH="$PATH:${ITEM}/bin"
  test -d "${ITEM}" || mkdir "${ITEM}"
done
export GOPATH=${GOPATH:1}
export PATH

if [ -f /usr/libexec/java_home ]
then
  export JAVA_HOME=$(/usr/libexec/java_home)
fi

export LEDGER_FILE=~/.hledger/main.txt

export ANDROID_SDK_ROOT=/usr/local/share/android-sdk

export FZF_DEFAULT_COMMAND='fd'

source ~/.ghcup/env

