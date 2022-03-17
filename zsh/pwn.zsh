#/usr/bin/env zsh

emulate -L zsh
setopt re_match_pcre

zmodload zsh/net/tcp

die() {
  echo "$@" >&2
  exit 1
}

unreachable() {
  die unreachable
}

chr() {
  printf "\\$(printf %o $1)"
}

to_bytes() {
  local n=$1
  local b=$2
  local endian=$3
  local i

  print -n - "$(
    case $endian in
      big)
        for ((i = b / 8 - 1; i >= 0; i--)) {
          chr $((n >> (i * 8) & 0xFF))
        }
      ;;
    little)
      for ((i = 0; i < b / 8; i++)) {
        chr $((n >> (i * 8) & 0xFF))
      }
      ;;
    *)
      unreachable
      ;;
    esac
  )"
}

ztcp "piecewise.challs.cyberchallenge.it" 9110 || die "Cannot open the connection"
SOCK=$REPLY

while IFS='' read -r -u $SOCK line; do
  print -r - "$line"

  if [[ "$line" =~ '^Please send me the number (\d+) as a (32|64)-bit (big|little)-endian ' ]]; then
    to_bytes "${match[1]}" "${match[2]}" "${match[3]}" >&$SOCK
  elif [[ "$line" =~ '^Please send me an empty line' ]]; then
    echo >&$SOCK
  else
    unreachable
  fi

  IFS='' read -r -u $SOCK line || die "Connection closed unexpectedly"
  echo "$line"
done
