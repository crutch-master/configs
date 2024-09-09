#!/usr/bin/env bash
message=$1

line () {
  echo -n '+'
  for ((i=0; i < ${#message} + 2; i++)); do echo -n '-'; done
  echo '+'
}

line
echo "| $message |"
line
