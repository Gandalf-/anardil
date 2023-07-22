#!/bin/sh

grep 'Tags: ' content/blog/*.md \
  | sed -e 's/.*Tags: //' -e 's/, /,/g' \
  | tr , '\n' \
  | tr -cd '[:print:]\n' \
  | sort \
  | uniq -c \
  | sort -snr
