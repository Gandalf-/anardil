#!/bin/bash

# this is called by cron on a raspberry pi to populate
# images for a timelapse
#
# quality = 10 is good enough for concatenating into an mp4
#   5 has too many artifacts
#  20 doesn't show a noticable difference except when zoomed in

folder=/mnt/zfs/timelapse/"$( date +'%F' )"
mkdir -p "$folder"

output="$folder/$( date +'%s' ).jpg"
raspistill -q 10 -o "$output"
