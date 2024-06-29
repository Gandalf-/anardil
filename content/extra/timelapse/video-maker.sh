#!/bin/bash

# take the directories under ~/working/pi/timelapse and create videos out of
# the jpeg content in each if they don't already exist and the directory isn't
# directory for today
#
# assumes
#
# ~/working/pi/timelapse
#   <date>
#     <image>.jpg
#     ...
#   ...

cd /mnt/zfs/working/pi/timelapse || exit 1

today="$( date +'%F' )"

for directory in *; do

    # don't process today, because we don't have all the images yet
    [[ "$directory" =~ $today ]] && continue
    [[ "$directory" = latest.jpg ]] && continue

    read -ra parts <<< "${directory//-/ }"
    year="${parts[0]}"
    month="${parts[1]}"

    base="/mnt/zfs/Media/Videos/Timelapse/$year-$month"
    output="$base"/"$directory".mp4

    # don't reprocess videos
    [[ -e "$output" ]] && continue

    # don't try empty days
    [[ "$( ls -A "$directory" )" ]] || {
        echo "$directory is empty"
        continue
    }

    mkdir -p "$base"
    ffmpeg -y -framerate 40 \
        -pattern_type glob -i "$directory/*-day.jpg" \
        "$output" || exit 0
done
