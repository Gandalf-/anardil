#!/bin/bash

# sort images into day and night by looking at the file size. night time images
# are significantly smaller than day time images (since there's less
# information to encode)
#
# assumes
#
# ~/working/pi/timelapse
#   <date>
#     <image>.jpg
#     ...
#   ...

month-yesterday() {
    # print whatever month it was yesterday

    python3 -c '
import datetime
today = datetime.date.today()
first = today.replace(day=1)
lastMonth = first - datetime.timedelta(days=1)
print(lastMonth.strftime("%Y-%m"))
'
}

thismonth="$( date +'%Y-%m' )"
yestmonth="$( month-yesterday )"

# on the first of the month, we need to go back and run day-night.py one last
# time on the previous month to categorize the last day. this does that. if
# we're not on the first of the month, this'll do nothing
[[ "$yestmonth" != "$thismonth" ]] &&
    python3 ~/google_drive/code/shell/timelapse/day-night.py "$yestmonth"

# categorize this month
python3 ~/google_drive/code/shell/timelapse/day-night.py "$thismonth"
