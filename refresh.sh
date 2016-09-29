#!/bin/bash
#
# Watches the folder or files passed as arguments to the script and when it
# detects a change it automatically refreshes the current selected Chrome tab or
# window.
#
# Usage:
# ./refresh.sh /folder/to/watch /some/folder/file_to_watch.html

TIME_FORMAT='%F %H:%M'
OUTPUT_FORMAT='%T Event(s): %e fired for file: %w. Refreshing.'

while inotifywait -q -r --timefmt "${TIME_FORMAT}" --format "${OUTPUT_FORMAT}" "$@"; do
    CHROME_WINDOW_ID=$(xdotool search --onlyvisible --class chromium | head -1)
    xvkbd -window $CHROME_WINDOW_ID -text "\Cr"
    # xdotool key --window $CHROME_WINDOW_ID 'CTRL+r'
done
