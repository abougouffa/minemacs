#!/bin/bash

# Directory to monitor
MONITOR_DIR="$1"

# Monitor for new files and call the API
inotifywait -r -m -e create --format '%w%f' "$MONITOR_DIR" |
    while read -r NEWFILE; do
	echo "Created: $NEWFILE"
    done
