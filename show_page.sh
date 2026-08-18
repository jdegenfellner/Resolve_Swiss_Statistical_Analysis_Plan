#!/bin/sh
# Opens paper.pdf in Preview at a given page, on the left of the screen,
# zoomed so the text is comfortable to read.
#
#   ./show_page.sh 13                 page number
#   ./show_page.sh "Kenward"          first page containing that text
#   FORCE=1 ./show_page.sh 13         full pass even if the page is unchanged
#
# The document is closed first, because Preview restores the previous
# scroll position when it reopens a file that is already open, which
# overrides the jump. Menu items are clicked by name rather than by
# keyboard shortcut, which turned out to be the unreliable part, and the
# page jump comes last, because changing view mode or zoom scrolls back
# to the top.
#
# Preview restores the last-viewed spot per file, so when the requested
# page is the same as last time, the window dance and the page jump are
# redundant. The script remembers the last target in .show_page_last and
# then only closes and reopens the document.
set -e
cd "$(dirname "$0")"
PDF="$(pwd)/paper.pdf"
STATE=".show_page_last"

PAGE="$1"
case "$PAGE" in
  ''|*[!0-9]*)
    PAGE=$(pdftotext "$PDF" - 2>/dev/null |
           awk -v pat="$1" 'BEGIN{RS="\f"} index($0,pat){print NR; exit}')
    ;;
esac
[ -z "$PAGE" ] && PAGE=1

LAST=""
[ -f "$STATE" ] && LAST=$(cat "$STATE")

if [ "$PAGE" = "$LAST" ] && [ -z "$FORCE" ]; then
  # same target as last time: reopen only, Preview restores the spot
  osascript <<EOF >/dev/null
tell application "Preview"
  try
    close (every document whose name is "paper.pdf") saving no
  end try
end tell
delay 0.4
tell application "Preview"
  open POSIX file "$PDF"
  activate
end tell
EOF
  echo "paper.pdf, Seite $PAGE (Position wiederhergestellt)"
  exit 0
fi

# window width as a fraction of the screen, and extra zoom steps beyond
# "Zoom to Fit" (0 keeps the page at window width)
WIDTH_FRACTION=${WIDTH_FRACTION:-0.55}
ZOOM_STEPS=${ZOOM_STEPS:-0}

osascript <<EOF >/dev/null
tell application "Preview"
  try
    close (every document whose name is "paper.pdf") saving no
  end try
end tell
delay 0.5
tell application "Preview"
  open POSIX file "$PDF"
  activate
end tell
delay 1.5

tell application "Finder" to set screenSize to bounds of window of desktop
set screenW to item 3 of screenSize
set screenH to item 4 of screenSize
set winW to (screenW * $WIDTH_FRACTION) as integer

tell application "System Events" to tell process "Preview"
  set position of window 1 to {0, 25}
  set size of window 1 to {winW, screenH - 25}
  delay 0.5
  click menu item "Continuous Scroll" of menu 1 of menu bar item "View" of menu bar 1
  delay 0.3
  click menu item "Zoom to Fit" of menu 1 of menu bar item "View" of menu bar 1
  delay 0.3
  if $ZOOM_STEPS > 0 then
    repeat $ZOOM_STEPS times
      click menu item "Zoom In" of menu 1 of menu bar item "View" of menu bar 1
      delay 0.2
    end repeat
  end if
  delay 0.3
  click menu item "Go to Page…" of menu 1 of menu bar item "Go" of menu bar 1
  delay 0.6
  keystroke "$PAGE"
  delay 0.3
  keystroke return
end tell
EOF
printf '%s' "$PAGE" > "$STATE"
echo "paper.pdf, Seite $PAGE"
