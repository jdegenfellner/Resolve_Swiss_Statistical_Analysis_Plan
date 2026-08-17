#!/bin/sh
# Opens paper.pdf in Preview at a given page, fitted to the window.
#
#   ./show_page.sh 13                 page number
#   ./show_page.sh "Kenward"          first page containing that text
#
# The document is closed first, because Preview restores the previous
# scroll position when it reopens a file that is already open, which
# overrides the jump. Menu items are clicked by name rather than by
# keyboard shortcut, which turned out to be the unreliable part.
set -e
cd "$(dirname "$0")"
PDF="$(pwd)/paper.pdf"

PAGE="$1"
case "$PAGE" in
  ''|*[!0-9]*)
    PAGE=$(pdftotext "$PDF" - 2>/dev/null |
           awk -v pat="$1" 'BEGIN{RS="\f"} index($0,pat){print NR; exit}')
    ;;
esac
[ -z "$PAGE" ] && PAGE=1

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
tell application "System Events" to tell process "Preview"
  click menu item "Continuous Scroll" of menu 1 of menu bar item "View" of menu bar 1
  delay 0.3
  click menu item "Zoom to Fit" of menu 1 of menu bar item "View" of menu bar 1
  delay 0.4
  click menu item "Go to Page…" of menu 1 of menu bar item "Go" of menu bar 1
  delay 0.6
  keystroke "$PAGE"
  delay 0.3
  keystroke return
end tell
EOF
echo "paper.pdf, Seite $PAGE"
