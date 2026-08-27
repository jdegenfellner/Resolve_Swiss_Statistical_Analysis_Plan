#!/bin/sh
# Builds paper_for_annotation.docx so colleagues can comment in Word.
# Run after the PDF build; the flow diagram is rendered from paper.pdf,
# because TikZ does not survive the pandoc conversion.
set -e
cd "$(dirname "$0")"

# locate the PDF page holding the flow diagram and render it as PNG
pages=$(pdfinfo paper.pdf | awk '/^Pages:/ {print $2}')
fig_page=""
for p in $(seq 1 "$pages"); do
  if pdftotext -f "$p" -l "$p" paper.pdf - 2>/dev/null \
      | grep -q "Flow of practices and participants"; then
    fig_page=$p
    break
  fi
done
if [ -n "$fig_page" ]; then
  pdftoppm -png -r 150 -f "$fig_page" -l "$fig_page" -singlefile \
    paper.pdf figures/flow_diagram
fi

# splice the rendered diagram in place of the tikzpicture, then convert
perl -0pe 's/\\begin\{tikzpicture\}.*?\\end\{tikzpicture\}/\\includegraphics[width=\\textwidth]{figures\/flow_diagram.png}/s' \
  paper.tex > .paper_docx.tex
pandoc .paper_docx.tex --citeproc --bibliography=references.bib \
  --number-sections -M link-citations=true \
  -o paper_for_annotation.docx
rm -f .paper_docx.tex
python3 redden_markers.py paper_for_annotation.docx
echo "paper_for_annotation.docx written"
