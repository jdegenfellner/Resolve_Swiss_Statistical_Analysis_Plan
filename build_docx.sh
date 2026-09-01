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

# Zwei Dinge, die pandoc nicht kann, hier vorweg erledigen:
# \cmidrule kennt es nicht und kippt den Spaltenbereich als Text in die
# Tabelle, und \ref bleibt als roher Schluessel stehen. Die Nummern stehen
# in paper.aux, also werden sie von dort eingesetzt.
python3 - <<'PYEOF'
import re
aux = open('paper.aux', encoding='utf-8').read()
num = dict(re.findall(r'\\newlabel\{([^}]+)\}\{\{([^{}]*)\}', aux))
s = open('.paper_docx.tex', encoding='utf-8').read()
s = re.sub(r'\\cmidrule(\([^)]*\))?\{[^}]*\}\s*', '', s)
s = re.sub(r'\\eqref\{([^}]+)\}', lambda m: '(%s)' % num.get(m.group(1), '?'), s)
s = re.sub(r'\\ref\{([^}]+)\}',  lambda m: num.get(m.group(1), '?'), s)
open('.paper_docx.tex', 'w', encoding='utf-8').write(s)
print(f"{len(num)} Querverweise aufgeloest, cmidrule entfernt")
PYEOF
pandoc .paper_docx.tex --citeproc --bibliography=references.bib \
  --number-sections -M link-citations=true \
  -o paper_for_annotation.docx
rm -f .paper_docx.tex
python3 redden_markers.py paper_for_annotation.docx
python3 style_tables.py paper_for_annotation.docx
echo "paper_for_annotation.docx written"
