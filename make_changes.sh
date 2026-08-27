#!/bin/sh
# Baut die Aenderungsfassung gegen die Version, die Fabian und Andrea
# kommentiert haben (Commit 7fffc69, 19.8.2026).
#
#   paper_changes.pdf   alle Aenderungen in gruener Schrift
#   paper_changes.docx  dieselben Aenderungen gruen hervorgehoben,
#                       dazu Word-Kommentare, die jede Rueckmeldung beantworten
#
# Geloeschter Text wird nicht angezeigt, die Fassung ist zum Lesen gedacht.
set -e
cd "$(dirname "$0")"

BASE=${1:-7fffc69}

git show "$BASE:paper.tex" > .baseline.tex
# Tabellen und tikz werden als unteilbare Bloecke behandelt, sonst schiebt
# latexdiff seine Markierung mitten in cmidrule und tabular hinein.
# math-markup=whole aus demselben Grund fuer Formeln: auf der feinen Stufe
# zerlegt latexdiff \hat{\delta} und erzeugt unbalancierte Klammern. Eine
# geaenderte Formel erscheint damit als Ganzes in gruen.
latexdiff --encoding=utf8 \
  --append-safecmd="citep,citet,ref,eqref,open,hlo,label" \
  --math-markup=whole \
  --config "PICTUREENV=(?:picture|tikzpicture|tabular|table)[\w\d*@]*" \
  .baseline.tex paper.tex > paper_changes.tex 2>/dev/null

python3 - <<'PY'
import re
p = 'paper_changes.tex'
s = open(p, encoding='utf-8').read()
# Geloeschtes ganz herausschneiden. \renewcommand{\DIFdel}[1]{} allein genuegt
# nicht: enthielt der geloeschte Text eine Leerzeile, blieb ein leerer Absatz
# stehen, den lineno mitzaehlt. Nur der Textkoerper wird angefasst, denn in der
# Praeambel zerschneidet dieselbe Ersetzung die \usepackage-Zeilen.
head, sep, body = s.partition(r'\begin{document}')
assert sep, 'kein \\begin{document} gefunden'
for b, e in (('DIFdelbegin', 'DIFdelend'), ('DIFdelbeginFL', 'DIFdelendFL')):
    body, n = re.subn(r'\\%s\b.*?\\%s\b' % (b, e), '', body, flags=re.S)
    print(f'{n} {b}-Bloecke entfernt')
if body.count('{') != body.count('}'):
    raise SystemExit(f"Klammern unbalanciert: {body.count('{')} vs {body.count('}')}")
s = head + sep + body
patch = r"""
%DIF ---- gruene Markierung der Aenderungen ----
\definecolor{jdchg}{rgb}{0.00,0.45,0.16}
\renewcommand{\DIFadd}[1]{{\protect\color{jdchg}#1}}
\renewcommand{\DIFdel}[1]{}
%DIF ---- Ende ----
"""
s = s.replace(r'\begin{document}', patch + '\n' + r'\begin{document}', 1)
open(p, 'w', encoding='utf-8').write(s)
PY

latexmk -pdf -interaction=nonstopmode paper_changes.tex > /dev/null 2>&1
python3 build_changes_docx.py
rm -f .baseline.tex
echo "paper_changes.pdf und paper_changes.docx geschrieben"
