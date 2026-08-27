#!/usr/bin/env python3
"""Bringt die Dateinummern in literature/ wieder mit den Referenznummern des
gebauten Papers in Deckung.

Die Nummer im Dateinamen soll der Nummer im Literaturverzeichnis entsprechen.
Weil unsrtnat nach Erstnennung nummeriert, verschiebt jedes neue Zitat mitten
im Dokument alle nachfolgenden Nummern.

Aufruf:  python3 renumber_literature.py [--dry]

Schreibt literature/_undo_rename.sh, mit dem sich der Lauf zuruecknehmen laesst.
Der Ordner ist nicht in git, deshalb das Undo-Skript.
"""
import os, re, sys

HERE = os.path.dirname(os.path.abspath(__file__))
os.chdir(HERE)
LIT = "literature"
DRY = "--dry" in sys.argv

if not os.path.exists("paper.bbl"):
    sys.exit("paper.bbl fehlt. Zuerst latexmk -pdf paper.tex laufen lassen.")

keys = re.findall(r"\\bibitem\[[^\]]*\]\{([^}]+)\}", open("paper.bbl", encoding="utf-8").read())
if not keys:
    sys.exit("keine \\bibitem-Eintraege in paper.bbl gefunden")
num = {k: i for i, k in enumerate(keys, 1)}

plan = []
for f in sorted(os.listdir(LIT)):
    m = re.match(r"(\d{2})_(.+)\.pdf$", f)
    if not m:
        continue
    key, old = m.group(2), int(m.group(1))
    if key in num and num[key] != old:
        plan.append((f, f"{num[key]:02}_{key}.pdf"))

missing = [(i, k) for i, k in enumerate(keys, 1) if k not in
           {re.match(r"\d{2}_(.+)\.pdf$", f).group(1)
            for f in os.listdir(LIT) if re.match(r"\d{2}_.+\.pdf$", f)}]

if not plan:
    print("Nummerierung passt bereits.")
else:
    print(f"{len(plan)} Dateien umzubenennen:")
    for old, new in plan[:8]:
        print(f"  {old}  ->  {new}")
    if len(plan) > 8:
        print(f"  ... und {len(plan) - 8} weitere")

if plan and not DRY:
    with open(os.path.join(LIT, "_undo_rename.sh"), "w") as u:
        u.write('#!/bin/sh\n# nimmt den letzten renumber_literature.py-Lauf zurueck\n'
                'cd "$(dirname "$0")"\n')
        for old, new in plan:
            u.write(f'mv -n "{new}" "{old}"\n')
    os.chmod(os.path.join(LIT, "_undo_rename.sh"), 0o755)
    for old, new in plan:                     # zweiphasig gegen Kollisionen
        os.rename(os.path.join(LIT, old), os.path.join(LIT, "TMP__" + new))
    for old, new in plan:
        os.rename(os.path.join(LIT, "TMP__" + new), os.path.join(LIT, new))
    print("umbenannt, Undo in literature/_undo_rename.sh")

print(f"\nReferenzen: {len(keys)} | ohne PDF: {len(missing)}")
for i, k in missing:
    print(f"  [{i}] {k}")
