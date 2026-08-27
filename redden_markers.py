#!/usr/bin/env python3
"""Faerbt die offenen Marker in einem von pandoc erzeugten docx rot und fett.

Pandoc kennt \\textcolor nicht und liefert '[OPEN: ...]' unauffaellig in
Kapitaelchen aus. Hier werden genau diese Laeufe nachtraeglich eingefaerbt,
damit im Word sichtbar ist, was noch offen ist.

Aufruf: python3 redden_markers.py <datei.docx>
"""
import os, re, shutil, sys, zipfile

RED = '<w:color w:val="C00000"/><w:b/>'


def redden(path):
    tmp = path + ".unzip"
    shutil.rmtree(tmp, ignore_errors=True)
    with zipfile.ZipFile(path) as z:
        z.extractall(tmp)
        names = z.namelist()

    doc_path = os.path.join(tmp, "word", "document.xml")
    doc = open(doc_path, encoding="utf-8").read()

    runs = list(re.finditer(r"<w:r>(?:(?!</w:r>).)*</w:r>", doc, re.S))
    texts = [re.sub(r"<[^>]+>", "", r.group(0)) for r in runs]

    targets, i = set(), 0
    while i < len(runs):
        # ein Marker beginnt mit dem Lauf 'open' in Kapitaelchen
        if "<w:smallCaps" in runs[i].group(0) and texts[i].strip().lower() == "open":
            start = i - 1 if i > 0 and texts[i - 1].rstrip().endswith("[") else i
            j = i
            while j < len(runs) and "]" not in texts[j]:
                j += 1
            for k in range(start, min(j + 1, len(runs))):
                targets.add(k)
            i = j + 1
            continue
        i += 1

    for k in sorted(targets, reverse=True):
        r = runs[k]
        body = r.group(0)
        if "<w:rPr>" in body:
            new = body.replace("<w:rPr>", "<w:rPr>" + RED, 1)
        else:
            new = body.replace("<w:r>", "<w:r><w:rPr>" + RED + "</w:rPr>", 1)
        doc = doc[:r.start()] + new + doc[r.end():]

    open(doc_path, "w", encoding="utf-8").write(doc)
    os.remove(path)
    with zipfile.ZipFile(path, "w", zipfile.ZIP_DEFLATED) as z:
        for n in names:
            z.write(os.path.join(tmp, n), n)
    shutil.rmtree(tmp, ignore_errors=True)
    return len(targets)


if __name__ == "__main__":
    for f in sys.argv[1:]:
        n = redden(f)
        print(f"{os.path.basename(f)}: {n} Marker-Laeufe rot gesetzt")
