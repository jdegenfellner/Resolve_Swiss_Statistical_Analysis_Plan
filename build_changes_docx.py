#!/usr/bin/env python3
"""Baut paper_changes.docx: alle Aenderungen seit der kommentierten Fassung
gruen hervorgehoben, dazu Word-Kommentare, die jede Rueckmeldung von Fabian
und Andrea beantworten.

Voraussetzung: paper_changes.tex existiert (aus latexdiff, siehe make_changes.sh).
"""
import os, re, shutil, subprocess, sys, zipfile
from datetime import datetime, timezone

HERE = os.path.dirname(os.path.abspath(__file__))
os.chdir(HERE)

STAMP = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

# (Ankertext im Absatz, Autor, Antworttext)
ANSWERS = [
    ("on average, modest", "JD",
     "Fabian, zu deinem 'REF needed': du hattest recht, die Aussage stand "
     "unbelegt da. Die Quelle fand sich in unserem eigenen Studienprotokoll, "
     "Referenz 31, Wand und O'Connell 2008 in BMC Musculoskeletal Disorders. "
     "Dort steht, klinische Studien zeigten fuer die meisten gaengigen "
     "Interventionen bei chronisch unspezifischem Kreuzschmerz nur begrenzte "
     "Wirksamkeit. Weil sie allgemein von den gaengigen Interventionen "
     "sprechen und nicht speziell von Physiotherapie, heisst es im Text jetzt "
     "'the treatments commonly offered for it' statt 'the treatments "
     "physiotherapists currently offer'. Falls du etwas Neueres und "
     "Physiotherapie-Spezifisches hast, tausche ich es gern aus."),
    ("Two things are unresolved", "JD",
     "Fabian: Abschnitt umgebaut nach deiner Gliederung, erst die "
     "Forschungsluecke, dann die pragmatische Umsetzung, dann die Begruendung "
     "der Cluster-Randomisierung."),
    ("question of efficacy", "JD",
     "Fabian: efficacy und effectiveness stehen jetzt explizit im Text."),
    ("Randomisation at the individual participant level", "JD",
     "Fabian: dein Ersatzsatz ist uebernommen. Die drei Saetze ueber "
     "Therapeuten, die nebeneinander arbeiten, sind weg, ebenso die "
     "Hedge-Konstruktion beim Austausch zwischen Praxen."),
    ("complementary Bayesian analysis", "JD",
     "Fabian: 'comparative' zu 'complementary' geaendert, an beiden Stellen."),
    ("All outcomes are assessed at baseline", "JD",
     "Fabian: umformuliert. Deine Frage hat ausserdem einen Widerspruch "
     "aufgedeckt, Abschnitt 2.9.2 sagte 'at 18 weeks only'. Das Studienprotokoll "
     "ist eindeutig, alle Outcomes werden zu Baseline und Woche 18 erhoben, "
     "korrigiert."),
    ("randomised 1:1", "JD",
     "Fabian, zu den Querverweisen: die bleiben. In einem SAP ueber 24 Seiten "
     "sind sie ueblich, Trials nummeriert die Abschnitte ohnehin, und die "
     "Fallzahlrechnung greift mehrfach auf den Randomisierungsabschnitt zurueck."),
    ("BASEC 2025-00784", "JD",
     "Fabian: gekuerzt, hier steht nur noch die Nummer. Beschlussdatum und "
     "Auflagenerfuellung stehen in den Declarations."),
    ("Eligibility operates at two levels", "JD",
     "Fabian: patient und participant sind vereinheitlicht, deine Vermutung war "
     "richtig. Vor dem Einschluss patient, danach participant. Sieben Stellen "
     "geaendert."),
    ("50-hour RESOLVE training course", "JD",
     "Andrea: korrigiert, es sind 50 Stunden. Kapitel 9 des Studienprotokolls "
     "bestaetigt dich, dort steht eine 50-stuendige Ausbildung."),
    ("minimum practice size", "JD",
     "Andrea: das ist noch offen und geht ans Team. Im Studienprotokoll finde "
     "ich kein solches Kriterium, dort steht bei den Praxen nur die "
     "Bereitschaft, jedem Arm zugeteilt zu werden."),
    ("completed the 50-hour training", "JD",
     "Andrea: korrigiert, siehe oben."),
    ("control practices actually deliver", "JD",
     "Andrea, dein Kommentar zum 'anderen Sample' haengt an Fabians "
     "umgeschriebener Fassung dieses Satzes. Welches Sample meinst du, mit dem "
     "verglichen werden soll?"),
    ("no further practices are being recruited", "JD",
     "Andrea, zu 'Ist das schon sicher??': nein, war es nicht. Das Team hat "
     "inzwischen entschieden, nicht weiter zu rekrutieren. Der SAP behauptet "
     "das jetzt auch nicht mehr."),
    ("trial statistician is not blinded", "JD",
     "Fabian: Begruendung ergaenzt. Die Auswertung braucht den vollstaendigen "
     "Datensatz einschliesslich der Zuordnung von Praxis zu Arm."),
    ("consider worth detecting", "JD",
     "Fabian, zu 'REF?': die 2 Punkte sind keine MID aus der Literatur, sondern "
     "ein bewusst konservativ gewaehltes Zielkriterium. Das steht jetzt so da, "
     "begruendet gegen RESTORE (4.6 Punkte, n=492) und Ben-Ami (2.7 Punkte, "
     "n=220)."),
    ("Their model splits the practice effect", "JD",
     "Fabian, zur Detailtiefe: die bleibt, aber der Abschnitt ist inzwischen "
     "umgebaut. Er folgt jetzt weitgehend einer einzigen Quelle, Teerenstra "
     "et al. 2012, statt eine Normalapproximation zu nehmen und sie danach zu "
     "korrigieren. Einzige Abweichung ist der Design-Effekt, denn Teerenstra "
     "nimmt gleich grosse Praxen an; fuer die Streuung der Praxisgroessen "
     "folgen wir Eldridge et al. 2006. Das ist kuerzer zu zitieren und "
     "leichter nachzuvollziehen. "
     "Die Detailtiefe selbst verlangt Gamble et al. 2017, die Leitlinie fuer "
     "SAP-Publikationen."),
    ("The trial nevertheless keeps its 15 practices", "JD",
     "Andrea: hier steht jetzt, warum nicht weiter rekrutiert wird. Mehr Praxen "
     "erhoehen die Power, lassen aber jedem Therapeuten weniger Faelle, und die "
     "Therapeuten werden mit Wiederholung besser. Dieser Verlust ist nicht "
     "quantifizierbar, deshalb die Zurueckhaltung."),
    ("intention to treat", "JD",
     "Fabian, zu den Aufzaehlungen: die bleiben vorerst. Es sind genau zwei "
     "Listen im ganzen Dokument, und ueber das Layout entscheidet am Ende das "
     "Journal."),
    ("patient-reported continuous outcomes", "JD",
     "Fabian: alle Instrumente sind jetzt zitiert, mit den Quellen, die das "
     "Studienprotokoll verwendet, plus die physischen Messungen und die GAS. "
     "Offen ist nur der OLEQ, der hat im Protokoll keine Quelle."),
    ("50-hour RESOLVE training is unavailable", "JD",
     "Andrea: korrigiert, siehe oben."),
    # --- Antworten auf die nachverfolgten Textaenderungen ---
    ("graded motor relearning", "JD",
     "Andrea, du hast hier 'relearning' zu 'retraining' geaendert. Das muss "
     "einheitlich werden, und zwar auch im Studienprotokoll: dort stehen "
     "zweimal 'graded motor relearning', einmal 'graded motor retraining', "
     "dazu 'graded sensorimotor relearning' und 'graded sensory retraining'. "
     "Bitte legt eine Schreibweise fest, dann ziehe ich sie durch."),
    ("Withdrawals since then leave", "JD",
     "Andrea: deine Korrektur von 7 auf 6 ist uebernommen, danke, das war "
     "nicht kosmetisch. Die gesamte Power-Rechnung haengt daran und ist neu "
     "gerechnet: 15 statt 16 Praxen, 13 statt 14 Freiheitsgrade, und die Power "
     "des aktuellen Splits faellt von 0.82 auf 0.80. Tabelle 2 und die "
     "Abbildung sind entsprechend neu."),
    ("control practices actually deliver", "JD",
     "Fabian: deine Umschreibung dieses Satzes habe ich nicht uebernommen, "
     "aber dein Unbehagen war berechtigt. Der Nebensatz 'because in a "
     "pragmatic trial the comparator is itself part of the finding' ist "
     "ersatzlos gestrichen, er erklaerte nur, was der erste Satz ohnehin "
     "sagt. Deine beiden anderen Umschreibungen sind drin, die zum Zweck des "
     "Plans und die in 2.4."),
    ("was developed prior to the analysis", "JD",
     "Fabian: deine Formulierung uebernommen, sie ist die uebliche in "
     "publizierten SAPs. Nur 'a summary of' vor der Bayes-Zwischenanalyse habe "
     "ich stehen lassen, weil dieser Plan die Zwischenanalyse tatsaechlich nur "
     "zusammenfasst und noch nicht ausfuehrt."),
    ("pain science", "JD",
     "Andrea: deine beiden Korrekturen sind uebernommen, in beiden Faellen "
     "hast du recht. Das Studienprotokoll schreibt siebzehnmal 'pain science "
     "education' und nie 'neuroscience', und es schreibt achtmal 'chronic "
     "non-specific low back pain'. Steht jetzt so im Titel, im Abstract und in "
     "der Einleitung."),
    ("Two implications arise", "JD",
     "Fabian: uebernommen. Dein 'initial allocation' habe ich weggelassen, "
     "weil inzwischen entschieden ist, dass keine zweite Welle kommt, und "
     "'initial' dann eine Fortsetzung suggeriert."),
]

W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"


def check_fresh():
    """Abbruch, wenn die Diff-Datei aelter ist als das Manuskript."""
    if not os.path.exists("paper_changes.tex"):
        sys.exit("paper_changes.tex fehlt. Zuerst ./make_changes.sh laufen lassen.")
    if os.path.getmtime("paper_changes.tex") < os.path.getmtime("paper.tex"):
        sys.exit("paper_changes.tex ist aelter als paper.tex. "
                 "Nicht direkt aufrufen, sondern ./make_changes.sh benutzen.")


def make_docx_tex():
    """paper_changes.tex -> Variante fuer pandoc: DIFadd als \\hl, Flussdiagramm als PNG."""
    s = open("paper_changes.tex", encoding="utf-8").read()
    s = re.sub(r"\\renewcommand\{\\DIFadd\}\[1\]\{[^\n]*\}",
               r"\\renewcommand{\\DIFadd}[1]{\\hl{#1}}", s)
    if "\\usepackage{soul}" not in s:
        s = s.replace("\\begin{document}", "\\usepackage{soul}\n\\begin{document}", 1)
    s = re.sub(r"\\begin\{tikzpicture\}.*?\\end\{tikzpicture\}",
               r"\\includegraphics[width=\\textwidth]{figures/flow_diagram.png}",
               s, flags=re.S)
    open(".paper_changes_docx.tex", "w", encoding="utf-8").write(s)


def run_pandoc():
    subprocess.run(["pandoc", ".paper_changes_docx.tex", "--citeproc",
                    "--bibliography=references.bib", "--number-sections",
                    "-M", "link-citations=true", "-o", "paper_changes.docx"],
                   check=True)


def patch_docx():
    src = "paper_changes.docx"
    tmp = ".pc_unzip"
    shutil.rmtree(tmp, ignore_errors=True)
    with zipfile.ZipFile(src) as z:
        z.extractall(tmp)
        names = z.namelist()

    doc_path = os.path.join(tmp, "word", "document.xml")
    doc = open(doc_path, encoding="utf-8").read()

    # 1. gelbe Hervorhebung -> gruen
    doc, n_hl = re.subn(r'(<w:highlight[^>]*w:val=")yellow(")', r"\1green\2", doc)

    # 2. Kommentare einhaengen
    paras = list(re.finditer(r"<w:p\b[^>]*>.*?</w:p>", doc, re.S))
    # pandoc setzt geschuetzte Leerzeichen vor Zitationen, deshalb wird beim
    # Abgleich jede Folge von Leerraum auf ein einfaches Leerzeichen normiert.
    def norm(t):
        return re.sub(r"[\s\u00a0]+", " ", t).lower()
    plains = [norm(re.sub(r"<[^>]+>", "", m.group(0))) for m in paras]
    comments, missed = [], []
    by_para = {}          # Absatzindex -> Liste von Kommentar-Ids
    for cid, (anchor, author, text) in enumerate(ANSWERS):
        hit = None
        needle = norm(anchor)
        for i in range(len(paras)):
            if needle in plains[i]:
                hit = i
                break
        if hit is None:
            missed.append(anchor)
            continue
        by_para.setdefault(hit, []).append(cid)
        comments.append((cid, author, text))

    # von hinten nach vorne einsetzen, damit die Offsets stimmen
    for i in sorted(by_para, reverse=True):
        m = paras[i]
        body = m.group(0)
        pos = re.match(r"<w:p\b[^>]*>", body).end()
        m_ppr = re.match(r"\s*<w:pPr>.*?</w:pPr>", body[pos:], re.S)
        if m_ppr:
            pos += m_ppr.end()
        ids = by_para[i]
        starts = "".join(f'<w:commentRangeStart w:id="{c}"/>' for c in ids)
        ends = "".join(f'<w:commentRangeEnd w:id="{c}"/>'
                       f'<w:r><w:commentReference w:id="{c}"/></w:r>' for c in ids)
        new_body = body[:pos] + starts + body[pos:-len("</w:p>")] + ends + "</w:p>"
        doc = doc[:m.start()] + new_body + doc[m.end():]
    placed = len(comments)
    open(doc_path, "w", encoding="utf-8").write(doc)

    # 3. comments.xml
    def esc(t):
        return (t.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;"))

    parts = []
    for cid, author, text in comments:
        runs = "".join(
            f'<w:r><w:t xml:space="preserve">{esc(line)}</w:t></w:r>'
            for line in [text])
        parts.append(
            f'<w:comment w:id="{cid}" w:author="{esc(author)}" '
            f'w:date="{STAMP}" w:initials="{esc(author)}">'
            f'<w:p>{runs}</w:p></w:comment>')
    comments_xml = (
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
        f'<w:comments xmlns:w="{W}">' + "".join(parts) + "</w:comments>")
    open(os.path.join(tmp, "word", "comments.xml"), "w", encoding="utf-8").write(comments_xml)

    # 4. Beziehung und Content-Type registrieren
    rels_path = os.path.join(tmp, "word", "_rels", "document.xml.rels")
    rels = open(rels_path, encoding="utf-8").read()
    if "comments.xml" not in rels:
        ids = [int(x) for x in re.findall(r'Id="rId(\d+)"', rels)] or [0]
        rid = f"rId{max(ids) + 1}"
        rels = rels.replace(
            "</Relationships>",
            f'<Relationship Id="{rid}" Type="http://schemas.openxmlformats.org/'
            f'officeDocument/2006/relationships/comments" Target="comments.xml"/>'
            "</Relationships>")
        open(rels_path, "w", encoding="utf-8").write(rels)

    ct_path = os.path.join(tmp, "[Content_Types].xml")
    ct = open(ct_path, encoding="utf-8").read()
    if "comments+xml" not in ct:
        ct = ct.replace(
            "</Types>",
            '<Override PartName="/word/comments.xml" ContentType="application/'
            'vnd.openxmlformats-officedocument.wordprocessingml.comments+xml"/>'
            "</Types>")
        open(ct_path, "w", encoding="utf-8").write(ct)

    # 5. neu packen
    if "word/comments.xml" not in names:
        names.append("word/comments.xml")
    os.remove(src)
    with zipfile.ZipFile(src, "w", zipfile.ZIP_DEFLATED) as z:
        for name in names:
            z.write(os.path.join(tmp, name), name)
    shutil.rmtree(tmp, ignore_errors=True)
    return n_hl, placed, missed


if __name__ == "__main__":
    check_fresh()
    make_docx_tex()
    run_pandoc()
    n_hl, placed, missed = patch_docx()
    subprocess.run([sys.executable, "redden_markers.py", "paper_changes.docx"],
                   check=True)
    os.remove(".paper_changes_docx.tex")
    print(f"gruene Markierungen: {n_hl}")
    print(f"Kommentare gesetzt : {placed} von {len(ANSWERS)}")
    if missed:
        print("kein Anker gefunden fuer:")
        for a in missed:
            print("  -", a)
