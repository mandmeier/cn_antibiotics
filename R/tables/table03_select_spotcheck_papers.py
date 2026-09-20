#!/usr/bin/env python3
"""Select 20 spot-check papers (10 Zhang + 10 Supplemental) and stage folders.

Reproducible RNG seed. Does not overwrite agreement scores in table03_spotcheck.csv
once filled — only (re)builds paper folders + selection metadata.

Paper folders stay in the parent-repo figures_sandbox/ (not under cn_antibiotics/).

Run from cn_antibiotics/ (or any cwd; paths are absolute from this file):
  python3 R/tables/table03_select_spotcheck_papers.py
"""

from __future__ import annotations

import csv
import random
import re
from pathlib import Path

CN = Path(__file__).resolve().parents[2]
ROOT = CN.parent
OUT = ROOT / "figures_sandbox" / "table03_spotcheck_papers"
TABLES = CN / "tables"
SEED = 20250918


def classify(ref: str) -> str:
    ref = str(ref)
    if ref.startswith("NEW_") or ref.startswith("CAND_") or re.match(r"^N\d+$", ref):
        return "Supplemental"
    return "Zhang"


def main() -> None:
    random.seed(SEED)
    env = list(csv.DictReader((CN / "data/output/supporting/env_records.csv").open()))
    src = {
        r["pub_id"]: r
        for r in csv.DictReader(
            (CN / "data/raw/environmental_data/Data_Sources.csv").open()
        )
    }

    pools: dict[str, list[str]] = {"Zhang": [], "Supplemental": []}
    for ref in sorted({str(r["reference_number"]) for r in env}):
        if not (src.get(ref, {}).get("pub_full") or "").strip():
            continue
        pools[classify(ref)].append(ref)

    zhang_sel = sorted(random.sample(pools["Zhang"], 10))
    supp_sel = sorted(random.sample(pools["Supplemental"], 10))
    selected = [("Zhang", r) for r in zhang_sel] + [
        ("Supplemental", r) for r in supp_sel
    ]

    OUT.mkdir(parents=True, exist_ok=True)
    doi_re = re.compile(r"10\.\d{4,9}/[-._;()/:A-Z0-9]+", re.I)

    manifest = []
    for i, (group, ref) in enumerate(selected, 1):
        s = src[ref]
        citation = (s.get("pub_full") or "").strip()
        link = (s.get("article_link") or "").strip()
        doi = ""
        for text in (link, citation):
            m = doi_re.search(text)
            if m:
                doi = m.group(0).rstrip(").,;")
                break
        rows = [r for r in env if str(r["reference_number"]) == ref]
        paper_dir = OUT / f"{i:02d}_{group[:4].lower()}_{ref}"
        paper_dir.mkdir(exist_ok=True)

        with (paper_dir / "env_records_subset.csv").open("w", newline="") as f:
            w = csv.DictWriter(f, fieldnames=rows[0].keys())
            w.writeheader()
            w.writerows(rows)

        (paper_dir / "CITATION.txt").write_text(
            f"check_id: {i}\n"
            f"source_group: {group}\n"
            f"reference_number: {ref}\n"
            f"n_env_records: {len(rows)}\n"
            f"doi: {doi or 'NA'}\n"
            f"article_link: {link or 'NA'}\n\n"
            f"{citation}\n",
            encoding="utf-8",
        )

        manifest.append(
            {
                "check_id": i,
                "source_group": group,
                "reference_number": ref,
                "citation": citation,
                "article_link": link,
                "doi": doi,
                "n_env_records": len(rows),
            }
        )

    with (OUT / "SELECTION_MANIFEST.csv").open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(manifest[0].keys()))
        w.writeheader()
        w.writerows(manifest)

    (OUT / "README.md").write_text(
        f"""# Table 3 spot-check paper set

Random sample of **20** papers that contribute rows to `env_records.csv`
(10 Zhang numeric `pub_id`, 10 supplemental).

- **RNG seed:** `{SEED}` (`random.sample`)
- **Sampling frame:** unique `reference_number` in cleaned `env_records`
  with a non-empty citation in `Data_Sources.csv`
- **Pool sizes:** Zhang={len(pools['Zhang'])}, Supplemental={len(pools['Supplemental'])}

Each subfolder:

- `CITATION.txt` — id, DOI/link, citation
- `env_records_subset.csv` — curated rows to verify against the paper
- `paper.pdf` — OA copy if obtained
- `OPEN_LINK.txt` — manual download instructions when PDF is paywalled

Selection list for the manuscript table:
`cn_antibiotics/tables/table03_spotcheck.csv`

Rebuild folders (from `cn_antibiotics/`):
`python3 R/tables/table03_select_spotcheck_papers.py`
""",
        encoding="utf-8",
    )

    print("Selected Zhang:", zhang_sel)
    print("Selected Supp:", supp_sel)
    print("Wrote", OUT)
    print("Manuscript table CSV:", TABLES / "table03_spotcheck.csv")


if __name__ == "__main__":
    main()
