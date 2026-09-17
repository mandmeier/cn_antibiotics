# Manual Zenodo steps (after repo scaffolding)

Agent-prepared artifacts live in this repository:

- `deposit/README_deposit.md` — deposit readme (CC BY 4.0)
- `R/10_stage_deposit.R` — rebuilds `deposit/zenodo_v1/` and `deposit/cn_antibiotics_data_v1.0.0.zip`
- `LICENSE` (MIT), `CITATION.cff`, `.zenodo.json` — code / software archive metadata

Run before uploading if curated CSVs changed:

```bash
Rscript R/10_stage_deposit.R
```

## Round 1 (reviewers)

1. Log in to [Zenodo](https://zenodo.org/) and create a **New upload**.
2. Upload `deposit/cn_antibiotics_data_v1.0.0.zip`.
3. Set **Access right** to **Restricted** (or closed with a secret link), license **Creative Commons Attribution 4.0 International**.
4. Title/description can follow `deposit/README_deposit.md`. Publish the restricted record and copy the **secret/reviewer link** for Scientific Data.
5. Keep the GitHub repo private; share a collaborator invite or private link with editors/reviewers as needed.

## Software DOI (GitHub → Zenodo)

1. In Zenodo, enable **GitHub** integration and flip on the `mandmeier/cn_antibiotics` repository.
2. After the `v1.0.0` GitHub Release is created, Zenodo will draft a software archive; **publish** it and copy the software DOI.
3. Replace `SOFTWARE_DOI` / `DATA_DOI` placeholders in `CITATION.cff`, `.zenodo.json`, `README.md`, and `deposit/README_deposit.md` (or ask the agent to fill them).

### Create the GitHub release (when ready)

From the repository root, after scaffolding is on the branch you want to tag:

```bash
git checkout main   # or merge feature branch first
git pull
git tag -a v1.0.0 -m "v1.0.0 curated data and pipeline for Scientific Data"
git push origin v1.0.0
gh release create v1.0.0 --title "v1.0.0" --notes "Curated environmental, CARSS, and yearbook tables plus R pipeline. Data deposit: see Zenodo DATA_DOI (restricted until acceptance)."
```

Do **not** create this release until Zenodo GitHub integration is enabled if you want the first tag to mint the software DOI automatically.

## At acceptance

1. Make the Zenodo **data** record **Open** (public) under CC BY 4.0.
2. Make the GitHub repository **public**.
3. Paste final data and software DOIs into the Data Descriptor Data Availability section.
