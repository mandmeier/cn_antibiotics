# Manual Zenodo steps (status)

## Done

- Data deposit published (restricted files): https://doi.org/10.5281/zenodo.22807175  
  Concept DOI (all versions): https://doi.org/10.5281/zenodo.22807174  
- Repo scaffolding: `LICENSE` (MIT), `CITATION.cff`, `.zenodo.json`, `deposit/README_deposit.md`, `R/10_stage_deposit.R`

Rebuild the local zip only if curated CSVs change (do **not** replace files on the published Zenodo version — upload a new version instead):

```bash
Rscript R/10_stage_deposit.R
```

## Your remaining steps (round 1)

1. On the [Zenodo data record](https://doi.org/10.5281/zenodo.22807175), grant **file access** to editors/reviewers (approve access requests or add permitted users).
2. Keep GitHub private; invite reviewers/collaborators as needed.
3. In Zenodo → GitHub settings, enable integration for `mandmeier/cn_antibiotics` **before** relying on the software DOI webhook.

## Software DOI (GitHub → Zenodo)

After GitHub integration is on and `v1.0.0` exists as a GitHub Release:

1. Open the Zenodo draft created from the release and **Publish** it.
2. Copy the software DOI and replace `SOFTWARE_DOI` in `CITATION.cff`, `README.md`, and the manuscript (or ask the agent).

## At acceptance

1. Open the Zenodo **data** files (public download) under CC BY 4.0.
2. Make the GitHub repository **public**.
3. Confirm both DOIs in the Data Descriptor Data Availability section.
