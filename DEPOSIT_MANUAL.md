# Manual Zenodo steps (status)

## Done

- Data deposit published (restricted files): https://doi.org/10.5281/zenodo.22807175  
  Concept DOI (all versions): https://doi.org/10.5281/zenodo.22807174  
- Software archive (GitHub release v1.0.1): https://doi.org/10.5281/zenodo.22807986  
- Repo scaffolding: `LICENSE` (MIT), `CITATION.cff`, `.zenodo.json`, `deposit/README_deposit.md`, `R/10_stage_deposit.R`

Rebuild the local zip only if curated CSVs change (do **not** replace files on the published Zenodo data version — upload a new version instead):

```bash
Rscript R/10_stage_deposit.R
```

## Your remaining steps (round 1)

1. On the [Zenodo data record](https://doi.org/10.5281/zenodo.22807175), grant **file access** to editors/reviewers (approve access requests or add permitted users).
2. Keep GitHub private if desired; invite reviewers/collaborators as needed.

## At acceptance

1. Open the Zenodo **data** files (public download) under CC BY 4.0.
2. Make the GitHub repository **public** (if still private).
3. Confirm both DOIs in the Data Descriptor Data Availability section:
   - Data: https://doi.org/10.5281/zenodo.22807175
   - Software: https://doi.org/10.5281/zenodo.22807986
