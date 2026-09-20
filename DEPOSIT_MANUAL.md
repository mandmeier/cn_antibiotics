# Manual Zenodo steps (status)

## Done

- Data deposit published (restricted files): https://doi.org/10.5281/zenodo.22862670  
  Concept DOI (all versions): https://doi.org/10.5281/zenodo.22807174  
- Software archive (GitHub release v1.0.1): https://doi.org/10.5281/zenodo.22807986  
- Repo scaffolding + source licensing docs: `LICENSE`, `CITATION.cff`, `.zenodo.json`, `SOURCES_AND_LICENSES.md`, `deposit/README_deposit.md`, `R/08_stage_deposit.R`

Rebuild the local zip after licensing doc updates (then upload a **new Zenodo version** if you want the zip on Zenodo to match; metadata-only edits do not require a new file version):

```bash
Rscript R/08_stage_deposit.R
```

## Sync licensing text to the published Zenodo data record (do this)

On https://doi.org/10.5281/zenodo.22862670 → **Edit**:

1. Replace **Description** with the “Sources, licensing, and attribution” section from `deposit/README_deposit.md` (or the full README body).
2. Add **Related works**:
   - Is derived from / Documents → `10.6084/m9.figshare.19692241.v1` (Dataset)
   - Is derived from / Documents → `10.1038/s41597-022-01384-5` (Publication)
   - Is compiled by → `https://github.com/mandmeier/cn_antibiotics` (Software) — already present
   - Is supplemented by → `10.5281/zenodo.22807986` (Software)
3. Keep license **CC BY 4.0** and copyright naming both creators.
4. Optionally upload a new version of the zip that includes the updated `README_deposit.md` (new Zenodo version DOI); otherwise metadata edit alone is enough for review.

## Your remaining steps (round 1)

1. Grant **file access** on the data record to editors/reviewers when you have them.
2. GitHub is public; leave as-is unless the journal asks otherwise.

## At acceptance

1. Open the Zenodo **data** files (public download) under CC BY 4.0.
2. Confirm both DOIs plus Zhang Figshare citation in the Data Descriptor Data Availability section:
   - Data: https://doi.org/10.5281/zenodo.22862670
   - Software: https://doi.org/10.5281/zenodo.22807986
   - Zhang Figshare: https://doi.org/10.6084/m9.figshare.19692241.v1
