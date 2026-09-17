# Join key

Merge curated tables on English `province` names and standardized `antibiotic` names. `yearbook_province.csv` also carries a `year` column (population and urban share: 2015–2024; other core metrics: 2024 only) for optional alignment with CARSS years. Environmental concentrations are only comparable within the matrix ↔ unit system below. Column definitions: [`codebook.csv`](codebook.csv).

## 1. Province key (31)

Canonical provincial-level units (CARSS / NBS English names; `Tibet` for Xizang).

- Anhui
- Beijing
- Chongqing
- Fujian
- Gansu
- Guangdong
- Guangxi
- Guizhou
- Hainan
- Hebei
- Heilongjiang
- Henan
- Hubei
- Hunan
- Inner Mongolia
- Jiangsu
- Jiangxi
- Jilin
- Liaoning
- Ningxia
- Qinghai
- Shaanxi
- Shandong
- Shanghai
- Shanxi
- Sichuan
- Tianjin
- Tibet
- Xinjiang
- Yunnan
- Zhejiang

## 2. env ∩ CARSS compounds (15)

Exact name intersection of `env_records` and `resistance_province`. Use these for cross-domain joins; env-only and CARSS-only compounds remain in their domains.

| antibiotic | antibiotic_class |
| --- | --- |
| Ampicillin | Beta-lactams |
| Cefazolin | Beta-lactams |
| Cefotaxime | Beta-lactams |
| Ceftriaxone | Beta-lactams |
| Chloramphenicol | Phenicols |
| Ciprofloxacin | Fluoroquinolones |
| Clindamycin | Lincosamides |
| Erythromycin | Macrolides |
| Gentamicin | Other |
| Levofloxacin | Fluoroquinolones |
| Minocycline | Tetracyclines |
| Penicillin G | Beta-lactams |
| Rifampicin | Other |
| Tetracycline | Tetracyclines |
| Vancomycin | Other |

## 3. Matrix ↔ unit rules

Target units after conversion in `R/utils/environmental_units.R` (`target_concentration_unit`). Incompatible matrix–unit pairs are dropped in R/01.

| Matrix | Target unit |
| --- | --- |
| soil; sediment; sludge | `ng/g` |
| soil; sediment; sludge (source unit dry-weight) | `ng/g dw` |
| surface water; wastewater influent; wastewater effluent | `ng/L` |

## 4. Antibiotic-class lookup

133 compounds → 10 pharmacological classes (from `data/raw/reference/antibiotic_group_lookup.csv`). Machine-readable copy: [`antibiotic_classes.csv`](antibiotic_classes.csv).

**Beta-lactams.** Amoxicillin; Ampicillin; Aztreonam; Cefadroxil; Cefazolin; Cefepime; Cefotaxime; Cefotetan; Cefoxitin; Ceftazidime; Ceftriaxone; Cefuroxime; Cephalexin; Cloxacillin; Deacetoxycephalosporin; Ertapenem; Imipenem; Mecillinam; Meropenem; Oxacillin; Penicillin G; Penicillin V; Piperacillin

**Diaminopyrimidines.** Ormetoprim; Trimethoprim

**Fluoroquinolones.** Ciprofloxacin; Danofloxacin; Difloxacin; Enoxacin; Enrofloxacin; Fleroxacin; Flumequine; Gatifloxacin; Levofloxacin; Lomefloxacin; Marbofloxacin; Moxifloxacin; Nadifloxacin; Norfloxacin; Ofloxacin; Pefloxacin; Sarafloxacin; Sparfloxacin; Tosufloxacin

**Lincosamides.** Clindamycin; Lincomycin

**Macrolides.** Acetylspiramycin; Azithromycin; Clarithromycin; Dehydroerythromycin; Erythromycin; Erythromycin A dihydrate; Erythromycin-H2O; Josamycin; Kitasamycin; Leucomycin; Oleandomycin; Roxithromycin; Roxithromycin-H2O; Spiramycin; Tilmicosin; Tylosin

**Other.** Amikacin; Bacitracin; Carbadox; Furazolidone; Gentamicin; Kanamycin; Linezolid; Metronidazole; Neomycin; Nitrofurantoin; Olaquindox; Paromomycin; Polymyxin B; Rifampicin; Spectinomycin; Streptomycin; Teicoplanin; Tobramycin; Trimethoprim/Sulfamethoxazole; Vancomycin

**Phenicols.** Chloramphenicol; Florfenicol; Thiamphenicol

**Quinolones.** Cinoxacin; Nalidixic Acid; Oxolinic Acid; Pipemidic Acid

**Sulfonamides.** Acetylsulfamethazine; Acetylsulfamethoxazole; Sulfabenzamide; Sulfacetamide; Sulfachinoxaline; Sulfachloropyridazine; Sulfadiazine; Sulfadimethoxine; Sulfadimethoxypyrimidine; Sulfadimidine; Sulfadimoxine; Sulfadoxine; Sulfafurazole; Sulfaguanidine; Sulfamerazine; Sulfameter; Sulfamethizole; Sulfamethoxazole; Sulfamethoxydiazine; Sulfamethoxypyridazine; Sulfamonomethoxine; Sulfamoxole; Sulfanilamide; Sulfanitran; Sulfaphenazole; Sulfapyridine; Sulfaquinoxaline; Sulfathiazole; Sulfisomidine

**Tetracyclines.** 4-Epichlortetracycline; Anhydrochlortetracycline; Apo-Oxytetracycline; Chlortetracycline; Demeclocycline; Doxycycline; Epianhydrotetracycline; Epioxytetracycline; Epitetracycline; Isochlortetracycline; Methacycline; Minocycline; Oxytetracycline; Tetracycline; Tigecycline

