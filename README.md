# UK Road Safety Collisions 2024

This repository contains a reproducible data analysis project based on the **Road Safety Data — Collisions 2024** dataset published by the UK Department for Transport. The dataset describes police-recorded personal injury road collisions in Great Britain and includes information about collision time, location, road characteristics, weather, lighting, road surface, speed limits, number of casualties, and severity.

## Project overview

The goal of this project is to explore road collision patterns and gradually build a complete analytical workflow around one cleaned dataset. The project starts with data preparation and exploratory data analysis, then can be extended with statistical inference, regression analysis, nonparametric methods, and Bayesian modeling.

The main questions behind the project are:

- when collisions happen most often;
- whether weekdays and weekends have different collision patterns;
- how collision frequency changes by season and day of week;
- whether speed limits, road type, junction type, weather, lighting, and road surface conditions are associated with more severe outcomes;
- how urban and rural collisions differ;
- where collisions are spatially concentrated.

The cleaned dataset is intended to be reused across all future analysis stages, so every new part of the project should work from the same prepared data stored in `data/processed/`.

## Dataset

| Characteristic | Value |
|---|---:|
| Source | UK Department for Transport, STATS19 system |
| Dataset | Road Safety Data — Collisions 2024 |
| Original observations | 100,927 |
| Original variables | 44 |
| Final variables after cleaning | 39 |
| Unit of observation | One police-recorded personal injury collision |
| Geographic scope | Great Britain |
| Time period | 2024 |

Important limitation: the dataset includes only police-recorded personal injury collisions. It does not include damage-only collisions and may reflect differences in reporting quality across police forces.

## Repository structure

The repository is organized as follows:

```text
road-safety-analysis/
├── data/
│   ├── raw/              # Original source data
│   └── processed/        # Cleaned and prepared datasets
├── eda/                  # Exploratory data analysis notebooks and scripts
├── inference/            # Statistical inference notebooks and scripts
├── plots/
│   ├── eda/              # Figures produced during exploratory analysis
│   └── inference/        # Figures produced during statistical inference
├── renv/                 # R environment files
├── .Rprofile
├── .gitignore
├── renv.lock             # Reproducible R package environment
└── road-safety-analysis.Rproj
```

## Data preparation

Before analysis, the raw STATS19 data was cleaned and standardized:

- numeric category codes were decoded into readable text labels;
- non-standard missing value markers such as `Unknown`, `unknown (self reported)`, `Data missing or out of range`, and `Unallocated` were identified;
- missing value markers were converted to `NA` where appropriate;
- the value `-1` in the adjusted severity variable was treated as missing;
- date and time variables were converted to proper formats;
- technical identifiers, deprecated variables, and variables without analytical value were removed;
- an ambiguous 2024 coding issue in `junction_detail` was handled by treating code `19` as missing;
- categorical variables were kept as text in the cleaned CSV and can be converted to factors inside each notebook when needed.

## Exploratory Data Analysis

The first analysis block focuses on understanding the structure of the cleaned dataset and identifying visible patterns in collision frequency and severity.

### Main EDA directions

- **Time patterns:** analysis of collision frequency by hour, day of week, weekday/weekend type, month, and season.
- **Severity patterns:** comparison of fatal, serious, and slight collisions across time periods and road conditions.
- **Road infrastructure:** analysis of collision patterns by road type, junction type, speed limit, and urban/rural area.
- **Weather and lighting conditions:** evaluation of how weather, light conditions, and road surface conditions are associated with collision severity.
- **Spatial distribution:** overview of where collisions are geographically concentrated.

### Key findings from EDA

- Collisions are not evenly distributed throughout the day. Weekdays show clear morning and evening peaks, while weekends have a smoother daily pattern.
- The evening peak, especially around the end of the working day, is the most visible period of increased collision frequency.
- Night-time collisions are less frequent, but they tend to have a higher share of serious and fatal outcomes.
- Urban areas contain more collisions overall, while rural areas may show a higher relative severity of outcomes.
- Adverse road and lighting conditions appear to be associated with more severe collisions, although these relationships require further statistical testing.

All generated EDA figures are stored in:

```text
plots/eda/
```

## Reproducibility

1. Place the original 2024 collisions dataset in `data/raw/`.
2. Use the cleaned dataset from `data/processed/` for all analysis notebooks.
3. Restore the R environment if `renv` is used:

```r
renv::restore()
```

4. Render the EDA notebook:

```r
rmarkdown::render("eda/notebooks/...")
```

## Tools

The project is developed in R. The main packages used or recommended are:

- `tidyverse`
- `lubridate`
- `ggridges`
- `scales`
- `rmarkdown`

## Limitations

This project is descriptive and exploratory at the current stage. The observed patterns should not be interpreted as causal effects. Collision severity may depend on additional factors that are not fully captured in the dataset, such as driver behavior, traffic volume, vehicle type, exact road geometry, and local infrastructure conditions.

The dataset covers only one year, so long-term trends require adding data from previous years.
