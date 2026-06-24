# 🏡 HousePredX — AI-Powered House Valuation & Renovation Advisor

> Predict property prices and discover high-ROI renovation strategies — powered by a tuned Random Forest model and deployed as an interactive Shiny web app.

**Live Demo → [aditya-kumar-roy.shinyapps.io/HousePredX](https://aditya-kumar-roy.shinyapps.io/HousePredX/)**

---

## What it does

HousePredX is a Shiny web application that estimates house prices in real-time based on property features, then recommends renovation strategies ranked by return on investment.

Enter details like lot size, living area, quality ratings, and building type — the app runs your inputs through a pre-trained Random Forest model and returns:

- **Predicted sale price** based on your property's features
- **Feature importance chart** showing which factors drove the estimate
- **Renovation recommendations** tailored to your property's weak points
- **ROI analysis** quantifying the value gain from each suggested upgrade
- **Downloadable PDF report** summarising the full valuation

---

## Stack

| Layer | Technology |
|---|---|
| Language | R |
| Web framework | Shiny + shinythemes |
| ML model | Random Forest (`randomForest`, `ranger`) |
| Hyperparameter tuning | `caret` with 5-fold cross-validation |
| Visualisation | `plotly`, `ggplot2` |
| Report generation | `rmarkdown` + `pagedown` (Chrome-based PDF, no LaTeX) |
| Data wrangling | `dplyr` |
| Dataset | Ames Housing (~2,900 residential properties) |
| Containerisation | Docker (`rocker/shiny` base image) |
| Deployment | shinyapps.io |

---

## Project structure

```
.
├── app.r                         Shiny UI + server logic, download handler
├── model.r                       Feature selection, RF training, hyperparameter tuning
├── report_template.Rmd           R Markdown template rendered into the PDF report
├── House_Price_Prediction.ipynb  Exploratory data analysis notebook
├── AmesHousing.csv               Training dataset
├── rf_tuned.rds                  Serialised trained model (loaded by app at startup)
└── dockerfile                    Container configuration for self-hosted deployment
```

### How the pieces connect

```
User input (Shiny sidebar)
        │
        ▼
rf_tuned.rds  ──►  predict()  ──►  Predicted sale price
                                          │
                              Feature importance plot (plotly)
                                          │
                              Renovation rule engine (app.r:126–191)
                                    │            │
                            Recommendations    ROI chart (plotly)
                                          │
                              report_template.Rmd
                                          │
                              pagedown::chrome_print()
                                          │
                                    PDF download
```

---

## Running locally

**Prerequisites:** R ≥ 4.1, the packages listed below, and Chrome (required by `pagedown`).

```r
install.packages(c(
  "shiny", "shinythemes", "randomForest", "ranger",
  "caret", "plotly", "ggplot2", "dplyr",
  "rmarkdown", "pagedown", "scales"
))
```

Launch the app:

```bash
R -e "shiny::runApp('app.r')"
```

Then open `http://127.0.0.1:<port>` as shown in the console.

---

## Docker deployment

```bash
# Build
docker build -t housepredx .

# Run
docker run -p 3838:3838 housepredx
```

Navigate to `http://localhost:3838`.

---

## Reproducing the model

The pre-trained model (`rf_tuned.rds`) is included so the app works without retraining. To retrain from scratch:

```bash
R < model.r
```

This runs the full pipeline: loads `AmesHousing.csv`, selects features by correlation threshold (|r| > 0.4), trains a Random Forest with `caret` + `ranger` using 5-fold cross-validation, tunes `mtry` and `min.node.size`, then writes the best model to `rf_tuned.rds`.

---

## Key features in detail

**Price prediction** — The model was trained on 12 property features including living area, overall quality, year built, kitchen quality, basement condition, and building type. Feature importance is surfaced as an interactive bar chart so users understand what's driving their estimate.

**Renovation advisor** — A rule-based engine evaluates the user's inputs against thresholds (e.g. kitchen quality `Fa`, garage capacity `0`, overall quality `< 7`) and surfaces targeted recommendations. Each recommendation has an associated ROI coefficient that feeds into the revised price estimate.

**PDF report** — The download uses `pagedown::chrome_print()` to convert an `rmarkdown`-rendered HTML page to PDF via a headless Chrome instance. This avoids any LaTeX dependency and works on shinyapps.io without additional setup.

---

## Dataset

[Ames Housing Dataset](https://www.kaggle.com/datasets/prevek18/ames-housing-dataset) — residential property sales in Ames, Iowa with 80 features covering structural characteristics, quality ratings, and sale conditions. The model uses a filtered subset of 12 high-correlation features.

---

## Author

**Aditya Kumar Roy**
