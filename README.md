# Musical Valence Comparative Study

### Mathematical Modeling FCO - BSM AS 3A [Group 4]

## Project Overview
Evaluating the model performances of various mathematical modeling namely Lasso, Elastic Net, and XGBoost, this study made use of a cross-sectional dataset of Spotify from October 2022 (Pandya, 2022). It aims to compare their predictive performances towards Valence (emotional happiness) from analyzing the behavior of all other available musical features. 

## Install Packages Needed By
```R
install.packages("tidyverse")
install.packages("glmnet")
install.packages("xgboost")
```
## Run With
```
Rscript main.R
```

# Output Files

### Plots (`Outputs/Plots/`)
| File | Description |
|:---|:---|
| `elnet_coef_new.png` | Elastic Net Coefficients of Musical Features |
| `elnet_predicactual_new.png` | Predicted vs Actual Valence (y=x) of Elastic Net |
| `lasso_predicactual_new.png`| Predicted vs Actual Valence (y=x) of Lasso regression |
| `lasso_selection_new.png`| Coefficients of Musical Features set by Lasso|
| `xgboost_feature_all.png`| Feature Importance (Gains) of XGBoost with all features used |
| `xgboost_predicactual_all.png`| Predicted vs Actual Valence (y=x) of XGboost using all musical features |
| `xgboost_predicactual_new.png`| Predicted vs Actual Valence (y=x) of XGboost using features selected by Lasso |


### Plots (`Outputs/Table/`)
| File | Description |
|:---|:---|
| `spotify_processed_fr_valence.csv` | The cleaned dataset with no duplications |

## Folder Path
&rarr; `data` - original dataset (.csv)

&rarr;  `Outputs/Plots/` - scatterplots and bar graphs (.png)

&rarr; `src` - compilation of codes (.R)

