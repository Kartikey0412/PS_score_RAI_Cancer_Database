PS_score_RAI_Cancer_Database
==============================

In this analysis, a cohort of individuals comprising of controls vs cases who received Radioactive Iodine (RAI) for Thyroid cancer is obtained. The objective of this study was to find whether the treatment had a significant effect. But the control and case subjects are not matched based on the covariates and had significant differences before matching. In propensity_score.R, the differences in covariates before performing any matching are calculated. In an observational study propensity scores can be used to balance cases and control for covariates. this was shown in Rosenbaum and Rubin (1983) paper. In the first case I calculate propensity scores from logistic regression analysis from the probability to be in the treatment group. We use the 'Matchit' R library to create a 'Matched' dataset using the 'Nearest' method. In ps_xgboost.R xgboost trees are used to calculate the propensity scores. I implement a 5 fold cross-validation to tune the xgboost algorithm, and obtain the matched dataset again using 'Matchit' with 'Nearest' neighbor matching. With help from PI, Ivan Diaz, In ps_superLearner.R, propensity scores are estimated based on the superLearner algorithm for a try for creating better matched datasets. Multi superLearner libraries are implemented (glm, glm.interaction, randomforest, mean, glmnet and finally 10 fold cross-validation was used for fine tuning for the binomial outcome. In the end again 'Matchit' algorithm is implemented with the 'full' method to create the final matched dataset.


Project Organization
------------

    ├── LICENSE
    ├── Makefile           <- Makefile with commands like `make data` or `make train`
    ├── README.md          <- The top-level README for developers using this project.
    ├── data
    │   ├── external       <- Data from third party sources.
    │   ├── interim        <- Intermediate data that has been transformed.
    │   ├── processed      <- The final, canonical data sets for modeling.
    │   └── raw            <- The original, immutable data dump.
    │
    ├── docs               <- A default Sphinx project; see sphinx-doc.org for details
    │
    ├── models             <- Trained and serialized models, model predictions, or model summaries
    │
    ├── notebooks          <- Jupyter notebooks. Naming convention is a number (for ordering),
    │                         the creator's initials, and a short `-` delimited description, e.g.
    │                         `1.0-jqp-initial-data-exploration`.
    │
    ├── references         <- Data dictionaries, manuals, and all other explanatory materials.
    │
    ├── reports            <- Generated analysis as HTML, PDF, LaTeX, etc.
    │   └── figures        <- Generated graphics and figures to be used in reporting
    │
    ├── requirements.txt   <- The requirements file for reproducing the analysis environment, e.g.
    │                         generated with `pip freeze > requirements.txt`
    │
    ├── setup.py           <- makes project pip installable (pip install -e .) so src can be imported
    ├── src                <- Source code for use in this project.
    │   ├── __init__.py    <- Makes src a Python module
    │   │
    │   ├── data           <- Scripts to download or generate data
    │   │   └── make_dataset.py
    │   │
    │   ├── features       <- Scripts to turn raw data into features for modeling
    │   │   └── build_features.py
    │   │
    │   ├── models         <- Scripts to train models and then use trained models to make
    │   │   │                 predictions
    │   │   ├── predict_model.py
    │   │   └── train_model.py
    │   │
    │   └── visualization  <- Scripts to create exploratory and results oriented visualizations
    │       └── visualize.py
    │
    └── tox.ini            <- tox file with settings for running tox; see tox.readthedocs.io


--------

<p><small>Project based on the <a target="_blank" href="https://drivendata.github.io/cookiecutter-data-science/">cookiecutter data science project template</a>. #cookiecutterdatascience</small></p>
