## Multi-omics Integration Framework- panomiX
A multi-omics shiny app likely refers to a web application developed using the Shiny framework in the R programming language for the analysis and visualization of multi-omics data. 

Let's break down: Multi-omics refers to the integration and analysis of various biological data types, such as genomics, transcriptomics, proteomics, metabolomics, and other -omics data. Combining these datasets can provide a more comprehensive understanding of biological systems.

Based on this, a "multi-omics shiny app" would likely be a user-friendly interface for exploring, analyzing, and visualizing integrated multi-omics data. Users might be able to upload their datasets, perform various analyses, and visualize results through interactive plots and dashboards. The app could potentially include features such as Data Harmonization, Varaince Estimation, Multi-omics Prediction, Multi-omics Interaction, and other tools commonly used in the analysis of multi-omics data.

This application enables users to build a predictive model using the XGBoost package and the caret package. It provides functionality to fine-tune the XGBoost model and select the optimal configuration through cross-validation, specifically employing a cross-validation approach. Information about XGBoost <a href="https://xgboost.readthedocs.io/en/latest//">here</a>

# PanomiX Documentation

## Data Input

### Accepted Data Formats
PanomiX works with continuous molecular data, including:
- **Normalized RNA-seq counts**
- **Protein abundances**
- **Metabolite concentrations**
- **FTIR spectra**

Your data should be structured as a **feature matrix**, where:
- **Columns** represent biological samples (e.g., individuals or time points)
- **Rows** represent molecular features (e.g., genes, proteins, metabolites, or spectral variables)

For regression tasks, a **continuous outcome variable (y)** is required. This can be any measurable quantitative trait.

### Uploading Data
PanomiX accepts data files in standard tabular formats such as **CSV or TSV**. Ensure that your dataset follows the structure above before uploading.

## Data Preprocessing and Recommendations
PanomiX can handle large datasets but benefits from preprocessing steps to improve performance:
- **Filter low-variability features** to remove those with minimal variation across samples.
- **Exclude low-count features** in sequencing data (e.g., RNA-seq), dropping consistently low-count features.
- **Remove near-zero variance features** that do not significantly contribute to the model.

Preprocessing helps reduce dataset size, minimize overfitting, and optimize the efficiency of machine learning models like **XGBoost**.

## Data Harmonization and Scaling

### Transcriptomics Data Normalization
PanomiX normalizes transcriptomic data to ensure consistency:
1. **Log transformation** stabilizes variance.
2. **Infinite values are filtered out** for numerical stability.
3. **Gene expression is centered** around its mean to reduce biases.
4. **Median scaling** corrects batch effects and extreme variations.
5. **Log-transformed values are reverted** to preserve biological relevance.

### FTIR Data Processing
FTIR spectral data is often affected by background noise. PanomiX applies:
1. **Baseline correction** using the baseline R library.
2. **Savitzky-Golay smoothing** using the signal R library to reduce high-frequency noise while preserving spectral features.

### Recommendations
- **Transcriptomics:** Upload a count matrix with raw counts or pre-normalized values (TPM/CPM).
- **FTIR Spectra:** Ensure absorbance values are compiled into a single matrix before processing.

## Component of Variance Analysis
Understanding variance in the data is critical for analysis. PanomiX offers:
- **PCA (Principal Component Analysis)** to examine variance patterns across omics datasets.
- **ANOVA-based variance estimation** to identify major variance components.

Before PCA, datasets are standardized to a **unit sum of squares** for uniformity. PanomiX supports:
- **Separate PCA** for each omics dataset to assess individual variance.
- **Integrated PCA** for multi-omics datasets to uncover global patterns.

### Recommendations
- **Normalize data** before PCA (PanomiX handles scaling automatically).
- **Provide metadata** with at least:
  - An **ID column** matching omics data.
  - **Experimental conditions** (e.g., condition1 and condition2).

## Multi-Omics Prediction with XGBoost
PanomiX leverages **XGBoost** for high-dimensional multi-omics data analysis.

### Data Splitting Options
Users can split datasets via:
- **Random splitting** (train/test ratio adjustable via slider).
- **Replicate-based splitting** (keeps replicates together for consistency).

### Hyperparameter Tuning
PanomiX automates tuning using the **caret package**. Key hyperparameters:
- **Tree depth (max_depth):** 3–6 (higher values risk overfitting).
- **Learning rate (eta):** 0.1–0.5 (lower rates improve accuracy).
- **Gamma (gamma):** 0.1–0.6 (higher values prevent minor splits).
- **Boosting rounds (n_estimators):** 50–1000.
- **Subsample (subsample):** 0.5–0.8 (reduces overfitting).
- **Column subsampling (colsample_bytree):** 0.5–0.8.
- **Regularization (alpha, lambda):** Default L1 = 0, L2 = 1.
- **Min child weight (min_child_weight):** 1–5 (avoids overly specific splits).

### Feature Importance & SHAP Analysis
- PanomiX ranks predictors using **feature importance scores**.
- **SHAP values** help interpret feature contributions.
- A **beeswarm plot** visualizes SHAP values for better insights.
- **Boruta-SHAP algorithm** is available for alternative feature selection.

### Recommendations
- **Use replicate-based splitting** for biological replicates.
- **Tune rounds and tree depth** in the cloud version.
- **Use cross-validation (CV) for generalization**; LOOCV is better for small datasets.
- **Analyze SHAP values** to interpret key predictors.

## Multi-Omics Interaction Analysis
PanomiX evaluates interactions between predicted and known features. Users can:
- **Provide a list of relevant known features** for model assessment.
- **Assess feature relationships** through SHAP values.

### Constraint-Based Modeling
- **Random Constraints:** Upload a table with two columns:
  - "feature"
  - "final_association" (all values set to **0** for unknown relationships).
- **Monotonic Constraints:** Upload a table with "feature" and "final_association" values:
  - **1** for a known positive relationship.
  - **-1** for a known negative relationship.
  - **0** for unknown relationships.

PanomiX incorporates constraints to align model predictions with biological knowledge.

### Recommendations
- **Provide metadata** for experimental conditions.
- **Use monotonic constraints** when relationships between features and outcomes are known.
- **Leverage SHAP-based feature interactions** for deeper insights into biological processes.

---
For detailed examples and step-by-step tutorials, visit our [GitHub repository](https://github.com/your-repo-link).


