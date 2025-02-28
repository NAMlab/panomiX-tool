# PanomiX Documentation
The panomiX toolbox is developed using Shiny, a web application framework for R that allows for interactive data analysis and visualization. This interactive web-based platform allows us to effortlessly analyze complex biological data across different omics layers, such as genomics, transcriptomics, proteomics, metabolomics, and phenomics.
To ensure accessibility, panomiX is hosted on Shinyapps.io, a cloud-based hosting service for Shiny applications. This allows users to access the platform from anywhere without requiring additional infrastructure. Cloud deployment eliminates the need to manage local servers, providing a smoother user experience and easier maintenance.

## Data Input

### Accepted Data Formats

PanomiX works with continuous molecular data, including:

- **RNA-seq counts**
- **Protein abundances**
- **Metabolite concentrations**
- **FTIR spectra**

Your data should be structured as a **feature matrix**, where:

- **Columns** represent biological samples (e.g., individuals or time points)
- **Rows** represent molecular features (e.g., genes, proteins, metabolites, or spectral variables)

#### Example Feature Matrix

| Feature  | Sample 1 | Sample 2 | Sample 3 | Sample 4 | Sample 5 |
|----------|---------|---------|---------|---------|---------|
| Gene 1   | 5.2     | 3.8     | 6.1     | 4.5     | 7.3     |
| Gene 2   | 2.1     | 5.7     | 3.3     | 4.9     | 6.2     |
| Gene 3   | 7.8     | 6.4     | 5.1     | 3.2     | 4.8     |
| Gene 4   | 4.3     | 5.9     | 6.5     | 7.1     | 3.7     |
| Gene 5   | 3.6     | 4.2     | 5.4     | 6.8     | 7.9     |

For regression tasks, a **continuous outcome variable (y)** is required. This can be any measurable quantitative trait.

### Uploading Data

PanomiX accepts data files in standard tabular formats such as **CSV**. Ensure that your dataset follows the structure above before uploading.

## Data Preprocessing and Recommendations

PanomiX can handle large datasets but benefits from preprocessing steps to improve performance:

- **Filter low-variability features** to remove those with minimal variation across samples.
- **Exclude low-count features** in sequencing data (e.g., RNA-seq), dropping consistently low-count features.

Preprocessing helps reduce dataset size, minimize overfitting, and optimize the efficiency of machine learning models like **XGBoost** which we are using for model prediction.

## Data Harmonization and Scaling

### Transcriptomics Data Normalization

PanomiX uses a DESeq2-based approach to normalize raw transcriptomic data to ensure consistency. Simply drag and drop your raw data and download the normalized counts.

### FTIR Data Processing

FTIR spectral data is often affected by background noise. PanomiX applies:

1. **Baseline correction** using the baseline R library.
2. **Savitzky-Golay smoothing** using the signal R library to reduce high-frequency noise while preserving spectral features.

### Recommendations

Ensure expression/absorbance values are combined into a single matrix before processing (as mentioned above columns will represent biological samples and rows represent molecular features).

## Component of Variance Analysis

Understanding variance in data is crucial for meaningful analysis. PanomiX provides:

- **PCA (Principal Component Analysis)** to examine variance patterns across omics datasets. Before performing PCA, PanomiX standardizes datasets to a unit sum of squares, ensuring uniformity across different omics data types.
- **ANOVA-based variance estimation** to identify major variance components.

PanomiX supports:

- **Separate PCA** for each omics dataset to assess individual variance.
- **Integrated PCA** for multi-omics datasets to uncover global patterns.

### Recommendations

- **Use normalized data** before PCA (although PanomiX handles scaling automatically).
- **Provide metadata** with at least:
  - An **ID column** matching omics data.
  - **Experimental conditions** (e.g., condition1 and condition2).
  
#### Example Metadata

| ID  | condition1 | condition2 |
|----------|------------|------------|
| Gene 1   | TH1.1     | Control     |
| Gene 2   | TH1.2     | Control     |
| Gene 3   | TH1.3     | Control     |
| Gene 4   | TH2.1     | Treatment     |
| Gene 5   | TH2.3     | Treatment     |

## Multi-Omics Prediction with XGBoost

PanomiX leverages **XGBoost** for high-dimensional multi-omics data analysis. Unlike traditional multivariate dimension reduction techniques, XGBoost focuses on supervised learning, using decision trees as base models, and applying gradient boosting to optimize performance, making it highly suitable for finding relationships between complex datasets. PanomiX uses the following steps to make the mode training to minimize loss and improve prediction.

Data Splitting Options

Users can split datasets via:

- **Random splitting** (train/test ratio adjustable via a slider in the tool).
- **Replicate-based splitting** (splits train/test by grouping replicates together so that train and test sets contain all the replicates for maintaining consistency and no information lost).

### Hyperparameter Tuning

PanomiX automates tuning using the **caret package**. Key hyperparameters:

| Hyperparameter          | Description | Recommended Range |
|-------------------------|-------------|-------------------|
| max_depth              | Tree depth | 3–6 (higher values risk overfitting) |
| eta                    | Learning rate | 0.1–0.5 (lower rates improve accuracy) |
| gamma                  | Minimum loss reduction for split | 0.1–0.6 (higher values prevent minor splits) |
| n_estimators           | Number of boosting rounds | 50–100 |
| subsample              | Fraction of samples per tree | 0.5–0.8 (reduces overfitting) |
| colsample_bytree       | Fraction of features per tree | 0.5–1 |
| alpha, lambda          | Regularization parameters | L1 = 0, L2 = 1 (default) |
| min_child_weight       | Minimum sum of instance weights in a node | 1–5 (avoids overly specific splits) |


### Feature Importance & SHAP Analysis

- PanomiX ranks predictors using **feature importance scores**.
- **SHAP values** help interpret feature contributions.
- SHAP values are visualized using a **beeswarm plot**, offering clear insights into feature importance and their effect on predictions.
- **Boruta-SHAP algorithm** is also available for alternative feature selection.

### Recommendations

- **For datasets with biological replicates, replicate-based splitting is recommended to maintain better splitting of train and test data.**
- **Use cross-validation (CV) for generalization and with large datasets**; LOOCV is better for small datasets.
- **Analyze SHAP values** to interpret key predictors.

## Multi-Omics Interaction Analysis

PanomiX can evaluate interactions between predicted and known features. Users can input a list of known relevant features to assess their contribution to model predictions.

### Constraint-Based Modeling

- **Random Constraints:** Upload a table with two columns:
  - "feature"
  - "final\_association" (all values set to **0** for unknown relationships).
- **Monotonic Constraints:** Upload a table with "feature" and "final\_association" values:
  - **1** for a known positive relationship.
  - **-1** for a known negative relationship.
  - **0** for unknown relationships.

PanomiX incorporates constraints to align model predictions with biological knowledge.

#### Monotonic Constraints Example Table

| Feature | Final_Association |
|---------|------------------|
| Gene 1  | 1                |
| Gene 2  | 0                |
| Gene 3  | -1               |
| Gene 4  | 1                |
| Gene 5  | -1               |

### Recommendations

- **Use monotonic constraints** when relationships between features and outcomes are known.
- **Leverage SHAP-based feature interactions** for deeper insights into biological processes.

---
For detailed examples and step-by-step tutorials follow our article, and visit our [panomiX](https://szymanskilab.shinyapps.io/panomiX/).

