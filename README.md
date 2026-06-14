# Airline Passenger Satisfaction: Recognition through Machine Learning
 
## Overview
 
This project investigates whether a customer satisfaction survey conducted by an airline company is effective in identifying satisfied passengers, and which features are most influential in determining satisfaction. The analysis combines unsupervised clustering and supervised classification techniques applied to a dataset of ~26,000 passenger responses.
 
The work was developed as a group project for the *Machine Learning* course at Università degli Studi di Milano-Bicocca (A.Y. 2024/25).
 
---
 
## Dataset
 
The [Airline Passenger Satisfaction dataset](https://www.kaggle.com/datasets/teejmahal20/airline-passenger-satisfaction) contains 25,976 observations and 25 variables, each corresponding to a passenger who evaluated their travel experience after a flight.
 
Variables are grouped into two categories:
 
- **Facts:** demographic information (age, gender, customer type) and objective flight details (travel class, flight distance, departure/arrival delays)
- **Opinions:** 14 satisfaction ratings (1–5 scale) covering in-flight wifi, online boarding, seat comfort, food and drink, cleanliness, entertainment, and more
The target variable is `Satisfaction`: *satisfied* vs *neutral or dissatisfied*.
 
---
 
## Preprocessing
 
- Removal of ID columns irrelevant to the analysis
- Zero-value ratings treated as missing values (scale is 1–5)
- Missing value imputation: median for rating features, MICE for remaining variables
- Standardisation for distance-based methods
- Outlier detection via DBSCAN; extreme observations (mainly high departure/arrival delays) were removed after confirming they did not affect the class distribution of the target
- `Arrival Delay in Minutes` dropped due to high collinearity with `Departure Delay in Minutes`
---
 
## Unsupervised Learning
 
### DBSCAN
Used for outlier detection rather than clustering. Best parameters: ε = 3.5, min points = 18. Identified extreme delay observations as noise points, which were subsequently removed.
 
### Principal Component Analysis (PCA)
Applied to the 14 rating variables to reduce dimensionality before clustering. Bartlett's test and KMO (= 0.79) confirmed suitability. **7 components** were retained (Kaiser criterion + scree plot elbow), explaining a sufficient share of cumulative variance. Components were interpreted as:
 
| Component | Interpretation |
|---|---|
| Comp1 | Inflight experience (food, seat, entertainment, cleanliness) |
| Comp2 | Digital convenience (wifi, online booking) |
| Comp3 | Service quality (on-board, baggage, inflight service) |
| Comp4 | Online boarding |
| Comp5 | Check-in service |
| Comp6 | Leg room comfort |
| Comp7 | Timeliness and accessibility |
 
### K-means (on PCA components)
Silhouette analysis suggested k = 2, but the resulting clusters mirrored the original class distribution without meaningfully separating satisfied from dissatisfied passengers.
 
### Hierarchical Clustering (on PCA components)
Agglomerative clustering with Ward linkage and Euclidean distance produced the best results (k = 2). One cluster contained 65.2% satisfied passengers; the other 74.9% neutral or dissatisfied, driven primarily by the "in-flight experience" component (cleanliness, food, seat comfort, entertainment).
 
---
 
## Supervised Learning
 
The dataset was split into train (75%) and test (25%).
 
| Model | Test Accuracy | Sensitivity | Specificity | AUC |
|---|---|---|---|---|
| **Random Forest** (80 trees) | **94.78%** | **96.36%** | **92.71%** | **0.946** |
| AdaBoost (depth=11, rounds=90) | 92.82% | 90.94% | 94.27% | 0.926 |
| KNN-9 | 91.81% | 87.81% | 94.86% | 0.914 |
| SVM (linear, C=0.5) | 89.16% | 87.17% | 90.68% | 0.889 |
 
**Random Forest** achieved the best overall performance in terms of accuracy, AUC, and computational efficiency.
 
### Key findings from feature importance (Random Forest)
- `Online boarding` is the single most important feature for predicting satisfaction
- `Gender` and `Departure Delay in Minutes` have negligible predictive power
- `Food and drink` and `Gate location` are the least impactful rating features — potential candidates for removal from the survey to reduce respondent burden
- `Class` is strongly associated with satisfaction graphically, but its importance drops in terms of classification accuracy
---
 
## Tech Stack
 
- **Language:** R
- **Key packages:** `randomForest`, `class`, `e1071`, `ada`, `factoextra`, `dbscan`, `mice`, `pROC`, `ggplot2`
---
 
*Group project — Machine Learning, A.Y. 2024/25, Università degli Studi di Milano-Bicocca*
