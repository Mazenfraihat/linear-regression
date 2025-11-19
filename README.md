# Horsepower Prediction Using Linear Regression (R)

This project analyzes and predicts automobile **horsepower** using multiple numerical attributes.  
It demonstrates how linear regression models can be applied to real-world data for performance prediction and feature comparison.

---

## 🔍 Overview
The goal of this project is to identify which technical features best explain variation in horsepower.  
Several models are compared:
- Simple linear regression (one predictor)
- Multiple linear regression (two or more predictors)
- Quadratic and interaction terms for model improvement

Model performance is evaluated using:
- **R²** – goodness of fit  
- **AIC** – model comparison and parsimony

---

## 📊 Data
The dataset (`imports-85.csv`) contains specifications for various automobiles, such as:
- Engine size  
- Price  
- Bore and wheelbase  
- Horsepower (response variable)

---

## 🧮 How to Run
Run the analysis in R or RStudio:

```r
source("Linear_Regression.R")
