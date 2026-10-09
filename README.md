# Predicting Dry Eye Disease Using Data Analytics

Final project for **BAN279 – Data Modeling for Healthcare** (Hofstra University, Spring 2025). Grade: A-.

## Overview
Can dry eye disease (DED) be predicted from lifestyle and clinical factors? This project builds and compares three classification models on a clinical/lifestyle dataset to answer that question.

## Data
- Clinical/lifestyle dataset: demographics, sleep (duration, quality, disorders), screen time, caffeine/alcohol/smoking, blood pressure, physical activity, and eye symptoms
- Target: `Dry_Eye_Disease` (binary)

## Methods
- **Preprocessing** (tidyverse): encoded categorical variables, split blood pressure into systolic/diastolic, normalized continuous features, 70/30 stratified train-test split
- **Models**: logistic regression (`glm`), decision tree (`rpart`), random forest (`randomForest`, 100 trees)
- **Evaluation**: accuracy, precision, recall, F1 (caret `confusionMatrix`), ROC curves and AUC (pROC)
- **Visualization**: ggplot2 (symptom prevalence, model comparison), corrplot (correlation heatmap)

## Contents
- `Group_7_final_project.R` — full analysis script (data prep, modeling, evaluation, visualization)

## Tools
R · tidyverse · caret · rpart · randomForest · pROC · ggplot2 · corrplot
