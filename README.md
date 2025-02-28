# Forecasting_opinionPooling

# README: Forecasting Opinion Pooling - generated with chatGPT

## Overview
This project processes and analyzes forecasting data using various statistical and data manipulation techniques in R. The workflow includes data cleaning, aggregation, and accuracy evaluation of forecasting methods.

## Dependencies
The following R libraries are required to run the scripts:
- `dplyr` – For data manipulation
- `stringr` – For string operations
- `lubridate` – For date-time handling
- `psych` – For statistical functions like geometric mean

## Setup
### Set Working Directory
Ensure that your working directory is correctly set to the location where your data files are stored:
```r
```

## Data Files
The project uses three key datasets:
1. `rct-a-questions-answers.csv`: Contains questions, answers, and metadata.
2. `rct-a-daily-forecasts.csv`: Stores daily forecasts.
3. `rct-a-prediction-sets.csv`: Contains individual-level forecasts.

## Function Definitions
A custom function is defined to calculate the geometric mean of odds:
```r
get_GeometricMeanOfOdds <- function(probVec){
  complement <- 1 - probVec
  oddsVec <- complement / probVec
  GeometricMeanOfOdds <- exp(mean(log(oddsVec)))
  return (GeometricMeanOfOdds)
}
```

## Data Processing Workflow
### Step 1: Load and Filter Data
- The datasets are loaded and examined using `head()`, `str()`, and `summary()`.
- Questions and answers are filtered to include only relevant entries present across all datasets.
- Predictions related to practice questions are removed.

### Step 2: Aggregate Forecasts
For each forecasting day, individual-level forecasts are aggregated using:
- Mean forecast
- Median forecast
- Trimmed mean (10%)
- Geometric mean
- Geometric mean of odds

The aggregation is done using the most recent forecast per forecaster on each day.

### Step 3: Accuracy Calculation
The accuracy of forecasting methods is evaluated by comparing aggregated forecasts to resolved outcomes. This is done using:
- Simple accuracy comparison
- Brier score calculation for each method
- Linear pooling using a weighting factor based on individual Brier scores

### Step 4: Adjusted Forecasting
A scaling factor is applied to adjust forecasts based on question difficulty, aiming to optimize forecast accuracy.

## Output
- `AggForecasts_v1.csv`: Contains initial aggregated forecasts.
- `AggForecasts_v3.csv`: Includes linear pooling adjustments.
- `AggForecasts_withLP.csv`: Final dataset with adjusted forecasts.

## Key Findings
- The trimmed mean forecast provides a good balance of accuracy.
- Linear pooling enhances forecast accuracy by weighting more reliable forecasters.
- The geometric mean of odds provides an alternative probability-based aggregation method.

## Running the Script
Ensure that all dependencies are installed and run the script sequentially. The script processes the data and outputs refined forecasting accuracy results.

## Contact
For queries or issues, please contact TamsinELee.com

---
This README provides a structured guide to understanding and executing the forecasting opinion pooling analysis in R.

