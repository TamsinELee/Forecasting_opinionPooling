# 12 October 2024
# TEL

# Load libraries
library(dplyr)     # For data manipulation
library(stringr)   # For string operations
library(lubridate) # For date-time manipulation
library(psych)     # For statistical functions like geometric mean

###################################################
# Set working directory to the location where files are stored
setwd("~/dataverse_files/Forecasting_opinionPooling/")

###################################################
# Function files
# Define a custom function to calculate the geometric mean of odds
get_GeometricMeanOfOdds <- function(probVec){
  complement          <- 1 - probVec
  oddsVec             <- complement / probVec
  GeometricMeanOfOdds <- exp(mean(log(oddsVec)))
  return (GeometricMeanOfOdds)
}
###################################################

# Load and get to know the data. 
# This dataset captures questions and answers used in a forecasting system, 
# alongside their associated metadata, such as timestamps, 
# user IDs, and descriptions. Each question is uniquely identified and follows a 
# lifecycle from creation to resolution. Answers to these questions are also 
# tracked, with additional attributes like timestamps for 
# resolution.
# Load question answers
# A list of all questions (aka. IFP's), their answers, and associated metadata 
# such as dates and descriptions.
questionsAnswers      <- read.csv("rct-a-questions-answers.csv")
head(questionsAnswers)     # Display the first few rows of the dataset
str(questionsAnswers)      # Display the structure of the dataset
summary(questionsAnswers)  # Show summary statistics for the dataset

# Group by question ID and name, then count the number of answers per question
NAnswerPerQ           <- questionsAnswers %>% dplyr::group_by(discover.question.id, question.name) %>% 
                                              dplyr::summarise(n = n())

# Load forecasts
# For each performer forecasting method, this report provides the last forecast 
# for each scoring day (i.e., the last forecast before 2:01pm ET).
dailyForecasts        <- read.csv("rct-a-daily-forecasts.csv")
head(dailyForecasts)
str(dailyForecasts)

# Load predictions
# A list of all individual-level forecasts (aka. Prediction sets) created 
# in the system. THIS IS THE RELEVANT ONE FOR STEP 2.
predictionSets        <- read.csv("rct-a-prediction-sets.csv")
head(predictionSets)
str(predictionSets)

# Immediate filtering based on questions that are common in all three datasets. 
QA.question.id.vec    <- unique(questionsAnswers$discover.question.id)
DF.question.id.vec    <- unique(dailyForecasts$discover.question.id)
PS.question.id.vec    <- unique(predictionSets$discover.question.id)
# 187 question ids Forecast data so filter unnecessary question ids

# Questions and answers

# Filter out unnecessary questions that don't appear in all datasets
questionsAnswers2     <- questionsAnswers %>% dplyr::filter(discover.question.id %in% DF.question.id.vec)
QA2.question.id.vec   <- unique(questionsAnswers2$discover.question.id)

# Order the filtered questions and answers by question ID
testQA                <- questionsAnswers2 %>% dplyr::group_by(discover.question.id, question.name)
testQA                <- testQA[order(testQA$discover.question.id),]
head(testQA)
DF_QA                 <- questionsAnswers2

# Predictions
# Filter the prediction sets based on the questions available in dailyForecasts
predictionSets2       <- predictionSets %>% dplyr::filter(discover.question.id %in% DF.question.id.vec)
PS2.question.id.vec   <- unique(predictionSets2$discover.question.id)

# Check what the excluded questions are. 
# Identify excluded questions, i.e., those present in predictionSets but not in the filtered dataset.
excQuestions          <- setdiff(PS.question.id.vec, PS2.question.id.vec)
predictionSetsExc     <- predictionSets %>% dplyr::filter(discover.question.id %in% excQuestions)

# Looks like excluded questions are practice ones. Check no practice questions in the remaining df. 
# Check whether the excluded questions are "Practice Questions"
idxPracticeQ          <- str_starts(predictionSets$question.name, "Practice Question:", negate = FALSE)
temp2                 <- predictionSets[idxPracticeQ, ]
idxPracticeQ2         <- str_starts(predictionSetsExc$question.name, "Practice Question:", negate = FALSE)
temp3                 <- predictionSetsExc[idxPracticeQ2,]

# Compare the practice questions to ensure no essential questions were removed
setdiff(sort(temp2$prediction.set.id), sort(temp3$prediction.set.id))
# Check: temp2 and temp3 are the same, so only Practice questions were removed. 
DF_PRED               <- predictionSets2 # 746637
test_prediction       <- DF_PRED %>% dplyr::group_by(discover.question.id, question.name)
test_prediction       <- test_prediction[order(test_prediction$discover.question.id),]
head(test_prediction)

###################################################

# Step 2: Take all individual forecasts, and aggregate the individual-level forecasts in the following
# five ways. The result should be five data points for each question-day pair, using the most
# recent forecast from each forecaster on that day (i.e. for each day that the question had any
#                                                  forecasts, each of the following aggregates)

###################################################
# Investigate one question (with ID 177) to understand the forecast data structure
temp_pred             <- DF_PRED %>% dplyr:: filter(discover.question.id == 177) 
# Convert the timestamp from character to date-time format
converted_date        <- ymd_hms(temp_pred$updated.at, tz = "UTC")
head(converted_date)
head(temp_pred$updated.at)

# How many responses per membership.guid
test1 <- temp_pred %>% dplyr::group_by(discover.question.id, membership.guid) %>% 
   dplyr::summarise(n = n())
# How many responses per answer and per membership.guid
test2 <- temp_pred %>% dplyr::group_by(discover.question.id,
                                      answer.name,
                                      membership.guid) %>% 
                          dplyr::summarise(n = n())
# How many responses per answer and per membership.guid at the most recent updated time.
test3 <- temp_pred %>% dplyr::group_by(discover.question.id,
                                       answer.name,
                                       membership.guid) %>% 
  dplyr::summarise(maxtime = max(updated.at))
# Dig deeper. For a given member and answer:
test4 <- temp_pred %>% dplyr::filter(membership.guid == "00790045df5d52c2217ed7907f8f58fa4fdb08b8",
                                        answer.name == "Between 2,000 and 2,500, inclusive")
length(unique(test4$updated.at))
test5 <- test4 %>% dplyr::group_by(discover.question.id,
                              answer.name,
                              membership.guid) %>% 
         dplyr::summarise(updated.at = max(updated.at))
# Add forecast
test6 <- left_join(test5, test4 %>% dplyr::select(membership.guid, answer.name, updated.at, forecasted.probability))

###################################################
# Let's do the real thing!!
###################################################

# Remove predictions made after the time at which the answer to this question was known.
DF_PRED               <- DF_PRED %>% dplyr::filter(made.after.correctness.known == FALSE)
# 742643
# Convert the timestamp to date-time format again after filtering
converted_date        <- ymd_hms(DF_PRED$updated.at, tz = "UTC")
head(converted_date)
head(DF_PRED$updated.at)
DF_PRED$updated.at    <- converted_date
str(DF_PRED)
# Find most recent prediction.set.id
pred.set.id.DF <- DF_PRED %>% dplyr::select(prediction.set.id, 
                          discover.question.id, 
                          membership.guid, 
                          updated.at) %>% 
  dplyr::group_by(discover.question.id,
                  membership.guid) %>% 
  dplyr::arrange(desc(updated.at)) %>% slice(1) 
pred.set.id.vec <- unique(pred.set.id.DF$prediction.set.id)


PredRecent <- DF_PRED %>% dplyr::filter(prediction.set.id %in% pred.set.id.vec)
# Group by question, answer, and user, and select the most recent forecast time
# PredRecent <- DF_PRED %>% dplyr::select(discover.question.id, 
#                                        answer.name, 
#                                        membership.guid, 
#                                        updated.at, 
#                                        forecasted.probability,
#                                        answer.resolved.probability) %>% 
#                           dplyr::group_by(discover.question.id,
#                                        answer.name,
#                                        membership.guid) %>% 
#                           dplyr::arrange(desc(updated.at)) %>% slice(1) 
#                      #slice_max(order_by = updated.at, n = 1) #%>%dplyr::summarise(updated.at = max(updated.at))

# Add forecasted probability to each user's latest answer.
# Note - this will have a warning because 'old' DF_PRED entries are ignored. 
# PredRecent <- left_join(Recent, DF_PRED %>% dplyr::select(discover.question.id,
#                                                     membership.guid, 
#                                                     answer.name, 
#                                                     updated.at, 
#                                                     forecasted.probability, 
#                                                     starting.probability, 
#                                                     final.probability,
#                                                     answer.resolved.probability))
# Check there is only one resolution for each Q-A pair. 
check <- DF_PRED %>% dplyr::group_by(discover.question.id, answer.name) %>% 
  dplyr::summarise(n.resolutions = length(unique(answer.resolved.probability)))
max(check$n.resolutions)

# Reduce PredRecent to only select required columns. 
PredRecent_trim <- PredRecent %>% dplyr::select(prediction.set.id,
                                                membership.guid,
                                                discover.question.id,
                                                question.name, 
                                                answer.name,
                                                forecasted.probability,
                                                answer.resolved.probability)
NanswersDF      <- DF_QA %>% dplyr::group_by(discover.question.id) %>%
                                dplyr::summarise(n.answers = n())
discover.question.ID.01  <- NanswersDF$discover.question.id[which(NanswersDF$n.answers == 1)]
PredRecent_01            <- PredRecent_trim %>% dplyr::filter(discover.question.id %in% discover.question.ID.01)
PredRecentNo             <- PredRecent_01
PredRecentNo$answer.name <- "No" 
PredRecentNo$forecasted.probability <- 1 - PredRecent_01$forecasted.probability
PredRecentNo$answer.resolved.probability <- 1 - PredRecent_01$answer.resolved.probability
PredRecent_trim <- rbind(PredRecent_trim, PredRecentNo)

check <- PredRecent_trim %>% dplyr::filter(discover.question.id %in% discover.question.ID.01)

# Aggregate forecasts by various methods, including mean, median, and geometric mean
aggForecasts <- PredRecent_trim %>% dplyr::group_by(discover.question.id, answer.name) %>%
  dplyr::summarise(mean.forecast       = mean(forecasted.probability),
                   median.forecast     = median(forecasted.probability), 
                   trimmed.forecast    = mean(forecasted.probability, trim = 0.1),
                   geometric.forecast  = exp(mean(log(forecasted.probability))), # psych::geometric.mean(forecasted.probability),#
                   GeometricMeanOfOdds = get_GeometricMeanOfOdds(forecasted.probability),
                   GeometricMeanOfOdds_prob = 100 / get_GeometricMeanOfOdds(forecasted.probability),#psych.forecast = psych::geometric.mean(forecasted.probability),
                   resolution          = max(answer.resolved.probability), # Add resolution for accuracy calcs.
                   n.forecasters       = length(unique(membership.guid)))
# Number of questions
length(unique(aggForecasts$discover.question.id))

# Merge aggregated forecasts with additional metadata from questionsAnswers dataset.
AGGFORECASTS <- left_join(aggForecasts, DF_QA %>% dplyr::select(discover.question.id,
                                                                question.name,
                                                                discover.answer.id,
                                                                answer.name, 
                                                                Country...Primary,
                                                                Domain))

# Save the aggregated forecast data to a CSV file
write.csv(AGGFORECASTS, "AggForecasts_v1.csv", row.names = FALSE)

###
# accuracy
# Step 3: Now, calculate how accurate the aggregate forecast from each aggregation method
# was, comparing the aggregated forecast to the resolved outcome of each question. Include a
# table with the results and a paragraph describing why certain methods were more accurate than
# others.
question.id.vec <- unique(AGGFORECASTS$discover.question.id)
Nquestions      <- length(question.id.vec)
accuracyVec     <- matrix(0, nrow = Nquestions, ncol = 1)
simpleMat       <- data.frame(matrix(0, nrow = Nquestions, ncol = 6))
colnames(simpleMat) <- c("discoverQuestionID", "mean", "median", "trimmed", "geometric", "geometricOdds")
simpleMat$discoverQuestionID <- question.id.vec
for (q1 in 1:Nquestions) {
  thisData <- AGGFORECASTS %>% dplyr::filter(discover.question.id == question.id.vec[q1])
  if (nrow(thisData) > 1){
    maxMeanForecastIdx      <- which(thisData$mean.forecast == max(thisData$mean.forecast))
    maxMedianForecastIdx    <- which(thisData$median.forecast == max(thisData$median.forecast))
    maxTrimmedForecastIdx   <- which(thisData$trimmed.forecast == max(thisData$trimmed.forecast))
    maxGeometricForecastIdx <- which(thisData$geometric.forecast == max(thisData$geometric.forecast))
    resolutionIdx           <- which(thisData$resolution == 1)
    if (maxMeanForecastIdx == resolutionIdx){simpleMat$mean[q1] <- 1}
    if (resolutionIdx %in% maxMedianForecastIdx){simpleMat$median[q1] <- 1} # rough for now - bias in favour of accuracy
    if (maxTrimmedForecastIdx == resolutionIdx){simpleMat$trimmed[q1] <- 1}
    #if (maxGeometricForecastIdx == resolutionIdx){simpleMat$geometric[q1] <- 1}
    } else if (nrow(thisData) == 1){
      if (thisData$resolution[1] == 1) {
        if (thisData$mean.forecast > 0.5){simpleMat$mean[q1] <- 1}
        if (thisData$median.forecast > 0.5){simpleMat$median[q1] <- 1}
        if (thisData$trimmed.forecast > 0.5){simpleMat$trimmed[q1] <- 1}
      #  if (thisData$geometric.forecast > 0.5){simpleMat$geometric[q1] <- 1}
      } else if (thisData$resolution == 0){
        if (thisData$mean.forecast < 0.5){simpleMat$mean[q1] <- 1}
        if (thisData$median.forecast < 0.5){simpleMat$median[q1] <- 1}
        if (thisData$trimmed.forecast < 0.5){simpleMat$trimmed[q1] <- 1}
       # if (thisData$geometric.forecast < 0.5){simpleMat$geometric[q1] <- 1}
      }
  }
}

colSums(simpleMat)
BrierScoreMat2 <- AGGFORECASTS %>%
  dplyr::group_by(discover.question.id) %>%
  dplyr::summarise(
    mean          = sum((mean.forecast - resolution)^2), #
    median        = sum((median.forecast - resolution)^2),
    trimmed       = sum((trimmed.forecast - resolution)^2),
    geometric     = sum((geometric.forecast - resolution)^2),
    geometricOdds_prob = sum((GeometricMeanOfOdds_prob - resolution)^2),
    geometricOdds = sum((GeometricMeanOfOdds- resolution)^2)
  ) %>%
  ungroup()  # Ungroup after summarizing

colMeans(BrierScoreMat2)


BS_perGuidperQ <- PredRecent %>%
  dplyr::group_by(membership.guid, discover.question.id) %>%
  dplyr::summarise(
    BS            = sum((forecasted.probability - answer.resolved.probability)^2)
    ) 

BS_perGuid     <- BS_perGuidperQ %>%
  dplyr::group_by(membership.guid) %>%
  dplyr::summarise(
    meanBS = mean(BS), n.questions.responded.to = n())
BS_perGuid$BStrans <- 1 - BS_perGuid$meanBS / 2

test <- PredRecent %>% dplyr::filter(discover.question.id == 361)
length(unique(test$prediction.set.id))

# Linear pooling

PredRecent_trim <- left_join(PredRecent_trim, BS_perGuid)

PredRecent_trim$weighted.forecasted.probability <- PredRecent_trim$forecasted.probability * PredRecent_trim$BStrans

aggMineDF <- PredRecent_trim %>% dplyr::group_by(discover.question.id, answer.name) %>%
                          dplyr::summarise(aggMine = sum(weighted.forecasted.probability))

tempTotal <- aggMineDF %>% dplyr::group_by(discover.question.id) %>%
  dplyr::summarise(questionTotal = sum(aggMine))

aggMineDF <- left_join(aggMineDF, tempTotal)
aggMineDF$aggMineScaled <- aggMineDF$aggMine / aggMineDF$questionTotal

LinearPoolingDF <- aggMineDF %>% dplyr::select(discover.question.id, answer.name, aggMineScaled)


AGGFORECASTS_withLP <- left_join(AGGFORECASTS, LinearPoolingDF)
which(AGGFORECASTS_withLP$answer.name == "No")
discover.question.ID.01

for (q1 in 1:length(discover.question.ID.01)){
  replaceRow <- which(AGGFORECASTS_withLP$discover.question.id == discover.question.ID.01[q1] & 
                        AGGFORECASTS_withLP$answer.name == "No")
  takeFrom   <- which(AGGFORECASTS_withLP$discover.question.id == discover.question.ID.01[q1] & 
                        AGGFORECASTS_withLP$answer.name == "Yes")
  AGGFORECASTS_withLP$Domain[replaceRow] <- AGGFORECASTS_withLP$Domain[takeFrom]
  AGGFORECASTS_withLP$question.name[replaceRow] <- AGGFORECASTS_withLP$question.name[takeFrom]
  AGGFORECASTS_withLP$Country...Primary[replaceRow] <- AGGFORECASTS_withLP$Country...Primary[takeFrom]
}


# Use dplyr to replace "No" values based on corresponding "Yes" values in one step
# AGGFORECASTS_withLP <- AGGFORECASTS_withLP %>%
#   group_by(discover.question.id) %>%
#   mutate(
#     Domain = if_else(answer.name == "No", Domain[answer.name == "Yes"], Domain),
#     question.name = if_else(answer.name == "No", question.name[answer.name == "Yes"], question.name),
#     Country...Primary = if_else(answer.name == "No", Country...Primary[answer.name == "Yes"], Country...Primary)
#   ) 


# Save the aggregated forecast data to a CSV file
write.csv(AGGFORECASTS_withLP, "AggForecasts_v3.csv", row.names = FALSE)

####

BrierScoreMat2_LP <- AGGFORECASTS_withLP %>%
  dplyr::group_by(discover.question.id) %>%
  dplyr::summarise(
    LP            = sum((aggMineScaled - resolution)^2), #
    mean          = sum((mean.forecast - resolution)^2), #
    median        = sum((median.forecast - resolution)^2),
    trimmed       = sum((trimmed.forecast - resolution)^2),
    geometric     = sum((geometric.forecast - resolution)^2),
    geometricOdds_prob = sum((GeometricMeanOfOdds_prob - resolution)^2),
    geometricOdds = sum((GeometricMeanOfOdds- resolution)^2)
  ) %>%
  ungroup()  # Ungroup after summarizing

colMeans(BrierScoreMat2_LP)

####

DF_QA_trimmed <- DF_QA %>% dplyr::select(discover.question.id,
                                         creator.id, 
                                         IFP.Generation.Method,
                                         Region,
                                         Domain,
                                         Deaths)
DF_QA_trimmed <- distinct(DF_QA_trimmed)
DF_QA_trimmed <- left_join(DF_QA_trimmed, BrierScoreMat2_LP %>% 
                             dplyr::select(discover.question.id, trimmed))

compareRegion <- DF_QA_trimmed %>% group_by(Region) %>% dplyr::summarise(BS = mean(trimmed))

compareIFP.Generation.Method <- DF_QA_trimmed %>% group_by(IFP.Generation.Method) %>% 
  dplyr::summarise(BS = mean(trimmed))

compareDomain <- DF_QA_trimmed %>% group_by(Domain) %>% 
  dplyr::summarise(BS = mean(trimmed))

compareDeaths <- DF_QA_trimmed %>% group_by(Deaths) %>% 
  dplyr::summarise(BS = mean(trimmed))

# Calculate the average Brier score for different classes of questions. 
# Rescale the Brier score so it's between 0 and 1 (not 0 and 2), with 0 being perfectly forecasted.
compareX <- DF_QA_trimmed %>% group_by(Region, IFP.Generation.Method, Domain, Deaths) %>% 
  dplyr::summarise(BS01 = mean(trimmed)/2)

AGGFORECASTS_withLP_2 <- left_join(AGGFORECASTS_withLP, 
                                   DF_QA_trimmed %>% dplyr::select(discover.question.id,
                                                           Region, 
                                                           IFP.Generation.Method, 
                                                           Domain, 
                                                           Deaths))
AGGFORECASTS_withLP_2 <- left_join(AGGFORECASTS_withLP, 
                                   DF_QA_trimmed %>% dplyr::select(discover.question.id,
                                                                   Region))
AGGFORECASTS_withLP_2 <- left_join(AGGFORECASTS_withLP_2, compareX)

adjustFactor          <- 0.2 # when zero should reduce to trimmed mean forecast.
newForecastDF         <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(newForecastDF) <- c("discover.question.id", "answer.name", "newForecast")
for (q1 in 1:Nquestions){
  thisDF      <- AGGFORECASTS_withLP_2 %>% 
                     dplyr::filter(discover.question.id == DF.question.id.vec[q1])  
  idxMax      <- which.max(thisDF$trimmed.forecast)
  idxOther    <- setdiff(c(1:nrow(thisDF)), idxMax)
  toShiftAll  <- max(0, adjustFactor * unique(na.omit(thisDF$BS01)) * thisDF$trimmed.forecast[idxMax])
  toShiftEach <- toShiftAll / (nrow(thisDF)-1)
  toSave      <- data.frame(matrix(nrow = nrow(thisDF), ncol = 3))
  colnames(toSave) <- c("discover.question.id", "answer.name", "newForecast")
  toSave$discover.question.id  <- thisDF$discover.question.id
  toSave$answer.name           <- thisDF$answer.name
  toSave$newForecast[idxMax]   <- thisDF$trimmed.forecast[idxMax]  - toShiftAll
  toSave$newForecast[idxOther] <- thisDF$trimmed.forecast[idxOther] + toShiftEach
  newForecastDF <- rbind(newForecastDF, toSave)
}

AGGFORECASTS_withLPNew <- left_join(AGGFORECASTS_withLP, newForecastDF)

BrierScoreMat2_new <- AGGFORECASTS_withLPNew %>%
  dplyr::group_by(discover.question.id) %>%
  dplyr::summarise(
    newForecast   = sum((newForecast - resolution)^2), #
    LP            = sum((aggMineScaled - resolution)^2), #
    mean          = sum((mean.forecast - resolution)^2), #
    median        = sum((median.forecast - resolution)^2),
    trimmed       = sum((trimmed.forecast - resolution)^2),
    geometric     = sum((geometric.forecast - resolution)^2),
    geometricOdds_prob = sum((GeometricMeanOfOdds_prob - resolution)^2),
    geometricOdds = sum((GeometricMeanOfOdds- resolution)^2)
  ) %>%
  ungroup()  # Ungroup after summarizing

colMeans(BrierScoreMat2_new)

# adjustFactor = 1  ; newForecast BS = 0.4239060 > 0.3729954
# adjustFactor = 0.8; newForecast BS = 0.4107020 > 0.3729954
# adjustFactor = 0.6; newForecast BS = 0.3990089
# adjustFactor = 0.4; newForecast BS = 0.3888268
# adjustFactor = 0.2; newForecast BS = 0.3801557

#test <- predictionSets %>% dplyr::filter(discover.question.id == 196, 
#                                         membership.guid == "00790045df5d52c2217ed7907f8f58fa4fdb08b8")
# test <- PredRecent %>% dplyr::filter(membership.guid == "266104c78f8e0b2896d673590f2deeff0e96a9d5", 
#                                      discover.question.id == 361)
# test3 <- DF_PRED %>% dplyr::filter(membership.guid == "266104c78f8e0b2896d673590f2deeff0e96a9d5", 
#                                      discover.question.id == 361)
# 
# test <- PredRecent %>% dplyr::filter(membership.guid == "0024b482bba96754f4850f872c37f903005707da", 
#                                      discover.question.id == 281)


colMeans(BrierScoreMat2)
# https://colah.github.io/posts/2014-07-Understanding-Convolutions/

#############################################################
# CODE GRAVEYARD

# Error analysis - not using Briar score - just simpel mode analysis

# wrongForecast <- simpleMat$discoverQuestionID[which(simpleMat$mean == 0)]
# explore <- DF_QA_trimmed %>% dplyr::filter(discover.question.id %in% wrongForecast)
# 
# 
# compareRegion <- left_join(DF_QA_trimmed %>% group_by(Region) %>% dplyr::summarise(n_all = n()),
#                            explore %>% group_by(Region) %>% dplyr::summarise(n_wrong = n()))
# compareRegion$prop <- compareRegion$n_wrong / compareRegion$n_all
# 
# compareIFP.Generation.Method <- left_join(DF_QA_trimmed %>% group_by(IFP.Generation.Method) %>% dplyr::summarise(n_all = n()),
#                                           explore %>% group_by(IFP.Generation.Method) %>% dplyr::summarise(n_wrong = n()))
# compareIFP.Generation.Method$prop <- compareIFP.Generation.Method$n_wrong / compareIFP.Generation.Method$n_all
# 
# compareDomain <- left_join(DF_QA_trimmed %>% group_by(Domain) %>% dplyr::summarise(n_all = n()),
#                            explore %>% group_by(Domain) %>% dplyr::summarise(n_wrong = n()))
# compareDomain$prop <- compareDomain$n_wrong / compareDomain$n_all
# 
# compareDeaths <- left_join(DF_QA_trimmed %>% group_by(Deaths) %>% dplyr::summarise(n_all = n()),
#                            explore %>% group_by(Deaths) %>% dplyr::summarise(n_wrong = n()))
# compareDeaths$prop <- compareDeaths$n_wrong / compareDeaths$n_all
# 
# compareCreated.By <- left_join(DF_QA_trimmed %>% group_by(Created.By) %>% dplyr::summarise(n_all = n()),
#                                explore %>% group_by(Created.By) %>% dplyr::summarise(n_wrong = n()))
# compareCreated.By$prop <- compareCreated.By$n_wrong / compareCreated.By$n_all
# 
# compareX <- left_join(DF_QA_trimmed %>% group_by(Domain, IFP.Generation.Method) %>% dplyr::summarise(n_all = n()),
#                       explore %>% group_by(Domain, IFP.Generation.Method) %>% dplyr::summarise(n_wrong = n()))
# compareX$prop <- compareX$n_wrong / compareX$n_all
# 
# 
# test <- predictionSets %>% dplyr::filter(discover.question.id == 196, 
#                                          membership.guid == "00790045df5d52c2217ed7907f8f58fa4fdb08b8")
# # test <- PredRecent %>% dplyr::filter(membership.guid == "266104c78f8e0b2896d673590f2deeff0e96a9d5", 
# #                                      discover.question.id == 361)
# # test3 <- DF_PRED %>% dplyr::filter(membership.guid == "266104c78f8e0b2896d673590f2deeff0e96a9d5", 
# #                                      discover.question.id == 361)
# # 
# # test <- PredRecent %>% dplyr::filter(membership.guid == "0024b482bba96754f4850f872c37f903005707da", 
# #                                      discover.question.id == 281)



# ASSUMPTION: "External predictor ID = The ID of the forecasting method that submitted
# this forecast" = "forecaster" as described in Step 2. 

# temp_forecast         <- dailyForecasts %>% dplyr:: filter(discover.question.id == 177) 
# length(unique(temp_forecast$external.predictor.id))
# length(unique(temp_forecast$external.prediction.id)) # same as number of rows. Good!
# converted_date        <- ymd_hms(temp_forecast$created.at, tz = "UTC")
# head(converted_date)
# head(temp_forecast$created.at)
# temp_forecast$created.at <- ymd_hms(temp_forecast$created.at, tz = "UTC")
# str(temp_forecast)
# 
# test1 <- temp_forecast %>% dplyr::group_by(discover.question.id, external.predictor.id) %>% 
#   dplyr::summarise(n = n())
# 
# test2 <- temp_forecast %>% dplyr::group_by(external.predictor.id, 
#                                            discover.answer.id, 
#                                            created.at, forecast) %>% 
#                            dplyr::summarise(n = n())
# 
# test3 <- temp_forecast %>% dplyr::group_by(external.predictor.id, 
#                                            discover.answer.id) %>% 
#                            dplyr::summarise(maxtime = max(created.at), n = n())
# 
# 
# test3_forecast <- left_join(test3, temp_forecast)
# 
# test4 <- test3_forecast %>% dplyr::group_by(discover.question.id, discover.answer.id) %>% 
#   dplyr::summarise(meanForecast = mean(forecast))
# sum(test4$meanForecast)
# 
# test5 <- left_join(test4, questionsAnswers2 %>% dplyr::select(discover.question.id, discover.answer.id, question.name, answer.name))
# 
# testPred <- DF_PRED %>% dplyr:: filter(discover.question.id == 177) 
