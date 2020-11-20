# Script created for HarvardX PH125.9x Data Science: Capstone
# Assessment: IDV Learners - Project Submission: Choose Your Own

# Title: 'HarvardX PH125.9x Capstone - CYO - Script'
# Author: "b_woods"
# Date: "15/11/2020"

# Created for R 4.0.2. Not tested on earlier versions. 
# Can take 10 minutes to run due to calculations required.



################################################################################
# Setup libraries required for script

# Install required libraries if not already installed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")


# Import required libraries
library(tidyverse) # Data manipulation
library(data.table) # Data storage
library(cowplot) # Chart layout
library(matrixStats) # Row based calculations
library(caret) # Machine learning



################################################################################
# Import and save required data

# Download data from UCI Machine Learning Repository
import_file <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", 
              import_file)


# File does not have column names so manually record these and type of column data
# Column names taken from 'abalone.names' readme file available at 
# https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.names
import_col_names <- c('sex', 'length', 'diameter', 'height', 'weight_Whole', 
                      'weight_shucked', 'weight_viscera', 'weight_shell', 
                      'rings')


# Ordered string of column types, f = factor, d = decimal, i = int
import_col_types <- "fdddddddi" 


# Create new dataset from downloaded data with specified column names/types
abalone <- read_csv(import_file, 
                    col_names = import_col_names, 
                    col_types = import_col_types)



################################################################################

# Data analysis


# Structure of data
dim(abalone) # Number of rows/columns
str(abalone) # Structure of tibble
head(abalone) # First rows


# Summary statistics
summary(abalone)


# Check for NA's - none present
abalone %>%
  summarize(
    na_sex = sum(is.na(sex)),
    na_length = sum(is.na(length)),
    na_diameter = sum(is.na(diameter)),
    na_height = sum(is.na(height)),
    na_weight_Whole = sum(is.na(weight_Whole)),
    na_weight_shucked = sum(is.na(weight_shucked)),
    na_weight_viscera = sum(is.na(weight_viscera)),
    na_weight_shell = sum(is.na(weight_shell)),
    na_rings = sum(is.na(rings)),
  )



# Analysis of rings

# Unique values
unique(abalone$rings)


# Distribution of rings is roughly normally distrusted with a mean around 10 
# rings, though the tail to the right extends into much larger values.
abalone %>%
  ggplot(aes(rings)) +
  geom_histogram(binwidth=1, fill="#2931d6", color="#e9ecef", alpha=0.9) +
  ggtitle("Rings distribution")

           
      
# Sex distribution 

# Three categories - Male(M), Female(F) and Infant (I)
# Female and infant around the same counts with male being a little larger
abalone %>%
  ggplot(aes(sex)) +
  geom_bar(fill="#2931d6", color="#e9ecef") +
  ggtitle("Sex distribution")


# Distribution of rings per sex
# Male and female roughly the same, with infant being smaller
# Large number of higher outliers while few smaller
abalone %>%
  ggplot(aes(sex, rings, colour = sex)) +
  geom_boxplot() +
  ggtitle("Rings distribution per sex")



# Length and diameter distribution

# Distribution of length and diameter is similar so display these side by side
# Both have a right skew
plot_length_dist <- abalone %>%
  ggplot(aes(length)) +
  geom_histogram(fill="#2931d6", color="#e9ecef", bins = 30) +
  ggtitle("Length distribution")

plot_diameter_dist <- abalone %>%
  ggplot(aes(diameter)) +
  geom_histogram(fill="#2931d6", color="#e9ecef", bins = 30) +
  ggtitle("Diameter distribution")

plot_grid(plot_length_dist, plot_diameter_dist) # Display graphs together


# Compare the comparison of length/diameter to rings side by side
# Infants make up most of the smaller data ranges, the larger roughly split between 
# male and female 
plot_length_comp <- abalone %>%
  ggplot(aes(length, rings, colour = sex)) +
  geom_point(alpha = .6) +
  ggtitle("Rings distribution by length")

plot_length_diameter <- abalone %>%
  ggplot(aes(diameter, rings, colour = sex)) +
  geom_point(alpha = .6) +
  ggtitle("Rings distribution by diameter")

plot_grid(plot_length_comp, plot_length_diameter) # Display graphs together


# Length and diameter have a clear linear relationship though the correlation
# is lowered as values become larger
abalone %>%
  ggplot(aes(diameter, length, colour = sex)) +
  geom_point(alpha = .6) +
  ggtitle("Diameter to length comparison")



# Height distribution

# Heights fall within a small range and are roughly normally distributed.
# Note the records with a height of 0.
abalone %>%
  ggplot(aes(height)) +
  geom_histogram(fill="#2931d6", color="#e9ecef", binwidth = .01) +
  ggtitle("Height distribution")


# Number of records with a height of 0
abalone %>%
  filter(height == 0) %>%
  summarize(height_zero = n()) %>%
  pull(height_zero)


# Rings distribution by height, colour coded by sex
# Small range of values, with infants in the lower range and male/female split 
# in the higher ranges. Not extreme outliers.
abalone %>%
  ggplot(aes(height, rings, colour = sex)) +
  geom_point(alpha = .6) +
  ggtitle("Rings distribution by height")



# Weight Distribution

# There are four different weight values. Create graphs for each of them
# All four plots have roughly the same shape and a clear right skew
plot_weight_whole_dist <- abalone %>%
  ggplot(aes(weight_Whole)) +
  geom_histogram(fill="#2931d6", color="#e9ecef", bins = 30) +
  ggtitle("Whole weight distribution")

plot_weight_shucked_dist <- abalone %>%
  ggplot(aes(weight_shucked)) +
  geom_histogram(fill="#2931d6", color="#e9ecef", bins = 30) +
  ggtitle("Shucked weight distribution")

plot_weight_viscera_dist <- abalone %>%
  ggplot(aes(weight_viscera)) +
  geom_histogram(fill="#2931d6", color="#e9ecef", bins = 30) +
  ggtitle("Viscera weight distribution")

plot_weight_shell_dist <- abalone %>%
  ggplot(aes(weight_shell)) +
  geom_histogram(fill="#2931d6", color="#e9ecef", bins = 30) +
  ggtitle("Shell weight distribution")

# Display graphs together
plot_grid(plot_weight_whole_dist,  
          plot_weight_shucked_dist,
          plot_weight_viscera_dist,
          plot_weight_shell_dist)


# Map the relationship of how each of the different weights relates to the whole
# weight for each different sex
# All three have a clear linear relationship, with shell weight showing more deviation
# towards higher end values
plot_weight_comp_shucked <- abalone %>%
  ggplot(aes(weight_shucked, weight_Whole, colour = sex)) +
  geom_point(alpha = .6) +
  ggtitle("Shucked weight to whole weight comparison")

plot_weight_comp_viscera <- abalone %>%
  ggplot(aes(weight_viscera, weight_Whole, colour = sex)) +
  geom_point(alpha = .6) +
  ggtitle("Viscera weight to whole weight comparison")

plot_weight_comp_shell <- abalone %>%
  ggplot(aes(weight_shell, weight_Whole, colour = sex)) +
  geom_point(alpha = .6) +
  ggtitle("Shell weight to whole weight comparison")

# Display graphs together
plot_grid(plot_weight_comp_shucked,
          plot_weight_comp_viscera,
          plot_weight_comp_shell)


# Create another four graphs, mapping each weight value with ring count
# All four plots have a similar distribution shape
plot_weight_Whole_comp <- abalone %>%
  ggplot(aes(weight_Whole, rings, colour = sex)) +
  geom_point(alpha = .6) +
  ggtitle("Rings distribution by whole weight")

plot_weight_shucked_comp <- abalone %>%
  ggplot(aes(weight_shucked, rings, colour = sex)) +
  geom_point(alpha = .6) +
  ggtitle("Rings distribution by shucked weight")

plot_weight_viscera_comp <- abalone %>%
  ggplot(aes(weight_viscera, rings, colour = sex)) +
  geom_point(alpha = .6) +
  ggtitle("Rings distribution by viscera weight")

plot_weight_shell_comp <- abalone %>%
  ggplot(aes(weight_shell, rings, colour = sex)) +
  geom_point(alpha = .6) +
  ggtitle("Rings distribution by shell weight")

# Display graphs together
plot_grid(plot_weight_Whole_comp, 
          plot_weight_shucked_comp,
          plot_weight_viscera_comp,
          plot_weight_shell_comp)






################################################################################

# Dataset cleaning


# Remove records with a height of 0 as this is invalid data
# Create a column for age, which is rings + 1.5. Remove rings as no longer required.
abalone_clean <- abalone %>%
  filter(height != 0) %>% # Remove values with an invalid height
  mutate(age = rings + 1.5) %>%
  select(-rings) # Don't select rings as age is now the dependent variable



# Generate a train/test set of .75/.25
set.seed(1, sample.kind="Rounding") # Use a common seed value for reproducibility
abalone_clean_test_index <- createDataPartition(y = abalone_clean$sex, times = 1,
                                                p = .25, list = FALSE)

abalone_clean_train <- abalone_clean[-abalone_clean_test_index,]
temp <- abalone_clean[abalone_clean_test_index,]

# Ensure age values in the test set also exist in the train set
abalone_clean_test <- temp %>% 
  semi_join(abalone_clean_train, by = "age")

removed <- anti_join(temp, abalone_clean_test, by = "age")
abalone_clean_train <- rbind(abalone_clean_train, removed)



################################################################################

# Model building

# As a baseline, determine the mean age value in the test set and calculate the
# RMSE of the test set using just this mean value.

train_mean_age <- mean(abalone_clean_train$age)
rmse_mean <- RMSE(train_mean_age, abalone_clean_test$age)
cat("RMSE for mean only:", rmse_mean)

# Create a storage table for tracking different RMSE values
rmse_results <- tibble(method = "Mean Only", 
                       RMSE = rmse_mean)



# Train a number of different models, testing the RMSE of each.
# Store the RMSE of each model in a table for comparison during result analysis.

# 1) Linear model

# Train a linear model
lm_control <- trainControl(method = "cv", number = 10) # Cross validation
train_lm <- train(age ~ .,
               data = abalone_clean_train,
               method = "lm",
               trControl = lm_control)

# Model coefficient / intercepts
train_lm$finalModel

# Residuals vs Fitted
plot(train_lm$finalModel, which = 1) 

# QQ Plot
plot(train_lm$finalModel, which = 2) 

# Test the linear model against the test data set
y_hat_lm <- predict(train_lm, abalone_clean_test)

# Calculate RMSE and add to reference table
rmse_lm <- RMSE(y_hat_lm, abalone_clean_test$age)
rmse_results <- add_row(rmse_results,
                        method = "Linear Model", 
                        RMSE = rmse_lm)

cat("RMSE for Linear Model:", rmse_lm)

# Plot how the predictions compare to the actual results
plot(y_hat_lm, abalone_clean_test$age)



# 2) K Nearest Neighbors (KNN)

# Train a KNN model
knn_control <- trainControl(method = "cv", number = 10, p = .8) # Cross validation
knn_tunegrid <- data.frame(k = seq(1, 25, 1)) # Determine optimal K between 1 and 25

train_knn <- train(age ~ ., 
                   method = "knn", 
                   data = abalone_clean_train,
                   tuneGrid = knn_tunegrid,
                   control = knn_control
                   )

# Model details
train_knn
train_knn$bestTune %>% pull()

# Plot the various K values
plot(train_knn)

# Test the knn model against the test data set
y_hat_knn <- predict(train_knn, abalone_clean_test)

# Calculate RMSE and add to reference table
rmse_knn <- RMSE(y_hat_knn, abalone_clean_test$age)
rmse_results <- add_row(rmse_results,
                        method = "KNN", 
                        RMSE = rmse_knn)

cat("RMSE for KNN:", rmse_knn)

# Plot how the predictions compare to the actual results
plot(y_hat_knn, abalone_clean_test$age)



# 3) Random forest

# Train a random forest model
rf_control <- trainControl(method="cv", number = 10) # Cross validation

train_rf <- train(age ~ ., 
                  method = "rf", 
                  data = abalone_clean_train,
                  trControl = rf_control
)


# RMSE against predictors
plot(train_rf)
train_rf$bestTune %>% pull()


# Test the random forest model against the test data set
y_hat_rf<- predict(train_rf, abalone_clean_test)

# calculate RMSE and add to reference table
rmse_rf <- RMSE(y_hat_rf, abalone_clean_test$age)
rmse_results <- add_row(rmse_results, 
                        method = "RF", 
                        RMSE = rmse_rf)

cat("RMSE for Random Forest:", rmse_rf)

# Plot how the predictions compare to the actual results
plot(y_hat_rf, abalone_clean_test$age)




# 4) Ensemble
# Ensemble models combine different model results into a generalized model
# Test two different ensembles - the mean of each row and the median

# Create a tibble containing all three predictions for each record
predctions_combined <- 
  tibble(lm = y_hat_lm, 
         knn = y_hat_knn, 
         rf = y_hat_rf) 


# First ensemble is the mean of all three predictions
ensemble_mean <- rowMeans(predctions_combined)


# calculate RMSE and add to reference table
rmse_ensemble_mean <- RMSE(ensemble_mean, abalone_clean_test$age)
rmse_results <- add_row(rmse_results, 
                        method = "Ensemble - Mean", 
                        RMSE = rmse_ensemble_mean)

cat("RMSE for Ensemble - Mean:", rmse_ensemble_mean)

# Plot how the predictions compare to the actual results
plot(ensemble_mean, abalone_clean_test$age)


# Second ensemble is the median of all three predictions
ensemble_median <- rowMedians(as.matrix(predctions_combined))

# calculate RMSE and add to reference table
rmse_ensemble_median <- RMSE(ensemble_median, abalone_clean_test$age)
rmse_results <- add_row(rmse_results,
                        method = "Ensemble - Median", 
                        RMSE = rmse_ensemble_median)

cat("RMSE for Ensemble - Median:", rmse_ensemble_median)

# Plot how the predictions compare to the actual results
plot(ensemble_median, abalone_clean_test$age)



################################################################################

# Analysis of results


# RMSE of each model
# Linear model/KNN/Random Forest have very similar RMSE values. Ensemble models 
# have a much lower RMSE, with the mean ensemble being the lowest value overall
rmse_results


# Combine the predictions for each model and actual values for ease of graphing
combined_results <- 
  tibble(actual_age = abalone_clean_test$age,
         lm = y_hat_lm,
         knn = y_hat_knn, 
         rf = y_hat_rf,
         ens_mean = ensemble_mean,
         ens_median = ensemble_median 
  )


# Compare the predicted value and the real value for each of the single models
# Include a simple line with a slope of 1 to each graph for comparison

# Actual age vs linear model predicted age
graph_actual_predicted_lm <- combined_results %>% 
  ggplot(aes(actual_age, lm)) +
  geom_abline(intercept = 0, slope = 1, color="#878787", size = 1.5, alpha = .5) +
  geom_point(colour = "#4dbb24", alpha = .6) +
  ggtitle("Actual Age vs Linear Model Prediction") +
  ylab("Linear Model Prediction") +
  xlab("Actual Age")

# Actual age vs KNN predicted age
graph_actual_predicted_knn <- combined_results %>% 
  ggplot(aes(actual_age, knn)) +
  geom_abline(intercept = 0, slope = 1, color="#878787", size = 1.5, alpha = .5) +
  geom_point(colour = "#2e5ac4", alpha = .6) +
  ggtitle("Actual Age vs KNN Prediction") +
  ylab("KNN Predictions") +
  xlab("Actual Age")

# Actual age vs Random Forest predictions
graph_actual_predicted_rf <- combined_results %>% 
  ggplot(aes(actual_age, rf)) +
  geom_abline(intercept = 0, slope = 1, color="#878787", size = 1.5, alpha = .5) +
  geom_point(colour = "#f1ba1b", alpha = .6) +
  ggtitle("Actual Age vs Random Forest Prediction") +
  ylab("Random Forest Predictions") +
  xlab("Actual Age")

# Overlay the above graphs in a single graph to indicate central and shared outliers
graph_actual_predicted_combined <- combined_results %>% 
  ggplot() +
  geom_abline(intercept = 0, slope = 1, color="#878787", size = 1.5, alpha = .5) +
  geom_point(aes(actual_age, lm), colour = "#4dbb24", alpha = .3) +
  geom_point(aes(actual_age, knn), colour = "#2e5ac4", alpha = .3) +
  geom_point(aes(actual_age, rf), colour = "#f1ba1b", alpha = .3) +
  geom_line(aes(1,1))+
  ggtitle("Actual Age vs Predictions Overlayed") +
  ylab("Predictions Overlayed") +
  xlab("Actual Age")

# Display all four graphs together for comparison
# Each model has a general linear contour but differs in the distribution
# Most deviation when actual age is higher
plot_grid(
  graph_actual_predicted_lm,
  graph_actual_predicted_knn,
  graph_actual_predicted_rf,
  graph_actual_predicted_combined
  )


# Now perform similar comparison on the ensemble models

# Mean ensemble
graph_actual_predicted_mean <- combined_results %>% 
  ggplot(aes(actual_age, ens_mean)) +
  geom_abline(intercept = 0, slope = 1, color="#878787", size = 1.5, alpha = .5) +
  geom_point(colour = "#2e5ac4", alpha = .6) +
  ggtitle("Actual Age vs Mean Ensemble Prediction") +
  ylab("Mean Ensemble Prediction") +
  xlab("Actual Age")


# Median ensemble
graph_actual_predicted_median <- combined_results %>% 
  ggplot(aes(actual_age, ens_median)) +
  geom_abline(intercept = 0, slope = 1, color="#878787", size = 1.5, alpha = .5) +
  geom_point(colour = "#4dbb24", alpha = .6) +
  ggtitle("Actual Age vs Median Ensemble Prediction") +
  ylab("Median Ensemble Prediction") +
  xlab("Actual Age")


# Compare median and mean models
# Very similar distribution and share share same deviation when age is higher
plot_grid(
  graph_actual_predicted_mean,
  graph_actual_predicted_median
)


# These graphs show the main issue looks to be with handling outliers with 
# high age. Create a box plot of age comparisons to visualize this further.
# The ensemble models handle the lower outliers better but none of the models 
# handle the upper outliers. This makes the center value of each predicted model
# higher than the center of the actual age value.
combined_results %>%
  pivot_longer(.,
               cols = c(actual_age, lm, knn, rf, ens_mean, ens_median),
               names_to = "variable", 
               values_to = "value") %>%
  
  ggplot(aes(variable, value, colour = variable)) +
  geom_boxplot() +
  ggtitle("Spread of Age") +
  ylab("Age") +
  xlab("Source")

  
# The ensemble models have lower RMSE and a more centered distribution making 
# them more suitable for the final model than one algorithm alone. Both are very
# close, but as the mean ensemble has slightly lower RMSE this will be the final model


# Create a new dataset containing just the actual_age, predicted_age and summary statistics
final_model_results <- combined_results %>%
  select(actual_age, predicted_age = ens_mean) %>%
  mutate(age_diff = actual_age - predicted_age) 


# Summary statistics of the final model
summary(final_model_results)


# Compare the distribution of actual age and predicted age
# Predicted age is more centered while actual age is more spread out, especially
# in the upper ranges
final_model_results %>%
  ggplot() +
  geom_histogram(aes(actual_age), fill = "#aaaaaa", alpha = .6, binwidth = 1) +
  geom_histogram(aes(predicted_age), fill = "#2e5ac4", alpha = .6, binwidth = 1) +
  ggtitle("Mean Ensemble Predicted Age vs Actual Age") +
  xlab("Age (Blue = Predicted, Silver = Actual)") +
  ylab("Count")
  
  
# Compare the distribution of differences between the actual age and predicted age
# Relatively normal distribution centered around 0
final_model_results %>%
  ggplot(aes(age_diff)) +
  geom_histogram(fill = "#2e5ac4", alpha = .7, binwidth = .5) +
  ggtitle("Actual - Predicted Age Differences") +
  xlab("Age Difference") +
  ylab("Count")
  


################################################################################
