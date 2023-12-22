# Key Variables: Sex, Best3SquatKg, Best3DeadliftKg, Best3BenchKg
Auto <- read.csv(file.choose(), header = TRUE, stringsAsFactors = TRUE)


## Eda done for project 
### THIS IS ALL FOR EDA for problem #1##
# Filter out rows with missing values in the key variable areas: Best3 Of each class, Bodyweight, and Sex
Auto <- Auto[complete.cases(Auto[, c("Best3SquatKg", "Best3BenchKg", "Best3DeadliftKg", "BodyweightKg", "Sex", "WeightClassKg", "Age", "AgeClass")]), ]

# Boxplots for the original data
boxplot(Best3SquatKg ~ Sex, data = Auto, xlab = "Sex", ylab = "Best3SquatKg", main = "Boxplot of Best3SquatKg by Sex")
boxplot(Best3BenchKg ~ Sex, data = Auto, xlab = "Sex", ylab = "Best3BenchKg", main = "Boxplot of Best3BenchKg by Sex")
boxplot(Best3DeadliftKg ~ Sex, data = Auto, xlab = "Sex", ylab = "Best3DeadliftKg", main = "Boxplot of Best3DeadliftKg by Sex")

# Calculate the average difference between men and women for the original data
average_diff_Squat_orig <- mean(subset(Auto, Sex == "M")$Best3SquatKg - subset(Auto, Sex == "F")$Best3SquatKg)
average_diff_Bench_orig <- mean(subset(Auto, Sex == "M")$Best3BenchKg - subset(Auto, Sex == "F")$Best3BenchKg)
average_diff_Deadlift_orig <- mean(subset(Auto, Sex == "M")$Best3DeadliftKg - subset(Auto, Sex == "F")$Best3DeadliftKg)

cat("Average difference in Best3SquatKg between men and women (Original Data):", average_diff_Squat_orig, "\n")
cat("Average difference in Best3BenchKg between men and women (Original Data):", average_diff_Bench_orig, "\n")
cat("Average difference in Best3DeadliftKg between men and women (Original Data):", average_diff_Deadlift_orig, "\n")

# Convert 'Best3SquatKg' to numeric
Auto$Best3SquatKg <- as.numeric(Auto$Best3SquatKg)
# Filter out rows with missing values, negative values, in 'Best3SquatKg' or 'Sex'
Auto <- Auto[complete.cases(Auto[, c("Best3SquatKg", "Sex")]) & Auto$Best3SquatKg >= 0, ]
Auto <- Auto[complete.cases(Auto[, c("Best3BenchKg", "Sex")]) & Auto$Best3BenchKg >= 0, ]
Auto <- Auto[complete.cases(Auto[, c("Best3DeadliftKg", "Sex")]) & Auto$Best3DeadliftKg >= 0, ]

# Create separate histograms for males and females
par(mfrow = c(1, 2))  # Set up a 1x2 layout for two plots side by side
# Histogram for males Squat
hist(subset(Auto, Sex == "M")$Best3SquatKg, main = "Histogram for Males", xlab = "Best3SquatKg", col = "blue", breaks = 15)
# Histogram for females Squat
hist(subset(Auto, Sex == "F")$Best3SquatKg, main = "Histogram for Females", xlab = "Best3SquatKg", col = "pink", breaks = 15)
# Histogram for males Bench
hist(subset(Auto, Sex == "M")$Best3BenchKg, main = "Histogram for Males", xlab = "Best3BenchKg", col = "blue", breaks = 15)
# Histogram for females Bench
hist(subset(Auto, Sex == "F")$Best3BenchKg, main = "Histogram for Females", xlab = "Best3BenchKg", col = "pink", breaks = 15)
# Histogram for males Deadlift
hist(subset(Auto, Sex == "M")$Best3DeadliftKg, main = "Histogram for Males", xlab = "Best3DeadliftKg", col = "blue", breaks = 15)
# Histogram for females Deadlift
hist(subset(Auto, Sex == "F")$Best3DeadliftKg, main = "Histogram for Females", xlab = "Best3DeadliftKg", col = "pink", breaks = 15)
# Reset the plot layout
par(mfrow = c(1, 1))  # Reset the layout to default
#### THIS IS THE END OF OUR EDA GRAPHING 



# Filter by gender and select the most recent records, Otherwise Too much RAM usage
male_data <- subset(Auto, Sex == "M")
female_data <- subset(Auto, Sex == "F")
male_data <- male_data[order(male_data$Date, decreasing = TRUE), ][1:1000, ]
female_data <- female_data[order(female_data$Date, decreasing = TRUE), ][1:1000, ]
filtered_data <- rbind(male_data, female_data)

# Boxplots for filtered data
boxplot(Best3SquatKg ~ Sex, data = filtered_data, xlab = "Sex", ylab = "Best3SquatKg", main = "Boxplot of Best3SquatKg by Sex")

# Calculate the average difference between men and women for the filtered data
average_diff_Squat <- mean(subset(filtered_data, Sex == "M")$Best3SquatKg - subset(filtered_data, Sex == "F")$Best3SquatKg)
average_diff_Bench <- mean(subset(filtered_data, Sex == "M")$Best3BenchKg - subset(filtered_data, Sex == "F")$Best3BenchKg)
average_diff_Deadlift <- mean(subset(filtered_data, Sex == "M")$Best3DeadliftKg - subset(filtered_data, Sex == "F")$Best3DeadliftKg)

cat("Average difference in Best3SquatKg between men and women (Filtered Data):", average_diff_Squat, "\n")
cat("Average difference in Best3BenchKg between men and women (Filtered Data):", average_diff_Bench, "\n")
cat("Average difference in Best3DeadliftKg between men and women (Filtered Data):", average_diff_Deadlift, "\n")

# Moving On to modeling:
set.seed(1)  # Set seed for reproducibility
sample_indices <- sample(1:nrow(filtered_data), 0.8 * nrow(filtered_data))
train_data <- filtered_data[sample_indices, ]
test_data <- filtered_data[-sample_indices, ]

# Using linear regression with cross-validation
library(caret)
ctrl <- trainControl(method = "cv", number = 5)
model_cv_Squat <- train(Best3SquatKg ~ Sex, data = train_data, method = "lm", trControl = ctrl)
model_cv_Bench <- train(Best3BenchKg ~ Sex, data = train_data, method = "lm", trControl = ctrl)
model_cv_Deadlift <- train(Best3DeadliftKg ~ Sex, data = train_data, method = "lm", trControl = ctrl)

# View cross-validation results for Best3SquabtKg
print(model_cv_Squat)
print(model_cv_Bench)
print(model_cv_Deadlift)

# Using Random Forest for Alternative modeling, though this time assuming that Weight also matters:
library(randomForest)
# Using Random Forest for Best3SquatKg with filtered_data
rf_model_Squat_filtered <- randomForest(Best3SquatKg ~ Sex + BodyweightKg + Age + AgeClass, data = filtered_data)
rf_model_Deadlift_filtered <- randomForest(Best3DeadliftKg ~ Sex + BodyweightKg   + Age + AgeClass, data = filtered_data)
rf_model_Bench_filtered<- randomForest(Best3BenchKg ~ Sex + BodyweightKg + Age + AgeClass, data = filtered_data)



# View Random Forest results for Best3SquatKg with filtered_data
print(rf_model_Squat_filtered)
print(rf_model_Deadlift_filtered)
print(rf_model_Bench_filtered)
# Extract variable importance scores for Best3SquatKg
importance_scores_Squat <- importance(rf_model_Squat_filtered)

# Print the variable importance scores for Best3SquatKg
print(importance_scores_Squat)


# Display results for Best3SquatKg
cat("Linear Regression Results for Best3SquatKg:\n")
print(model_cv_Squat)

# Display results for Best3BenchKg
cat("Linear Regression Results for Best3BenchKg:\n")
print(model_cv_Bench)

# Display results for Best3DeadliftKg
cat("Linear Regression Results for Best3DeadliftKg:\n")
print(model_cv_Deadlift)


# Scatterplot by gender before linear regression for Best3SquatKg
par(mfrow = c(1, 2))  # Set up a 1x2 layout for two plots side by side

# Scatterplot for males Squat (Before)
plot(subset(train_data, Sex == "M")$Best3SquatKg, main = "Scatterplot for Males (Before)", xlab = "Index", ylab = "Best3SquatKg", col = "blue")
points(subset(test_data, Sex == "M")$Best3SquatKg, col = "darkblue")

# Scatterplot for females Squat (Before)
plot(subset(train_data, Sex == "F")$Best3SquatKg, main = "Scatterplot for Females (Before)", xlab = "Index", ylab = "Best3SquatKg", col = "pink")
points(subset(test_data, Sex == "F")$Best3SquatKg, col = "darkred")

# Linear Regression for Best3SquatKg
model_Squat <- lm(Best3SquatKg ~ Sex, data = train_data)

# Scatterplot by gender after linear regression for Best3SquatKg
# Scatterplot for males Squat (After)
plot(subset(train_data, Sex == "M")$Best3SquatKg, main = "Scatterplot for Males (After)", xlab = "Index", ylab = "Best3SquatKg", col = "blue")
points(subset(test_data, Sex == "M")$Best3SquatKg, col = "darkblue")
abline(model_Squat, col = "red")

# Scatterplot for females Squat (After)
plot(subset(train_data, Sex == "F")$Best3SquatKg, main = "Scatterplot for Females (After)", xlab = "Index", ylab = "Best3SquatKg", col = "pink")
points(subset(test_data, Sex == "F")$Best3SquatKg, col = "darkred")
abline(model_Squat, col = "red")





