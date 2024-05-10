
# Load required libraries
library(dplyr)   # For data manipulation
library(ggplot2) # For data visualization
library(tidyr)   # For data restructuring



# Create vectors for quality scores and visual aids
quality_scores <- c(50, 60, 58, 72, 36, 51, 49, 49, 25, 52, 41, 32, 58, 39, 25, 40, 61,
                    58, 70, 60, 73, 40, 63, 54, 60, 29, 57, 66, 37, 50, 48, 80, 65, 70)

visual_aids <- rep(c("No Visual Aids", "With Visual Aids"), each = 17)

# Create a data frame
lecture_data <- data.frame(Student = 1:17, Quality_Score = quality_scores, Visual_Aids = visual_aids)

# View the first few rows of the data frame
head(lecture_data)

print(lecture_data)

View(lecture_data)


#Data Cleaning
# Check for missing values
missing_values <- sum(is.na(lecture_data))
if(missing_values > 0) {
  lecture_data <- na.omit(lecture_data)
}
missing_values

# Data Exploration
# Summary statistics
summary(lecture_data)


# Assuming lecture_data contains the quality scores and visual aids data

# Create a box plot
ggplot(lecture_data, aes(x = Visual_Aids, y = Quality_Score, fill = Visual_Aids)) +
  geom_boxplot() +
  labs(x = "Visual Aids", y = "Quality Score", title = "Quality Scores with and without Visual Aids") +
  scale_fill_manual(values = c("No Visual Aids" = "blue", "With Visual Aids" = "skyblue"))





# Observations:
# - The median quality score appears higher with visual aids compared to no visual aids.
# - There seems to be greater variability in quality scores without visual aids.



# Shapiro-Wilk test for normality
shapiro.test(lecture_data$Quality_Score)

# Descriptive statistics for quality scores
summary(lecture_data$Quality_Score)  # Summary statistics including min, 1st quartile, median, mean, 3rd quartile, and max

# Mean
mean_score <- mean(lecture_data$Quality_Score)
cat("Mean:", mean_score, "\n")

# Median
median_score <- median(lecture_data$Quality_Score)
cat("Median:", median_score, "\n")

# Standard deviation
sd_score <- sd(lecture_data$Quality_Score)
cat("Standard Deviation:", sd_score, "\n")

# Interquartile range
quartiles <- quantile(lecture_data$Quality_Score, probs = c(0.25, 0.75))
iqr <- diff(quartiles)
cat("Interquartile Range:", iqr, "\n")



# Load the e1071 package for skewness calculation
library(e1071)

# Compute skewness
skew <- skewness(lecture_data$Quality_Score)
cat("Skewness:", skew, "\n")


# If normally distributed: Mean, median, standard deviation, and confidence interval would be appropriate.
# If not normally distributed: Median, interquartile range, and non-parametric tests such as Wilcoxon signed-rank test.

# Assuming normal distribution for simplicity.


# Calculate mean and standard deviation
# Calculate mean
mean_quality <- mean(lecture_data$Quality_Score)
cat("Mean:", mean_quality, "\n")

# Calculate standard deviation
sd_quality <- sd(lecture_data$Quality_Score)
cat("Standard Deviation:", sd_quality, "\n")

# Calculate mean
mean_quality <- mean(lecture_data$Quality_Score)
cat("Mean:", mean_quality, "\n")

# Calculate standard deviation
sd_quality <- sd(lecture_data$Quality_Score)
cat("Standard Deviation:", sd_quality, "\n")




# Confidence intervals
# Calculate confidence interval (95% confidence level)
# Assuming the data follows a normal distribution
n <- length(lecture_data$Quality_Score)
stderr <- sd(lecture_data$Quality_Score) / sqrt(n)
margin_error <- qt(0.975, df = n - 1) * stderr
lower_ci <- mean(lecture_data$Quality_Score) - margin_error
upper_ci <- mean(lecture_data$Quality_Score) + margin_error
cat("95% Confidence Interval:", lower_ci, "-", upper_ci, "\n")


# Calculate mean, standard deviation, and confidence interval for each condition
condition_means <- aggregate(Quality_Score ~ Visual_Aids, data = lecture_data, mean)
condition_sds <- aggregate(Quality_Score ~ Visual_Aids, data = lecture_data, sd)

# Assuming the data follows a normal distribution, calculate confidence intervals
n <- length(lecture_data$Quality_Score)
condition_cis <- tapply(lecture_data$Quality_Score, lecture_data$Visual_Aids, function(x) {
  stderr <- sd(x) / sqrt(length(x))
  margin_error <- qt(0.975, df = length(x) - 1) * stderr
  c(mean(x) - margin_error, mean(x) + margin_error)
})

# Print the results
cat("Condition\tMean\t\tStandard Deviation\t95% Confidence Interval\n")
for (i in 1:nrow(condition_means)) {
  cat(condition_means[i, "Visual_Aids"], "\t",
      condition_means[i, "Quality_Score"], "\t",
      condition_sds[i, "Quality_Score"], "\t\t\t",
      condition_cis[[i]][1], "-", condition_cis[[i]][2], "\n")
}

# Paired t-test
t_test_result <- t.test(Quality_Score ~ Visual_Aids, data = lecture_data, paired = TRUE)

# Print the results
cat("Paired t-test results:\n")
print(t_test_result)


# Null hypothesis: There is no difference in the quality of lecture due to the number of visual aids used.
# Alternative hypothesis: There is a difference in the quality of lecture due to the number of visual aids used.

# 0.05 level of significance means that if the p-value is less than 0.05, we reject the null hypothesis.

# Print the results of the paired t-test




# Print the confidence interval
cat("95% Confidence Interval for the Mean Difference:\n")
print(t_test_result$conf.int)

# Conclusion
if (t_test_result$p.value < 0.05) {
  cat("Reject the null hypothesis. There is a significant difference in lecture quality due to the number of visual aids used.")
} else {
  cat("Fail to reject the null hypothesis. There is no significant difference in lecture quality due to the number of visual aids used.")
}




