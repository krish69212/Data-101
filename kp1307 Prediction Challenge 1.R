# Load training data
train <- read.csv("C:/Users/krish/OneDrive/Desktop/Data 101/Prediction Challenge 1/Prediction20251Train.csv")

# Display first few rows and summary statistics
head(train)
summary(train)

# Analyzing Business students by Grade
business_A <- subset(train, Grade == "A" & Major == "Business")
business_B <- subset(train, Grade == "B" & Major == "Business")

cat("Quartiles for Business A students - Score:\n")
print(quantile(business_A$Score, probs = c(0.25, 0.5, 0.75)))
cat("Quartiles for Business A students - Participation:\n")
print(quantile(business_A$Participation, probs = c(0.25, 0.5, 0.75)))

cat("Quartiles for Business B students - Score:\n")
print(quantile(business_B$Score, probs = c(0.25, 0.5, 0.75)))
cat("Quartiles for Business B students - Participation:\n")
print(quantile(business_B$Participation, probs = c(0.25, 0.5, 0.75)))

# Analyzing CS students by Grade
cs_A <- subset(train, Grade == "A" & Major == "CS")

cat("Quartiles for CS A students - Score:\n")
print(quantile(cs_A$Score, probs = c(0.25, 0.5, 0.75)))
cat("Quartiles for CS A students - Participation:\n")
print(quantile(cs_A$Participation, probs = c(0.25, 0.5, 0.75)))

# Define colors for visualization
colors <- c('red', 'blue', 'cyan', 'yellow', 'green')

# Boxplot of Score vs. Grade
boxplot(Score ~ Grade, data = train, xlab = "Grade", ylab = "Score", 
        main = "Boxplot of Grade vs. Score", col = colors, border = "black")

# Boxplot of Participation vs. Grade
boxplot(Participation ~ Grade, data = train, xlab = "Grade", ylab = "Participation", 
        main = "Boxplot of Grade vs. Participation", col = colors, border = "black")

# Grade distribution table
cat("Grade Distribution:\n")
print(table(train$Grade))

# Grade distribution for students with high participation (> 0.6)
high_participation <- subset(train, Participation > 0.6)
cat("Grade distribution for high participation students:\n")
print(table(high_participation$Grade))

# Grade distribution by Major
cat("Grade distribution by Major:\n")
print(table(train$Grade, train$Major))

# Grade distribution by Seniority
cat("Grade distribution by Seniority:\n")
print(table(train$Grade, train$Seniority))

# Grade distribution for students with low participation (< 0.25)
low_participation <- subset(train, Participation < 0.25)
cat("Grade distribution for low participation students:\n")
print(table(low_participation$Grade))

# Scatterplot: Participation vs. Score
plot(train$Participation, train$Score, ylab = "Score", xlab = "Participation", 
     main = "Score vs. Participation", col = "red")

# Boxplot: Participation vs. Seniority
boxplot(Participation ~ Seniority, data = train, xlab = "Seniority", ylab = "Participation", 
        main = "Boxplot of Participation vs. Seniority", col = colors, border = "black")

# High-performing students with high participation
high_perf <- subset(train, Participation > 0.5 & Score > 75)
cat("Grade distribution for high-score & high-participation students:\n")
print(table(high_perf$Grade))

# ANALYSIS & ASSUMPTIONS FROM DATA:
#
# 1. GRADE 'A' PATTERNS:
#    - Scores 90+ with participation >= 0.5
#    - Scores 85+ with participation >= 0.7
#    - Seniors have slightly lower threshold 85+ with >= 0.6 participation
#
# 2. GRADE 'B' PATTERNS:
#    - Most B grades fall between 75-90 score range
#    - Higher scores 80-90 need participation >= 0.4
#    - Lower scores 75-80 need participation >= 0.6
#
# 3. GRADE 'F' PATTERNS:
#    - Scores below 65 result in F
#    - Participation < 0.2 results in F
#
# 4. GRADE 'C' PATTERNS:
#    - C is the default grade
#
# 5. KEY THRESHOLDS:
#    - Score: 90+ (A), 80-90 (B), 75-80 (B high participation), <65 (F)
#    - Participation: 0.7+ (high), 0.4-0.6 (moderate), <0.2 (failing)


# First apply prediction rules to training data, everything becomes C
decision <- rep("C", nrow(train))

# A grade rules
decision[train$Score >= 90 & train$Participation >= 0.5] <- "A"
decision[train$Score >= 85 & train$Participation >= 0.7] <- "A"

# B grade rules
decision[train$Score >= 80 & train$Score < 90 & train$Participation >= 0.4] <- "B"
decision[train$Score >= 75 & train$Score < 80 & train$Participation >= 0.6] <- "B"

# F grade rule
decision[train$Score < 65] <- "F"
decision[train$Participation < 0.2] <- "F"

# Calculate and print accuracy
accuracy <- mean(decision == train$Grade)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# Load test data
test <- read.csv("C:/Users/krish/OneDrive/Desktop/Data 101/Prediction Challenge 1/Prediction20251TestStudents.csv")

# Apply same rules to test data
decision <- rep("C", nrow(test))

decision[test$Score >= 90 & test$Participation >= 0.5] <- "A"
decision[test$Score >= 85 & test$Participation >= 0.7] <- "A"

decision[test$Score >= 80 & test$Score < 90 & test$Participation >= 0.4] <- "B"
decision[test$Score >= 75 & test$Score < 80 & test$Participation >= 0.6] <- "B"

decision[test$Score < 65] <- "F"
decision[test$Participation < 0.2] <- "F"

#Load submission file
submission <- read.csv("C:/Users/krish/OneDrive/Desktop/Data 101/Prediction Challenge 1/submission2025-1.csv")

submission$Grade <- decision

write.csv(submission, 'submission.csv', row.names=FALSE)

