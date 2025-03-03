library(rpart)
library(rpart.plot)

# Load training data
train <- read.csv("C:/Users/Krish/Documents/Data 101/Prediction Challenge 3/Prediction3Train.csv")
location <- read.csv("C:/Users/Krish/Documents/Data 101/Prediction Challenge 3/Location.csv")

# Add State column as Location
train$Location <- location$State[match(train$University, location$University)]

# Display first few rows and summary statistics
head(train)
summary(train)

# Analyzing by Major
business_hired <- subset(train, Hired == 1 & Major == "Business")
business_not_hired <- subset(train, Hired == 0 & Major == "Business")

cat("Quartiles for Business Hired students - GPA:\n")
print(quantile(business_hired$GPA, probs = c(0.25, 0.5, 0.75)))

cat("Quartiles for Business Not Hired students - GPA:\n")
print(quantile(business_not_hired$GPA, probs = c(0.25, 0.5, 0.75)))

# Analyzing CS students
cs_hired <- subset(train, Hired == 1 & Major == "CS")
cs_not_hired <- subset(train, Hired == 0 & Major == "CS")

cat("Quartiles for CS Hired students - GPA:\n")
print(quantile(cs_hired$GPA, probs = c(0.25, 0.5, 0.75)))

cat("Quartiles for CS Not Hired students - GPA:\n")
print(quantile(cs_not_hired$GPA, probs = c(0.25, 0.5, 0.75)))

# Define colors for visualization
colors <- c('red', 'blue', 'green', 'yellow')

# Boxplot of GPA vs. Hired
boxplot(GPA ~ Hired, data = train, xlab = "Hired", ylab = "GPA", 
        main = "Boxplot of Hired vs. GPA", col = colors, border = "black")

# Hired distribution table
cat("Hired Distribution:\n")
print(table(train$Hired))

# Hired distribution by Major
cat("Hired distribution by Major:\n")
print(table(train$Hired, train$Major))

# Hired distribution by Location
cat("Hired distribution by Location:\n")
print(table(train$Hired, train$Location))

# Feature Engineering
train$GPA_Percentile <- rank(train$GPA)/nrow(train)
train$Univ_GPA_Diff <- train$GPA - tapply(train$GPA, train$University, mean, na.rm=TRUE)[train$University]
train$Major_GPA_Diff <- train$GPA - tapply(train$GPA, train$Major, mean, na.rm=TRUE)[train$Major]

# Train decision tree model
hire_model <- rpart(Hired ~ GPA + Major + Location + GPA_Percentile + 
                      Univ_GPA_Diff + Major_GPA_Diff, 
                    data=train, method="class")

# Visualize the tree
rpart.plot(hire_model)

# Make predictions on training data
train_pred <- as.numeric(predict(hire_model, train)[,2] > 0.5)
train_accuracy <- mean(train_pred == train$Hired)
print(paste("Training Accuracy:", round(train_accuracy * 100, 2), "%"))

# Load test data
test <- read.csv("C:/Users/Krish/Documents/Data 101/Prediction Challenge 3/Prediction3Students.csv")

# Add Location for test data - with error checking
test$Location <- location$State[match(test$University, location$University)]
if(any(is.na(test$Location))) {
  warning("Some universities in test data don't have matching locations")
  # Use most common state as fallback
  most_common_state <- names(sort(table(train$Location), decreasing = TRUE))[1]
  test$Location[is.na(test$Location)] <- most_common_state
}

# Handle majors more carefully
all_train_majors <- unique(train$Major)
if(any(!(test$Major %in% all_train_majors))) {
  warning("New majors found in test data")
  test$Major[!(test$Major %in% all_train_majors)] <- "Other"
}

# Handle locations more carefully
all_train_locations <- unique(train$Location)
if(any(!(test$Location %in% all_train_locations))) {
  warning("New locations found in test data")
  most_common_location <- names(sort(table(train$Location), decreasing = TRUE))[1]
  test$Location[!(test$Location %in% all_train_locations)] <- most_common_location
}

# Ensure GPA is within valid range
test$GPA[test$GPA < 0] <- 0
test$GPA[test$GPA > 4] <- 4

# Convert to factors with proper error handling
train$Major <- as.factor(train$Major)
train$Location <- as.factor(train$Location)
test$Major <- factor(test$Major, levels = levels(train$Major))
test$Location <- factor(test$Location, levels = levels(train$Location))

# Apply same feature engineering to test data
test$GPA_Percentile <- rank(test$GPA)/nrow(test)
test$Univ_GPA_Diff <- test$GPA - tapply(test$GPA, test$University, mean, na.rm=TRUE)[test$University]
test$Major_GPA_Diff <- test$GPA - tapply(test$GPA, test$Major, mean, na.rm=TRUE)[test$Major]

# Make predictions
predictions <- predict(hire_model, test, type = "prob")[,2]
predictions <- ifelse(predictions > 0.5, 1, 0)

# Create simple submission dataframe
submission <- data.frame(
    ID = test$ID,
    Prediction = predictions
)

# Save submission file without row names
write.csv(submission, "submission.csv", row.names = FALSE)

# Print confirmation
cat("Submission file saved with", nrow(submission), "predictions\n")
print(table(submission$Prediction))
