# Install and load required packages
library(rpart)
library(rpart.plot)
library(caret)  

# Load training data
marriage_data <- read.csv("~/Data 101/prediction challenge 2/Prediction2025-2Train.csv")

# Add basic variables
marriage_data$Age_Diff <- marriage_data$Groom_Age - marriage_data$Bride_Age
marriage_data$Avg_Age <- (marriage_data$Groom_Age + marriage_data$Bride_Age) / 2
marriage_data$Same_Edu <- marriage_data$Groom_Edu == marriage_data$Bride_Edu
marriage_data$Same_MB <- marriage_data$Groom_MB == marriage_data$Bride_MB
marriage_data$Young_Couple <- marriage_data$Groom_Age < 30 & marriage_data$Bride_Age < 30

# Add detailed Myers-Briggs analysis
# Introversion/Extroversion (E/I)
marriage_data$Both_Introverted <- substr(marriage_data$Groom_MB, 1, 1) == "I" & 
  substr(marriage_data$Bride_MB, 1, 1) == "I"
marriage_data$Both_Extroverted <- substr(marriage_data$Groom_MB, 1, 1) == "E" & 
  substr(marriage_data$Bride_MB, 1, 1) == "E"
marriage_data$Mixed_IE <- substr(marriage_data$Groom_MB, 1, 1) != 
  substr(marriage_data$Bride_MB, 1, 1)

# Sensing/Intuition (S/N)
marriage_data$Both_Sensing <- substr(marriage_data$Groom_MB, 2, 2) == "S" & 
  substr(marriage_data$Bride_MB, 2, 2) == "S"
marriage_data$Both_Intuitive <- substr(marriage_data$Groom_MB, 2, 2) == "N" & 
  substr(marriage_data$Bride_MB, 2, 2) == "N"
marriage_data$Mixed_SN <- substr(marriage_data$Groom_MB, 2, 2) != 
  substr(marriage_data$Bride_MB, 2, 2)

# Thinking/Feeling (T/F)
marriage_data$Both_Thinking <- substr(marriage_data$Groom_MB, 3, 3) == "T" & 
  substr(marriage_data$Bride_MB, 3, 3) == "T"
marriage_data$Both_Feeling <- substr(marriage_data$Groom_MB, 3, 3) == "F" & 
  substr(marriage_data$Bride_MB, 3, 3) == "F"
marriage_data$Mixed_TF <- substr(marriage_data$Groom_MB, 3, 3) != 
  substr(marriage_data$Bride_MB, 3, 3)

# Judging/Perceiving (J/P)
marriage_data$Both_Judging <- substr(marriage_data$Groom_MB, 4, 4) == "J" & 
  substr(marriage_data$Bride_MB, 4, 4) == "J"
marriage_data$Both_Perceiving <- substr(marriage_data$Groom_MB, 4, 4) == "P" & 
  substr(marriage_data$Bride_MB, 4, 4) == "P"
marriage_data$Mixed_JP <- substr(marriage_data$Groom_MB, 4, 4) != 
  substr(marriage_data$Bride_MB, 4, 4)

# Add common successful combinations
marriage_data$NT_SF_Pair <- (substr(marriage_data$Groom_MB, 2, 3) == "NT" & 
                               substr(marriage_data$Bride_MB, 2, 3) == "SF") |
  (substr(marriage_data$Groom_MB, 2, 3) == "SF" & 
     substr(marriage_data$Bride_MB, 2, 3) == "NT")

marriage_data$NF_ST_Pair <- (substr(marriage_data$Groom_MB, 2, 3) == "NF" & 
                               substr(marriage_data$Bride_MB, 2, 3) == "ST") |
  (substr(marriage_data$Groom_MB, 2, 3) == "ST" & 
     substr(marriage_data$Bride_MB, 2, 3) == "NF")

# Add education and age variables
marriage_data$Groom_More_Edu <- marriage_data$Groom_Edu == "College" & 
  marriage_data$Bride_Edu == "HighSchool"
marriage_data$Bride_More_Edu <- marriage_data$Bride_Edu == "College" & 
  marriage_data$Groom_Edu == "HighSchool"
marriage_data$Large_Age_Gap <- marriage_data$Age_Diff > 5
marriage_data$Very_Young_Marriage <- marriage_data$Bride_Age < 25 & 
  marriage_data$Groom_Age < 25

# Create enhanced decision tree model
marriage_tree <- rpart(Outcome ~ Groom_MB + Bride_MB + Groom_Edu + Bride_Edu + 
                         Groom_Age + Bride_Age + Age_Diff + Avg_Age + Same_Edu + 
                         Same_MB + Young_Couple + Both_Introverted + Both_Extroverted + 
                         Mixed_IE + Both_Sensing + Both_Intuitive + Mixed_SN + 
                         Both_Thinking + Both_Feeling + Mixed_TF + Both_Judging + 
                         Both_Perceiving + Mixed_JP + NT_SF_Pair + NF_ST_Pair + 
                         Groom_More_Edu + Bride_More_Edu + Large_Age_Gap + 
                         Very_Young_Marriage,
                       data=marriage_data, 
                       method="class",
                       control=rpart.control(maxdepth=8, minsplit=20, cp=0.01))

# Print model info and plot
printcp(marriage_tree)
rpart.plot(marriage_tree)

# Load test data for predictions
marriage_data_submission <- read.csv("~/Data 101/prediction challenge 2/Prediction2025-2TestStud.csv")

# Add basic variables
marriage_data_submission$Age_Diff <- marriage_data_submission$Groom_Age - marriage_data_submission$Bride_Age
marriage_data_submission$Avg_Age <- (marriage_data_submission$Groom_Age + marriage_data_submission$Bride_Age) / 2
marriage_data_submission$Same_Edu <- marriage_data_submission$Groom_Edu == marriage_data_submission$Bride_Edu
marriage_data_submission$Same_MB <- marriage_data_submission$Groom_MB == marriage_data_submission$Bride_MB
marriage_data_submission$Young_Couple <- marriage_data_submission$Groom_Age < 30 & marriage_data_submission$Bride_Age < 30

# Myers-Briggs personality analysis
marriage_data_submission$Both_Introverted <- substr(marriage_data_submission$Groom_MB, 1, 1) == "I" & 
  substr(marriage_data_submission$Bride_MB, 1, 1) == "I"
marriage_data_submission$Both_Extroverted <- substr(marriage_data_submission$Groom_MB, 1, 1) == "E" & 
  substr(marriage_data_submission$Bride_MB, 1, 1) == "E"
marriage_data_submission$Mixed_IE <- substr(marriage_data_submission$Groom_MB, 1, 1) != 
  substr(marriage_data_submission$Bride_MB, 1, 1)

# Sensing/Intuition (S/N)
marriage_data_submission$Both_Sensing <- substr(marriage_data_submission$Groom_MB, 2, 2) == "S" & 
  substr(marriage_data_submission$Bride_MB, 2, 2) == "S"
marriage_data_submission$Both_Intuitive <- substr(marriage_data_submission$Groom_MB, 2, 2) == "N" & 
  substr(marriage_data_submission$Bride_MB, 2, 2) == "N"
marriage_data_submission$Mixed_SN <- substr(marriage_data_submission$Groom_MB, 2, 2) != 
  substr(marriage_data_submission$Bride_MB, 2, 2)

# Thinking/Feeling (T/F)
marriage_data_submission$Both_Thinking <- substr(marriage_data_submission$Groom_MB, 3, 3) == "T" & 
  substr(marriage_data_submission$Bride_MB, 3, 3) == "T"
marriage_data_submission$Both_Feeling <- substr(marriage_data_submission$Groom_MB, 3, 3) == "F" & 
  substr(marriage_data_submission$Bride_MB, 3, 3) == "F"
marriage_data_submission$Mixed_TF <- substr(marriage_data_submission$Groom_MB, 3, 3) != 
  substr(marriage_data_submission$Bride_MB, 3, 3)

# Judging/Perceiving (J/P)
marriage_data_submission$Both_Judging <- substr(marriage_data_submission$Groom_MB, 4, 4) == "J" & 
  substr(marriage_data_submission$Bride_MB, 4, 4) == "J"
marriage_data_submission$Both_Perceiving <- substr(marriage_data_submission$Groom_MB, 4, 4) == "P" & 
  substr(marriage_data_submission$Bride_MB, 4, 4) == "P"
marriage_data_submission$Mixed_JP <- substr(marriage_data_submission$Groom_MB, 4, 4) != 
  substr(marriage_data_submission$Bride_MB, 4, 4)

# Common personality combinations
marriage_data_submission$NT_SF_Pair <- (substr(marriage_data_submission$Groom_MB, 2, 3) == "NT" & 
                                          substr(marriage_data_submission$Bride_MB, 2, 3) == "SF") |
  (substr(marriage_data_submission$Groom_MB, 2, 3) == "SF" & 
     substr(marriage_data_submission$Bride_MB, 2, 3) == "NT")

marriage_data_submission$NF_ST_Pair <- (substr(marriage_data_submission$Groom_MB, 2, 3) == "NF" & 
                                          substr(marriage_data_submission$Bride_MB, 2, 3) == "ST") |
  (substr(marriage_data_submission$Groom_MB, 2, 3) == "ST" & 
     substr(marriage_data_submission$Bride_MB, 2, 3) == "NF")

# Education and age differences
marriage_data_submission$Groom_More_Edu <- marriage_data_submission$Groom_Edu == "College" & 
  marriage_data_submission$Bride_Edu == "HighSchool"
marriage_data_submission$Bride_More_Edu <- marriage_data_submission$Bride_Edu == "College" & 
  marriage_data_submission$Groom_Edu == "HighSchool"
marriage_data_submission$Large_Age_Gap <- marriage_data_submission$Age_Diff > 5
marriage_data_submission$Very_Young_Marriage <- marriage_data_submission$Bride_Age < 25 & 
  marriage_data_submission$Groom_Age < 25

# Make predictions on the test data
predictions_submission <- predict(marriage_tree, marriage_data_submission, type="class")

# Load the submission template
submission <- read.csv("~/Data 101/prediction challenge 2/Prediction2025-2submission.csv")

# Add predictions to the submission template
submission$Outcome <- predictions_submission

# Save the predictions back to the original submission file
write.csv(submission, "~/Data 101/prediction challenge 2/Prediction2025-2submission.csv", row.names = FALSE)

# Print confirmation
print("Predictions added to submission file successfully!")