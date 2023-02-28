library(dplyr)


colnames(matched_data)


subset_matched_data <- matched_data[c("eid", "Age_recr.0.0", "Sex.0.0", "BMI.0.0", "Mood_swings.0.0", "Smoking_status.0.0", "Alc_drinker_status.0.0",
                                      "C_reactive_protein.0.0", "Traff_int_major_rd.0.0", "housing_score", "health_score", "case_leuk", "case_lymph",
                                      "case_status", "cancer")]

print(subset_matched_data)

print(train_clustered)

train_clustered <- select(train_clustered, -...1)

# Combine the datasets horizontally
train_clustered <- cbind(subset_matched_data, train_clustered)

print(train_clustered)

# Save the merged dataset
write.csv(train_clustered, "clustered_data.csv")

