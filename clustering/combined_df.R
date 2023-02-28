library(dplyr)


colnames(matched_data)


subset_matched_data <- matched_data[c("X", "eid", "Age_recr.0.0", "Sex.0.0", "BMI.0.0", "Mood_swings.0.0", "Smoking_status.0.0", "Alc_drinker_status.0.0",
                                      "C_reactive_protein.0.0", "Traff_int_major_rd.0.0", "housing_score", "health_score", "case_leuk", "case_lymph",
                                      "case_status", "cancer")]

print(subset_matched_data)


# Add new column X with sequential numbering
train_clustered <- mutate(train_clustered, X = 1:nrow(train_clustered))

print(train_clustered)


train_clustered <- select(train_clustered, -...1)


# Merge the datasets based on the common column X
clustered_data <- merge(subset_matched_data, train_clustered, by = "X")
print(clustered_data)


# Save the merged dataset
write.csv(clustered_data, "clustered_data.csv")
