library(arsenal)
library(readr)
library(dplyr)

setwd("/Users/ravibandaru/Desktop/hnscc_pembro_cs131_trial/Main_Tables/T1")
file_path <- '../../Data/Demographics_HNSCC_CP.csv'
df <- read_csv(file_path)

selected_columns <- c("Subject ID", "Site", "Age", "Gender", "Race", "Ethnicity", "Smoking", "Alcohol", "Primary Disease Site", "Clinical Tumor Stage (Initial Diagnosis)")
df_selected <- df[selected_columns]
df_selected <- distinct(df_selected)
selected_columns <- c("Site", "Age", "Gender", "Smoking", "Alcohol", "Race", "Ethnicity", "Primary Disease Site", "Clinical Tumor Stage (Initial Diagnosis)")
df_selected <- df_selected[selected_columns]

# Set factor order for smoking and alcohol so categories display consistently
df_selected$Smoking <- factor(df_selected$Smoking,
  levels = c("Current smoker", "Former smoker", "Never smoker", "Marijuana", "Betel nut"))
df_selected$Alcohol <- factor(df_selected$Alcohol,
  levels = c("Current drinker", "Former drinker", "Non-drinker"))

df_selected$`Site` <- factor(df_selected$`Site`, levels = c("University of Cincinnati", "Jefferson"))

summary_table <- tableby(`Site` ~ ., data = df_selected)

sink("./hnscc_metadata_summary_stratified_by_site.md")
print(summary(summary_table))
sink()

summary_table_all <- tableby(~ ., data = df_selected)

sink("./hnscc_metadata_summary_no_stratification.md")
print(summary(summary_table_all))
sink()