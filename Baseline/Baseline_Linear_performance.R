library(tidyverse)

linear_data <- read.csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Linear_Baseline_Prediction.csv')

linear_data$Missing_Count <- 3


linear_data|>
  select(SID,Day,Missing_Count)|>
  distinct()|>
  group_by(Missing_Count)|>
  tally()

linear_data|>
  filter(is.na(Value_Missing))|>
  mutate(SE = (Value_Complete-linear_predictions)**2)|>
  select(SID, Day, DailyTI_Index, Missing_Count,SE)|>
  group_by(SID,Day,Missing_Count)|>
  summarise(RMSE = sqrt(mean(SE)))|>
  group_by(Missing_Count)|>
  summarise(AVG_Baseline_RMSE = mean(RMSE)
            ,SD_Baseline_RMSE = sd(RMSE)
  )



