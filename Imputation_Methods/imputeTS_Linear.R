library(imputeTS)
library(dplyr)
library(tidyverse)

test_set <- read.csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Glucose_Test_Set.csv')
test_complete <- read.csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Glucose_Test_Set_Complete.csv')

combined <- left_join(test_complete, test_set, by =c('SID','Day','Hour','Min'
                                                  , 'DT_Index','DailyTI_Index'
                                                  ,'StudyTI_Index','BMI'
                                                  ,'BMI_Class','Tx_Modality'))

combined

test_joined <- select(combined,-c('filter'))|>
  rename(Value_Missing = Value.y
         ,Value_Complete = Value.x )

test_missing_lists <- test_joined |>
  arrange(DailyTI_Index) |>
  group_by(SID, Day) |>
  summarise(Value_Missing = list(Value_Missing), .groups = 'drop')|>
  ungroup()

test_complete_lists <- test_joined |>
  arrange(DailyTI_Index) |>
  group_by(SID, Day) |>
  summarise(Value_Complete = list(Value_Complete), .groups = 'drop')|>
  ungroup()


test_missing_lists$linear_predictions <- lapply(test_missing_lists$Value_Missing, function(x) na_interpolation(x, option = 'linear'))

linear_interp <- test_missing_lists|>
  rowwise() |>
  mutate(na_indices = list(which(is.na(Value_Missing)))) |>
  ungroup()

eval_data <- inner_join(linear_interp, test_complete_lists, by =c('SID','Day'))

#change
linear_data <- eval_data |>
  unnest(linear_predictions)|>
  group_by(SID,Day) |>
  mutate(DailyTI_Index = row_number()-1) |>
  ungroup()|>
  select(SID,Day,DailyTI_Index,linear_predictions)

#change
write_csv(linear_data, './Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Linear_Test_Prediction.csv')

  
