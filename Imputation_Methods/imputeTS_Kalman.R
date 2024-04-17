library(imputeTS)
library(dplyr)
library(tidyverse)

test_set <- read.csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Glucose_Test_Set.csv')
test_complete <- read.csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Glucose_Test_Set_Complete.csv')

combined <- left_join(test_complete, test_set, by =c('SID','Day','Hour','Min'
                                                  , 'DT_Index','DailyTI_Index'
                                                  ,'StudyTI_Index','BMI'
                                                  ,'BMI_Class','Tx_Modality'))

test_joined <- select(combined,-c('filter'))|>
  rename(Value_Missing = Value.y
         ,Value_Complete = Value.x )


scale_mean <- mean(test_joined$Value_Missing, na.rm = TRUE)
scale_sd <- sd(test_joined$Value_Missing, na.rm = TRUE)

test_joined$Value_Missing <- as.vector(scale(test_joined$Value_Missing))


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

loop_df <- test_joined|>
  select(SID,Day)|>
  distinct()

vec_kalman <-c()
counter <- 0
for (i in 1:nrow(loop_df)){
  counter <- counter +1
  imputed <- test_joined|>
    filter(SID == loop_df[i,'SID'], Day ==loop_df[i,'Day'])|>
    select(Value_Missing,DailyTI_Index)
  kf_imputed <- imputed |> 
    na_kalman()
  vec_kalman<- c(vec_kalman, kf_imputed$Value_Missing)
}

test_joined$KF_predictions <- vec_kalman
test_joined <- test_joined|>
  mutate(KF_predictions= KF_predictions*scale_sd + scale_mean)

kf_na <- test_joined|>
  select(SID,Day,DailyTI_Index,KF_predictions)|>
  filter(is.na(KF_predictions))|>
  select(SID, Day)|>
  distinct()

nrow(kf_na)

anti_join(test_joined, kf_na, by = c("SID", "Day"))|>
  select(SID,Day,DailyTI_Index,KF_predictions)|>
  write_csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/KF_Test_Prediction.csv')
