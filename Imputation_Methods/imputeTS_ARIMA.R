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
    select(Value_Missing)
  arima_imputed <- imputed |> 
    na_kalman(model = 'auto.arima')
  vec_arima<- c(vec_kalman, arima_imputed$Value_Missing)
}

test_joined$ARIMA_predictions <- vec_arima

arima_na <- test_joined|>
  select(SID,Day,DailyTI_Index,ARIMA_predictions)|>
  filter(is.na(ARIMA_predictions))|>
  select(SID, Day)|>
  distinct()

nrow(arima_na)

test_joined|>
  select(SID,Day,DailyTI_Index,ARIMA_predictions)|>
  write_csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/ARIMA_Test_Prediction.csv')
