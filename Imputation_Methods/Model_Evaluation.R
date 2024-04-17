library(tidyverse)

spline_data <- read.csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Spline_Test_Prediction.csv')
NN_test_data <- read.csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Final_Seq2Seq_Test_Prediction.csv')
linear_data <- read.csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Linear_Test_Prediction.csv')
stine_data <- read.csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Stine_Test_Prediction.csv')
kf_data <- read.csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/KF_Test_Prediction.csv')
arima_data <- read.csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/ARIMA_Test_Prediction.csv')
locf_data <- read.csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Locf_Test_Prediction.csv')

spline_nn_data <- inner_join(spline_data,NN_test_data, by = join_by(SID, Day, DailyTI_Index))
linear_spline_nn <- inner_join(spline_nn_data,linear_data, by = join_by(SID, Day, DailyTI_Index))
linear_spline_nn_stine <- inner_join(linear_spline_nn,stine_data, by = join_by(SID, Day, DailyTI_Index))
linear_spline_nn_stine_a <- inner_join(linear_spline_nn_stine,arima_data, by = join_by(SID, Day, DailyTI_Index))
linear_spline_nn_st_a_kf <- left_join(linear_spline_nn_stine_a, kf_data, by = join_by(SID, Day, DailyTI_Index))
linear_spline_nn_st_a_kf_lo <- inner_join(linear_spline_nn_st_a_kf, locf_data, by = join_by(SID, Day, DailyTI_Index))

linear_spline_nn_st_a_kf_lo|>
  select(SID,Day,Missing_Count)|>
  distinct()|>
  group_by(Missing_Count)|>
  tally()

linear_spline_nn_st_a_kf_lo |>
  select(c(-NN_Sequence))|>
  write_csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Model_Predictions.csv')

model_perf <- linear_spline_nn_st_a_kf_lo|>
  filter(is.na(Value_Missing))|>
  mutate(NN_SE = (Value_Complete-NN_Predictions)**2)|>
  mutate(Spline_SE = (Value_Complete-spline_predictions)**2)|>
  mutate(Linear_SE = (Value_Complete-linear_predictions)**2)|>
  mutate(Stine_SE = (Value_Complete-stine_predictions)**2)|>
  mutate(KF_SE = (Value_Complete-KF_predictions)**2)|>
  mutate(AA_SE = (Value_Complete-ARIMA_predictions)**2)|>
  mutate(LOCF_SE = (Value_Complete-locf_predictions)**2)|>
  select(SID, Day, DailyTI_Index, Missing_Count,NN_SE,Spline_SE,Linear_SE,Stine_SE,KF_SE,AA_SE,LOCF_SE)|>
  group_by(SID,Day,Missing_Count)|>
  summarise(NN_RMSE = sqrt(mean(NN_SE))
            ,Spline_RMSE = sqrt(mean(Spline_SE))
            ,Linear_RMSE = sqrt(mean(Linear_SE))
            ,Stine_RMSE = sqrt(mean(Stine_SE))
            ,KF_RMSE = sqrt(mean(KF_SE, na.rm = T))
            ,AA_RMSE = sqrt(mean(AA_SE))
            ,LOCF_RMSE = sqrt(mean(LOCF_SE))
  )|>
  group_by(Missing_Count)|>
  summarise(AVG_NN_RMSE = mean(NN_RMSE)
            ,AVG_Spline_RMSE= mean(Spline_RMSE)
            ,AVG_Linear_RMSE= mean(Linear_RMSE)
            ,AVG_Stine_RMSE= mean(Stine_RMSE)
            ,AVG_KF_RMSE = mean(KF_RMSE,na.rm = T)
            ,AVG_AA_RMSE = mean(AA_RMSE)
            ,AVG_LOCF_RMSE = mean(LOCF_RMSE)
            ,SD_NN_RMSE= sd(NN_RMSE)
            ,SD_Spline_RMSE= sd(Spline_RMSE)
            ,SD_Linear_RMSE= sd(Linear_RMSE)
            ,SD_Stine_RMSE= sd(Stine_RMSE)
            ,SD_KF_RMSE = sd(KF_RMSE,na.rm = T)
            ,SD_AA_RMSE = sd(AA_RMSE)
            ,SD_LOCF_RMSE = sd(LOCF_RMSE)
  )


model_perf|>
  select(Missing_Count,AVG_KF_RMSE,AVG_Spline_RMSE,AVG_Stine_RMSE)|>
  

model_perf|>
  write_csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Model_RMSEs.csv')
