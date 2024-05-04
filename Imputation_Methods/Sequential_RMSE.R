library(tidyverse)

baseline_data <- read.csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Linear_Baseline_Prediction.csv')

missing_start_index <- baseline_data|>
  filter(is.na(Value_Missing))|>
  group_by(SID,Day)|>
  summarise(missing_start = min(DailyTI_Index))

seq_missing_baseline <- baseline_data |>
  left_join(missing_start_index, by = c('SID','Day'))|>
  filter(is.na(Value_Missing))|>
  mutate(seq_missing_index = DailyTI_Index - missing_start)

baseline <- seq_missing_baseline|>
  mutate(SE = (Value_Complete - linear_predictions)**2)|>
  group_by(seq_missing_index)|>
  summarise(RMSE = sqrt(mean(SE)))


model_data <- read.csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Model_Predictions.csv')

missing_start_index <- model_data|>
  filter(is.na(Value_Missing))|>
  filter(Missing_Count == 4)|>
  group_by(SID,Day)|>
  summarise(missing_start = min(DailyTI_Index))

seq_missing_model <- model_data |>
  left_join(missing_start_index, by = c('SID','Day'))|>
  filter(is.na(Value_Missing))|>
  mutate(seq_missing_index = DailyTI_Index - missing_start)

model <- seq_missing_model|>
  filter(is.na(Value_Missing))|>
  filter(Missing_Count == 4)|>
  mutate(Spline_SE = (Value_Complete-spline_predictions)**2)|>
  mutate(Linear_SE = (Value_Complete-linear_predictions)**2)|>
  mutate(Stine_SE = (Value_Complete-stine_predictions)**2)|>
  mutate(KF_SE = (Value_Complete-KF_predictions)**2)|>
  select(SID, Day, DailyTI_Index, seq_missing_index,Spline_SE,Linear_SE,Stine_SE,KF_SE)|>
  group_by(seq_missing_index)|>
  summarise(Spline_RMSE = sqrt(mean(Spline_SE))
            ,Linear_RMSE = sqrt(mean(Linear_SE))
            ,Stine_RMSE = sqrt(mean(Stine_SE))
            ,KF_RMSE = sqrt(mean(KF_SE, na.rm = T))
  )

melt(model, id.vars='seq_missing_index', variable.name='Method', value.name='RMSE')|>
  mutate(seq_missing_index = seq_missing_index+1)|>
  ggplot(aes(x = factor(seq_missing_index), y = RMSE, fill = Method)) +
  geom_col(position = "dodge") +
  labs(x = "Missing Reading",
       y = "RMSE",
       fill = "Method") +
  theme_minimal() +
  scale_fill_manual(values = c("Spline_RMSE" = "#87CEEB",
                               "Linear_RMSE" = "#6495ED",
                               "Stine_RMSE" = "#367588",
                               "KF_RMSE" = "#1E90FF"),
                    labels = c("Spline_RMSE" = "CSI",
                               "Linear_RMSE" = "LI",
                               "Stine_RMSE" = "SI",
                               "KF_RMSE" = "KS"
                               ))+
  theme(
    legend.direction = "horizontal",
    legend.position = c(0, 1),  # Place legend inside the plot area, coordinates are normalized (0,0) is bottom-left, (1,1) is top-right
    legend.justification = c(0, 1),  # Anchor the legend at the top-left of its bounding box
    legend.key = element_blank(),  # Optional: remove keys background if preferred
    plot.margin = margin(10, 10, 10, 10),
    panel.grid.major = element_blank()  # Remove major grid lines
    #panel.grid.minor = element_blank() 
  )+
  geom_hline(yintercept = 4.33, color = "#E57200", linetype = "dashed", size = 1)+
  annotate("text", x = 5, y = 10, label = "Max RMSE LI 3 MR", vjust = -0.5, hjust = 1.1, color="#E57200") 





baseline
