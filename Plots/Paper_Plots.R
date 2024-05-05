library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales) 
library(reshape2)

data <- read.csv("Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Model_RMSEs.csv")

data



cols_to_keep <- c("Missing_Count", grep("^AVG", names(data), value = TRUE))
cols_to_keep <- cols_to_keep[!cols_to_keep %in% c("AVG_AA_RMSE")]
data_melted <- melt(data[, cols_to_keep], id.vars = "Missing_Count")

#filtered_data <- data|>
#  filter(Missing_Count %in% c(4,6,8,12,24))

filtered_data <- data |>
  filter(Missing_Count %in% c(5))
filtered_data

data_melted <- melt(filtered_data[, cols_to_keep], id.vars = "Missing_Count")

data_melted

# Plot with multiple bar plots for each Missing_Count, excluding AVG_AA_RMSE
ggplot(data_melted, aes(x = variable, y = value, fill = variable)) +
  geom_col(position = "dodge") +
  labs(x = "Method",
       y = "AD-RMSE",
       fill = "Method") +
  theme_minimal() +
  scale_fill_manual(values = c("AVG_Spline_RMSE" = "#87CEEB",
                               "AVG_Linear_RMSE" = "#6495ED",
                               "AVG_Stine_RMSE" = "#367588",
                               "AVG_KF_RMSE" = "#1E90FF",
                               "AVG_NN_RMSE" = "#2A52BE",
                               "AVG_LOCF_RMSE" = "#00008B"),
                    labels = c("AVG_Spline_RMSE" = "CSI",
                               "AVG_Linear_RMSE" = "LI",
                               "AVG_Stine_RMSE" = "SI",
                               "AVG_KF_RMSE" = "KS",
                               "AVG_NN_RMSE" = "NN",
                               "AVG_LOCF_RMSE" = "LOCF"))+
  scale_x_discrete(labels = c("AVG_Spline_RMSE" = "CSI",
                              "AVG_Linear_RMSE" = "LI",
                              "AVG_Stine_RMSE" = "SI",
                              "AVG_KF_RMSE" = "KS",
                              "AVG_NN_RMSE" = "NN",
                              "AVG_LOCF_RMSE" = "LOCF")) +
  geom_hline(yintercept = 2.63, color = "#E57200", linetype = "dashed", size = 1)+
  annotate("text", x = 6.5, y = 12, label = "Baseline", vjust = -0.5, hjust = 1.1, color="#E57200")+
  guides(fill = FALSE)



model_data <- read.csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Model_Predictions.csv')

missing_start_index <- model_data|>
  filter(is.na(Value_Missing))|>
  filter(Missing_Count == 5)|>
  group_by(SID,Day)|>
  summarise(missing_start = min(DailyTI_Index))

seq_missing_model <- model_data |>
  left_join(missing_start_index, by = c('SID','Day'))|>
  filter(is.na(Value_Missing))|>
  mutate(seq_missing_index = DailyTI_Index - missing_start)

model <- seq_missing_model|>
  filter(is.na(Value_Missing))|>
  filter(Missing_Count == 5)|>
  mutate(NN_SE = (Value_Complete-NN_Predictions)**2)|>
  mutate(Spline_SE = (Value_Complete-spline_predictions)**2)|>
  mutate(Linear_SE = (Value_Complete-linear_predictions)**2)|>
  mutate(Stine_SE = (Value_Complete-stine_predictions)**2)|>
  mutate(KF_SE = (Value_Complete-KF_predictions)**2)|>
  mutate(LOCF_SE = (Value_Complete-locf_predictions)**2)|>
  select(SID, Day, DailyTI_Index, seq_missing_index,NN_SE, Spline_SE,Linear_SE,Stine_SE,KF_SE,LOCF_SE)|>
  group_by(seq_missing_index)|>
  summarise(NN_RMSE = sqrt(mean(NN_SE))
            ,Spline_RMSE = sqrt(mean(Spline_SE))
            ,Linear_RMSE = sqrt(mean(Linear_SE))
            ,Stine_RMSE = sqrt(mean(Stine_SE))
            ,KF_RMSE = sqrt(mean(KF_SE, na.rm = T))
            ,LOCF_RMSE = sqrt(mean(LOCF_SE))
  )

melt(model, id.vars='seq_missing_index', variable.name='Method', value.name='RMSE')|>
  mutate(seq_missing_index = seq_missing_index+1)|>
  group_by(Method)|>
  summarise(MT_RMSE=max(RMSE))|>
  ggplot(aes(x = Method, y = MT_RMSE, fill = Method)) +
  geom_col(position = "dodge") +
  labs(x = "Method",
       y = "MT-RMSE",
       fill = "Method") +
  theme_minimal() +
  scale_fill_manual(values = c("Spline_RMSE" = "#87CEEB",
                               "Linear_RMSE" = "#6495ED",
                               "Stine_RMSE" = "#367588",
                               "KF_RMSE" = "#1E90FF",
                               "NN_RMSE"="#2A52BE",
                               "LOCF_RMSE" ="#00008B" 
  ),
  labels = c("Spline_RMSE" = "CSI",
             "Linear_RMSE" = "LI",
             "Stine_RMSE" = "SI",
             "KF_RMSE" = "KS",
             "NN_RMSE"="NN",
             "LOCF_RMSE" ="LOCF"
  ))+
  scale_x_discrete(labels = c("Spline_RMSE" = "CSI",
                              "Linear_RMSE" = "LI",
                              "Stine_RMSE" = "SI",
                              "KF_RMSE" = "KS",
                              "NN_RMSE" = "NN",
                              "LOCF_RMSE" = "LOCF"))+
  theme(
    legend.direction = "horizontal",
    legend.position = c(0, 1),  # Place legend inside the plot area, coordinates are normalized (0,0) is bottom-left, (1,1) is top-right
    legend.justification = c(0, 1),  # Anchor the legend at the top-left of its bounding box
    legend.key = element_blank(),  # Optional: remove keys background if preferred
    plot.margin = margin(10, 10, 10, 10),
    #panel.grid.major = element_blank(),  # Remove major grid lines
    #panel.grid.minor = element_blank() 
  )+
  geom_hline(yintercept = 4.33, color = "#E57200", linetype = "dashed", size = 1)+
  annotate("text", x = 1.5, y = 23, label = "Baseline", vjust = -0.5, hjust = 1.1, color="#E57200") +
  guides(fill = FALSE)
