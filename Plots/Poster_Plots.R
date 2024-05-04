library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales) 
library(reshape2)

convert_time <- function(time_index) {
  # Calculate hours and minutes from the index
  hours <- time_index %/% 12
  minutes <- (time_index %% 12) * 5
  
  # Format as HH:MM string
  time_string <- sprintf("%02d:%02d", hours, minutes)
  # Convert to a POSIXct time object if needed
  as.POSIXct(time_string, format = "%H:%M", tz = "UTC")
}


missing_data <- read_csv('./Documents/UVA_MSDS/Capstones/Glucose/CGM_data/hall18_cgm_plus_features.csv')

missing_shell <- missing_data|>
  select(SID,Day)|>
  distinct()|>
  uncount(288)|>
  group_by(SID,Day)|>
  mutate(DailyTI_Index = row_number() - 1) %>%
  ungroup()


missing_ex <- left_join(missing_shell, missing_data, by= join_by(SID, Day, DailyTI_Index))|>
  mutate(ToD = convert_time(DailyTI_Index))

missing_obs <- missing_ex|>
  select(SID, Day)|>
  distinct()
 
 
ob <-11
missing_ex|>
  filter(SID == missing_obs$SID[ob])|>
  filter(Day == missing_obs$Day[ob])|>
  ggplot(aes(x = ToD, y = Value)) +
  geom_line() + # This automatically creates gaps for NA values
  geom_point(size = 0.5) +
  scale_x_datetime(labels = date_format("%H:%M"), breaks = date_breaks("4 hours")) +
  labs(x = "Time", y = "Blood Glucose") +
  theme_minimal()  


prediction_sequences <- sequences|>mutate(ToD = convert_time(DailyTI_Index))

seq_plot <- prediction_sequences|>
  rename(CSI = spline_predictions
         ,NN = NN_Predictions
         ,LI = linear_predictions
         ,SI = stine_predictions
         ,KS = KF_predictions
         ,LOCF = locf_predictions
         
         )|>
  select(SID, Day, DailyTI_Index, Value_Missing, Value_Complete, CSI, LI, SI, KS, NN, LOCF, ToD)

distinct_seq <- seq_plot|>
  select(SID,Day)|>
  distinct()

ob <- 56

daily_seq <- seq_plot |>
  filter(Day == distinct_seq$Day[ob])|>
  filter(SID == distinct_seq$SID[ob])

join_df<- daily_seq|>
  select(Day, SID, DailyTI_Index, Value_Complete, ToD)|>
  rename(Actual = Value_Complete)

missing_seq <- daily_seq|>
  filter(is.na(Value_Missing))|>
  select(-c(Value_Complete,ToD))

plot_seq <- left_join(join_df, missing_seq, by= join_by(SID, Day, DailyTI_Index))

plot_seq

ggplot(plot_seq, aes(x = ToD)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = CSI, color = "CSI")) +
  geom_line(aes(y = LI, color = "LI")) +
  geom_line(aes(y = SI, color = "SI")) +
  geom_line(aes(y = KS, color = "KS")) +
  geom_line(aes(y = NN, color = "NN")) +
  geom_line(aes(y = LOCF, color = "LOCF")) +
  scale_color_manual(values = c("Actual" = "black", "CSI" = "#87CEEB", "LI" = "#6495ED",
                                "SI" = "#367588", "KS" = "#1E90FF", "NN" = "#2A52BE", "LOCF" = "#00008B")) +
  scale_x_datetime(labels = date_format("%H:%M"), breaks = date_breaks("4 hours")) +
  labs(x = "Time", y = "Blood Glucose", color = "Method") +
  theme_minimal() +
  theme(
    legend.direction = "horizontal",
    legend.position = c(0, 1),  # Place legend inside the plot area, coordinates are normalized (0,0) is bottom-left, (1,1) is top-right
    legend.justification = c(0, 1),  # Anchor the legend at the top-left of its bounding box
    legend.key = element_blank(),  # Optional: remove keys background if preferred
    plot.margin = margin(10, 10, 10, 10),
    panel.grid.major = element_blank()  # Remove major grid lines
    #panel.grid.minor = element_blank()   # Remove minor grid lines# Ensure there's a margin around the plot to not clip the legend
  )


plot_seq|>
  filter(DailyTI_Index>=80)|>
  filter(DailyTI_Index<=150)|>
  ggplot(aes(x = ToD)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = CSI, color = "CSI")) +
  geom_line(aes(y = LI, color = "LI")) +
  geom_line(aes(y = SI, color = "SI")) +
  geom_line(aes(y = KS, color = "KS")) +
  geom_line(aes(y = NN, color = "NN")) +
  geom_line(aes(y = LOCF, color = "LOCF")) +
  scale_color_manual(values = c("Actual" = "black", "CSI" = "#87CEEB", "LI" = "#6495ED",
                                "SI" = "#367588", "KS" = "#1E90FF", "NN" = "#2A52BE", "LOCF" = "#00008B")) +
  scale_x_datetime(labels = date_format("%H:%M"), breaks = date_breaks("1 hours")) +
  labs(x = "Time", y = "Blood Glucose", color = "Method") +
  theme_minimal() +
  theme(
    legend.direction = "horizontal",
    legend.position = c(0, 1),  # Place legend inside the plot area, coordinates are normalized (0,0) is bottom-left, (1,1) is top-right
    legend.justification = c(0, 1),  # Anchor the legend at the top-left of its bounding box
    legend.key = element_blank(),  # Optional: remove keys background if preferred
    plot.margin = margin(10, 10, 10, 10),
    panel.grid.major = element_blank()  # Remove major grid lines
    #panel.grid.minor = element_blank()   # Remove minor grid lines# Ensure there's a margin around the plot to not clip the legend
  )







ob <- 101

daily_seq <- seq_plot |>
  filter(Day == distinct_seq$Day[ob])|>
  filter(SID == distinct_seq$SID[ob])

join_df<- daily_seq|>
  select(Day, SID, DailyTI_Index, Value_Complete, ToD)|>
  rename(Actual = Value_Complete)

missing_seq <- daily_seq|>
  filter(is.na(Value_Missing))|>
  select(-c(Value_Complete,ToD))

plot_seq <- left_join(join_df, missing_seq, by= join_by(SID, Day, DailyTI_Index))

plot_seq

ggplot(plot_seq, aes(x = ToD)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = CSI, color = "CSI")) +
  geom_line(aes(y = LI, color = "LI")) +
  geom_line(aes(y = SI, color = "SI")) +
  geom_line(aes(y = KS, color = "KS")) +
  geom_line(aes(y = NN, color = "NN")) +
  geom_line(aes(y = LOCF, color = "LOCF")) +
  scale_color_manual(values = c("Actual" = "black", "CSI" = "#87CEEB", "LI" = "#6495ED",
                                "SI" = "#367588", "KS" = "#1E90FF", "NN" = "#2A52BE", "LOCF" = "#00008B")) +
  scale_x_datetime(labels = date_format("%H:%M"), breaks = date_breaks("4 hours")) +
  labs(x = "Time", y = "Blood Glucose", color = "Method") +
  theme_minimal() +
  theme(
    legend.direction = "vertical",
    legend.position = c(1, 1),  # Place legend inside the plot area, coordinates are normalized (0,0) is bottom-left, (1,1) is top-right
    legend.justification = c(1, 1),  # Anchor the legend at the top-left of its bounding box
    legend.key = element_blank(),  # Optional: remove keys background if preferred
    plot.margin = margin(10, 10, 10, 10),
    panel.grid.major = element_blank()  # Remove major grid lines
    #panel.grid.minor = element_blank()   # Remove minor grid lines# Ensure there's a margin around the plot to not clip the legend
  )





data <- read.csv("Documents/UVA_MSDS/Capstones/Glucose/CGM_data/Model_RMSEs.csv")

data



cols_to_keep <- c("Missing_Count", grep("^AVG", names(data), value = TRUE))
cols_to_keep <- cols_to_keep[!cols_to_keep %in% c("AVG_AA_RMSE")]
data_melted <- melt(data[, cols_to_keep], id.vars = "Missing_Count")

#filtered_data <- data|>
#  filter(Missing_Count %in% c(4,6,8,12,24))

filtered_data <- data |>
  filter(Missing_Count %in% c(4, 6, 8, 12,24)) |>
  mutate(Missing_Count = case_when(
    Missing_Count == 4  ~ "20 Min",
    Missing_Count == 6  ~ "30 Min",
    Missing_Count == 8  ~ "40 Min",
    Missing_Count == 12 ~ "1 Hour",
    Missing_Count == 24 ~ "2 Hours",
    TRUE ~ as.character(Missing_Count)  # Default case for other values
  )) |>
  mutate(Missing_Count = factor(Missing_Count, levels = c("20 Min", "30 Min", "40 Min", "1 Hour", "2 Hours")))


data_melted <- melt(filtered_data[, cols_to_keep], id.vars = "Missing_Count")

data_melted

# Plot with multiple bar plots for each Missing_Count, excluding AVG_AA_RMSE
ggplot(data_melted, aes(x = factor(Missing_Count), y = value, fill = variable)) +
  geom_col(position = "dodge") +
  labs(x = "Missing Time Frame",
       y = "Avg RMSE",
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
  theme(
    legend.direction = "horizontal",
    legend.position = c(0, 1),  # Place legend inside the plot area, coordinates are normalized (0,0) is bottom-left, (1,1) is top-right
    legend.justification = c(0, 1),  # Anchor the legend at the top-left of its bounding box
    legend.key = element_blank(),  # Optional: remove keys background if preferred
    plot.margin = margin(10, 10, 10, 10),
    panel.grid.major = element_blank()  # Remove major grid lines
    #panel.grid.minor = element_blank() 
    )+
  geom_hline(yintercept = 2.63, color = "#E57200", linetype = "dashed", size = 1)+
  annotate("text", x = 5, y = 35, label = " Avg RMSE LI 3 MR", vjust = -0.5, hjust = 1.1, color="#E57200") 

