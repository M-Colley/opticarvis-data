library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))

library(data.table)

library(devtools)
source_url("https://raw.githubusercontent.com/M-Colley/rCode/main/r_functionality.R")

library("grid")
library("gridExtra")
library(dplyr)
library(knitr)

# Mapping Strategies - Groups
# C2 ↔ A ↔ cold-start BO
# C3 ↔ E ↔ warm-start BO with expert designs
# C4 ↔ B ↔ warm-start BO with user designs
# C5 ↔ C ↔ expert Designs
# C6 ↔ F ↔ user Designs
# S6 (Baseline) ↔ D ↔ no visualizations

# Labels have minor differences
# ggstatsplot
xlabels <- c("GroupA" = "C4-\nCold-Start\nHITL MOBO", "GroupB" = "C6-\nUser-Informed\nWarm-Start\nHITL MOBO", "GroupC" = "C2-\nCustom design\nby experts", "GroupD" = "C1-\nNo Vis.", "GroupE" = "C5-\nExpert-Informed\nWarm-Start\nHITL MOBO", "GroupF" = "C3-\nCustom design\nby end-users")

# plots
labels_x_lab <- c(
  "GroupA" = "C4-\nCold-Start\nHITL MOBO",
  "GroupB" = "C6-\nUser-Informed\nWarm-Start\nHITL MOBO",
  "GroupC" = "C2-\nCustom design\nby experts",
  "GroupD" = "C1-\nNo Vis.",
  "GroupE" = "C5-\nExpert-Informed\nWarm-Start\nHITL MOBO",
  "GroupF" = "C3-\nCustom design\nby end-users"
)

# legend
legendLabels <- c(
  "GroupA" = "C4-Cold-start HITL MOBO",
  "GroupB" = "C6-User-Informed Warm-Start HITL MOBO",
  "GroupC" = "C2-Custom design by experts",
  "GroupD" = "C1-No Vis.",
  "GroupE" = "C5-Expert-Informed Warm-Start HITL MOBO",
  "GroupF" = "C3-Custom design by end-users"
)
# plots
labels_x_lab_Group <- c(
  "GroupA" = "C4-Cold-Start\nHITL MOBO",
  "GroupB" = "C6-User-Informed\nWarm-Start\nHITL MOBO",
  "GroupC" = "C2-Custom design\nby experts",
  "GroupD" = "C1-No Vis.",
  "GroupE" = "C5-Expert-Informed\nWarm-Start\nHITL MOBO",
  "GroupF" = "C3-Custom design\nby end-users"
)



# plots
labels_x_lab <- c(
  "CarStatus" = "Vehicle\nStatus\nHUD\n(Bool)",
  "CarStatusAlpha" = "Vehicle\nStatus\nHUD\nAlpha",
  "CoveredArea" = "CAD-\nCovered\nArea\n(Bool)",
  "CoveredAreaAlpha" = "CAD-\nCovered\nArea\nAlpha",
  "CoveredAreaSize" = "Covered\nArea\nSize",
  "EgoTrajectory" = "Ego\nTrajectory\n(Bool)",
  "EgoTrajectoryAlpha" = "Ego\nTrajectory\nAlpha",
  "EgoTrajectorySize" = "Ego\nTrajectory\nSize",
  "OccludedCars" = "Occluded\nCars\n(Bool)",
  "PedestrianIntention" = "Pedestrian\nIntention\n(Bool)",
  "PedestrianIntentionSize" = "Pedestrian\nIntention\nSize",
  "SemanticSegmentation" = "Sem.\nSegmentation\n(Bool)",
  "SemanticSegmentationAlpha" = "Sem.\nSegmentation\nAlpha",
  "Trajectory" = "Trajectory\n(Bool)",
  "TrajectoryAlpha" = "Trajectory\nAlpha",
  "TrajectorySize" = "Trajectory\nSize"
)

labels_legend <- c(
  "CarStatus" = "Vehicle Status\nHUD (Bool)",
  "CarStatusAlpha" = "Vehicle Status HUD Alpha",
  "CoveredArea" = "CAD-Covered Area (Bool)",
  "CoveredAreaAlpha" = "CAD-Covered Area Alpha",
  "CoveredAreaSize" = "Covered Area Size",
  "EgoTrajectory" = "Ego Trajectory (Bool)",
  "EgoTrajectoryAlpha" = "Ego Trajectory Alpha",
  "EgoTrajectorySize" = "Ego Trajectory Size",
  "OccludedCars" = "Occluded Cars (Bool)",
  "PedestrianIntention" = "Pedestrian Intention (Bool)",
  "PedestrianIntentionSize" = "Pedestrian Intention Size",
  "SemanticSegmentation" = "Sem. Segmentation (Bool)",
  "SemanticSegmentationAlpha" = "Sem. Segmentation Alpha",
  "Trajectory" = "Trajectory (Bool)",
  "TrajectoryAlpha" = "Trajectory Alpha",
  "TrajectorySize" = "Trajectory Size"
)




# Directory path
dir_path <- "./data"


filesObservation <- list.files(
  path = dir_path,
  recursive = TRUE,
  pattern = ".*_ObservationsPerEvaluation.csv$",
  full.names = TRUE
)


# Initialize an empty data.table
main_logs_observations <- data.table()

# Specify the row interval
n <- 1 # Change this to read every nth row

# Loop through each file
for (i in seq_along(filesObservation)) {
  file_name <- filesObservation[i]

  tryCatch(
    {
      cols_to_skip <- c("")

      # Check if the columns to skip actually exist in the file
      # existing_cols <- names(fread(file_name, nrows = 0))
      # cols_to_read <- setdiff(existing_cols, cols_to_skip)

      # Read every nth row
      df <- fread(file_name, sep = ";")

      # print(df)
      # Read every nth row if the data frame is not empty
      if (nrow(df) > 0) {
        df_nth_rows <- df[seq(from = n, to = nrow(df), by = n), ]
        main_logs_observations <- rbindlist(list(main_logs_observations, df_nth_rows), fill = TRUE)
      }
    },
    error = function(e) {
      message(paste("An error occurred while reading file number", i, ":", file_name))
      message("Error message:", e$message)

      # Debug: Print the column names of the problematic file
      existing_cols <- names(fread(file_name, nrows = 0))
      message("Existing columns in the file:", paste(existing_cols, collapse = ", "))
    }
  )
}

df <- NULL
df_nth_rows <- NULL

main_logs_observations$UserID <- as.factor(main_logs_observations$UserID)
main_logs_observations$GroupID <- factor(main_logs_observations$GroupID, levels = c("GroupD", "GroupC", "GroupF", "GroupA", "GroupE", "GroupB"))
main_logs_observations$LongitudinalID <- as.factor(main_logs_observations$LongitudinalID)
main_logs_observations$IsPareto <- as.factor(main_logs_observations$IsPareto)
main_logs_observations$Run <- as.factor(main_logs_observations$Run)




main_logs_observations <- subset(main_logs_observations, UserID %!in% c("DANIEL"))

# Delete rows for GroupE and GroupB where Run == 0
# Here: the QEHVI was not normalized
main_logs_observations <- main_logs_observations %>%
  filter(!(GroupID %in% c("GroupE", "GroupB") & Run == 0))


objectives <- c(
  "Trust QEHVI", "Understanding QEHVI", "MentalLoad QEHVI",
  "PerceivedSafety QEHVI", "Aesthetics QEHVI", "Acceptance QEHVI"
)


main_logs_observations <- main_logs_observations |> 
  group_by(UserID, GroupID, LongitudinalID) |> 
  mutate(PARETO_EMOA = add_pareto_emoa_column(pick(everything()), objectives = objectives)$PARETO_EMOA) |> 
  ungroup()

main_logs_observations$PARETO_EMOA <- as.logical(main_logs_observations$PARETO_EMOA)




# remove the QEHVI values
test_df <- main_logs_observations[, 7:28]

long_df_observations <- test_df %>%
  pivot_longer(cols = 1:16, names_to = "variable", values_to = "value")


long_df_observations$variable <- factor(long_df_observations$variable, levels = c("SemanticSegmentation", "SemanticSegmentationAlpha", "PedestrianIntention", "PedestrianIntentionSize", "Trajectory", "TrajectoryAlpha", "TrajectorySize", "EgoTrajectory", "EgoTrajectoryAlpha", "EgoTrajectorySize", "CoveredArea", "CoveredAreaAlpha", "CoveredAreaSize", "OccludedCars", "CarStatus", "CarStatusAlpha"))

















#########################################################################################


parameterfiles <- list.files(
  path = dir_path,
  recursive = TRUE,
  pattern = ".*_ParameterValues.csv$",
  full.names = TRUE
)


# Initialize an empty data.table
main_logs_parameters_param <- data.table()

# Specify the row interval
n <- 1 # Change this to read every nth row

# Loop through each file
for (i in seq_along(parameterfiles)) {
  file_name <- parameterfiles[i]

  tryCatch(
    {
      cols_to_skip <- c("")

      # Check if the columns to skip actually exist in the file
      # existing_cols <- names(fread(file_name, nrows = 0))
      # cols_to_read <- setdiff(existing_cols, cols_to_skip)

      # Read every nth row
      df <- fread(file_name, sep = ";")

      # print(df)
      # Read every nth row if the data frame is not empty
      if (nrow(df) > 0) {
        df_nth_rows <- df[seq(from = n, to = nrow(df), by = n), ]
        main_logs_parameters_param <- rbindlist(list(main_logs_parameters_param, df_nth_rows), fill = TRUE)
      }
    },
    error = function(e) {
      message(paste("An error occurred while reading file number", i, ":", file_name))
      message("Error message:", e$message)

      # Debug: Print the column names of the problematic file
      existing_cols <- names(fread(file_name, nrows = 0))
      message("Existing columns in the file:", paste(existing_cols, collapse = ", "))
    }
  )
}

df <- NULL
df_nth_rows <- NULL

main_logs_parameters_param$UserID <- as.factor(main_logs_parameters_param$UserID)
main_logs_parameters_param$GroupID <- factor(main_logs_parameters_param$GroupID, levels = c("GroupD", "GroupC", "GroupF", "GroupA", "GroupE", "GroupB"))
main_logs_parameters_param$LongitudinalID <- as.factor(main_logs_parameters_param$LongitudinalID)
main_logs_parameters_param$Run <- as.factor(main_logs_parameters_param$Run)




main_logs_parameters_param <- subset(main_logs_parameters_param, UserID %!in% c("DANIEL"))

# remove C1-No Vis as there was obviously no visualization
main_logs_parameters_param <- subset(main_logs_parameters_param, GroupID %!in% c("GroupD"))






long_df_parameters <- main_logs_parameters_param %>%
  pivot_longer(cols = 5:20, names_to = "variable", values_to = "value")



long_df_parameters$variable <- factor(long_df_parameters$variable, levels = c("SemanticSegmentation", "SemanticSegmentationAlpha", "PedestrianIntention", "PedestrianIntentionSize", "Trajectory", "TrajectoryAlpha", "TrajectorySize", "EgoTrajectory", "EgoTrajectoryAlpha", "EgoTrajectorySize", "CoveredArea", "CoveredAreaAlpha", "CoveredAreaSize", "OccludedCars", "CarStatus", "CarStatusAlpha"))
levels(long_df_parameters$variable)







# the three warm start methods
long_df_parameters <- subset(long_df_parameters, GroupID %!in% c("GroupA", "GroupE", "GroupB"))
long_df_parameters$Run <- as.numeric(as.character(long_df_parameters$Run))


# we keep run 1 which should be from day one!
long_df_parameters <- long_df_parameters %>%
  arrange(UserID, variable, GroupID, Run) %>%
  group_by(UserID, variable, GroupID) %>%
  mutate(is_first_run = row_number() == 1) %>%  # Mark the first run as TRUE
  ungroup() %>%
  filter(is_first_run)  # Keep only the first run for each combination

highest_run_df <- long_df_parameters %>%
  arrange(UserID, variable, GroupID, desc(Run)) %>%
  group_by(UserID, variable, GroupID) %>%
  slice_head(n = 1) %>%
  ungroup()




long_df_parameters <- long_df_parameters %>%
  group_by(UserID, variable, GroupID) %>%
  mutate(PARETO_EMOA = TRUE) %>%  # Add PARETO_EMOA column with TRUE value
  ungroup() 


# remove wrong or unnecessary columns
long_df_parameters <- long_df_parameters %>%
  select(-is_first_run)

long_df_observations <- long_df_observations %>%
  select(-IsPareto)


# merge the long_df_parameters with the long_df_observations
long_df <- rbind(long_df_parameters, long_df_observations)


long_df_pareto_true <- subset(long_df, PARETO_EMOA == TRUE)


long_df_pareto_true_means <- long_df_pareto_true %>%
  group_by(GroupID, variable) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

# Print the result
print(long_df_pareto_true_means, n = 200)




p1 <- long_df_pareto_true %>% ggplot() +
  aes(x = variable, y = value, fill = GroupID, colour = GroupID, group = GroupID) +
  ylab("Value") +
  # scale_color_see(labels = legendLabels) +
  scale_fill_see(labels = legendLabels) +
  theme(legend.position.inside = c(0.8, 0.85)) +
  xlab("") +
  ylab("") +
  geom_smooth() +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
  scale_x_discrete(labels = labels_x_lab) +
  scale_color_manual(labels = legendLabels, values = c("#4d9068", "#d52e2e", "#2d78b0", "#fb8125", "#fcd676"))
p1 + facet_grid(~GroupID, )
#ggsave("plots/parameters_per_run_last_day_1_lm_facet.pdf", width = 20, height = 12, device = cairo_pdf)


# Assuming test_params is your data frame
# Group by the variable and calculate the statistics
grouped_data <- long_df_pareto_true %>% group_by(variable)
stats <- grouped_data %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  ) %>%
  mutate(ymin = mean - 1 * sd, ymax = mean + 1 * sd)

print(stats)

# Create an offset for xmin and xmax
offset <- 0.2 # Adjust this value as needed for visibility
stats$xmin <- as.numeric(factor(stats$variable)) - offset
stats$xmax <- as.numeric(factor(stats$variable)) + offset

levels(long_df_pareto_true$variable)



long_df_pareto_true %>% ggplot() +
  aes(x = variable, y = value, fill = GroupID, colour = GroupID, group = GroupID) +
  geom_rect(
    data = stats, inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "grey", alpha = 0.2
  ) +
  # scale_color_see(labels = legendLabels) +
  scale_fill_see(labels = legendLabels) +
  theme(legend.position.inside = c(0.70, 0.09)) +
  xlab("") +
  ylab("Range of final and normalized value\nper visualization parameter per Condition") +
  ylim(0, 1) +
  #stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9, position = position_dodge(width = 0.1)) +
  #stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.1) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 1, position = position_dodge(width = 0.1), alpha = 0.5) +
  scale_x_discrete(labels = labels_x_lab) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "blue") +
  scale_color_manual(labels = legendLabels, values = c("#4d9068", "#d52e2e", "#2d78b0", "#fb8125", "#fcd676"))
ggsave("plots/parameters_pareto_true_day_1_grey_rect_1_sd.pdf", width = 20, height = 12, device = cairo_pdf)



user_count <- long_df_pareto_true %>%
  dplyr::group_by(GroupID) %>%
  dplyr::summarize(num_users = n_distinct(UserID))
user_count

result <- long_df_pareto_true %>%
  group_by(variable, GroupID) %>%
  dplyr::summarize(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  ) %>%
  mutate(Legendlabel = legendLabels[as.character(GroupID)])




# Convert your summarized result to Markdown format using kable
md_table <- kable(result, format = "markdown")

# Write the Markdown table to a file
writeLines(md_table, "summary_results_mean_sd.md")






result <- long_df_pareto_true %>%
  group_by(variable, GroupID) %>%
  dplyr::reframe(
    Q1 = quantile(value, 0.25, na.rm = TRUE),
    Q3 = quantile(value, 0.75, na.rm = TRUE),
    userid = UserID 
  )


result2 <- long_df_pareto_true %>%
  dplyr::group_by(variable, GroupID) %>%
  dplyr::reframe(
    Q1 = quantile(value, 0.25, na.rm = TRUE),
    Q3 = quantile(value, 0.75, na.rm = TRUE)
  ) %>% 
  dplyr::left_join(long_df_pareto_true, by = c("variable", "GroupID")) %>%
  dplyr::group_by(variable, GroupID) %>%
  dplyr::mutate(
    dist_to_Q1 = abs(value - Q1),
    dist_to_Q3 = abs(value - Q3)
  ) %>%
  dplyr::reframe(
    Q1 = first(Q1),  # Retain Q1
    Q3 = first(Q3),  # Retain Q3
    closest_to_Q1_value = value[which.min(dist_to_Q1)],  # Value closest to Q1
    closest_to_Q1_userid = UserID[which.min(dist_to_Q1)],  # User ID closest to Q1
    closest_to_Q3_value = value[which.min(dist_to_Q3)],  # Value closest to Q3
    closest_to_Q3_userid = UserID[which.min(dist_to_Q3)]   # User ID closest to Q3
  )



# Convert your summarized result to Markdown format using kable
md_table <- kable(result2, format = "markdown")

# Write the Markdown table to a file
writeLines(md_table, "summary_results_iqr_opticarvis.md")






long_df_pareto_true %>% ggplot() +
  aes(x = variable, y = as.numeric(value), fill = GroupID, colour = GroupID, group = GroupID) +
  geom_rect(
    data = stats, inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "grey", alpha = 0.5
  ) +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.3) +
  # scale_color_see(labels = legendLabels) +
  scale_fill_see(labels = legendLabels) +
  theme(legend.position.inside = c(0.70, 0.08)) +
  xlab("Design Parameters") +
  ylab("Range of final and normalized value\nper visualization parameter per Condition") +
  ylim(0.1, 1) +
  #stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9, position = position_dodge(width = 0.4)) +
  #stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.2) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 1, position = position_dodge(width = 0.4), alpha = 1) +
  scale_x_discrete(labels = labels_x_lab) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "blue") +
  scale_y_continuous(breaks = seq(0, 1, .1)) +
  scale_color_manual(labels = legendLabels, values = c("#4d9068", "#d52e2e", "#2d78b0", "#fb8125", "#fcd676"))
ggsave("plots/parameters_last_pareto_true_day_1_grey_rect_range_adj_sd.pdf", width = 20, height = 12, device = cairo_pdf)






long_df_pareto_true %>% ggplot() +
  aes(x = variable, y = value, fill = GroupID, colour = GroupID, group = interaction(GroupID, Run)) +
  # scale_color_see(labels = legendLabels) +
  scale_fill_see(labels = legendLabels) +
  theme(legend.position.inside = c(0.8, 0.85)) +
  xlab("") +
  ylab("Value") +
  geom_point(alpha = 0.01) +
  geom_line(alpha = 0.01) +
  #stat_summary(fun = mean, geom = "point", size = 1.5, alpha = 0.05, position = position_dodge(width = 0.1)) +
  #stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.05) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
  scale_x_discrete(labels = labels_x_lab) +
  scale_color_manual(labels = legendLabels, values = c("#4d9068", "#d52e2e", "#2d78b0", "#fb8125", "#fcd676"))
ggsave("plots/parameters_all_runs_pareto_true_day_1.pdf", width = 20, height = 12, device = cairo_pdf)



# test_params %>% ggplot() +
#
#   aes(x = variable, y = value, fill = UserID, colour = UserID, group = UserID) +
#   #scale_color_see(labels = legendLabels) +
#   scale_fill_see(labels = legendLabels) +
#
#   theme(legend.position.inside = c(0.8, 0.85)) +
#   xlab("") +
#   ylab("Value") +
#   stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
#   stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.3) +
#   stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
#   scale_x_discrete(labels = labels_x_lab)+ scale_color_manual(labels = legendLabels, values = c("#4d9068", "#d52e2e", "#2d78b0", "#fb8125", "#fcd676"))
# ggsave("plots/parameters_per_user_last_day_1.pdf", width = 20, height = 12, device = cairo_pdf)



# this only takes last plot
# ggsave("plots/parameters_per_run_last_day_1_combined.pdf", width = pdfwidth, height = pdfheight + 7, device = cairo_pdf)

long_df_pareto_true$Run <- as.numeric(as.character(long_df_pareto_true$Run))
long_df_pareto_true$Run <- ifelse(long_df_pareto_true$GroupID == "GroupA", long_df_pareto_true$Run, long_df_pareto_true$Run + 4)
long_df_pareto_true$Run <- as.factor(long_df_pareto_true$Run)


test_params_day_1 <- subset(long_df_pareto_true, Run %!in% c("16", "17"))


p <- test_params_day_1 %>% ggplot() +
  aes(x = variable, y = value, fill = GroupID, colour = GroupID, group = GroupID) +
  # scale_color_see(labels = legendLabels) +
  scale_fill_see(labels = legendLabels) +
  ylab("Value") +
  theme(legend.position.inside = c(0.2, 0.2)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.8, position = position_dodge(width = 0.1)) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.45) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.45) +
  scale_x_discrete(labels = labels_x_lab) +
  scale_color_manual(labels = legendLabels, values = c("#4d9068", "#d52e2e", "#2d78b0", "#fb8125", "#fcd676"))
p + facet_wrap(~Run, ncol = 3, )
#ggsave("plots/parameters_per_run_all_runs.pdf", width = pdfwidth, height = pdfheight + 10, device = cairo_pdf)


test_params_day_long <- subset(long_df_parameters, Run %in% c("15", "16", "17"))


dayLabels <- c(
  "15" = "Day 1",
  "16" = "Day 2", "17" = "Day 3"
)

levels(test_params_day_long$GroupID)



# Group by the variable and calculate the statistics
stats_longitudinal <- test_params_day_long %>%
  group_by(variable, Run) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE), # Calculate minimum value
    max = max(value, na.rm = TRUE) # Calculate maximum value
  ) %>%
  mutate(
    ymin = mean - sd,
    ymax = mean + sd
  )


# Create an offset for xmin and xmax
offset <- 0.2 # Adjust this value as needed for visibility
stats_longitudinal$xmin <- as.numeric(factor(stats_longitudinal$variable)) - offset
stats_longitudinal$xmax <- as.numeric(factor(stats_longitudinal$variable)) + offset



test_params_day_long$GroupID <- factor(test_params_day_long$GroupID, levels = c("GroupD", "GroupC", "GroupF", "GroupA", "GroupE", "GroupB"))

levels(test_params_day_long$GroupID)

test_params_day_long %>% ggplot() +
  aes(x = variable, y = as.numeric(value), fill = GroupID, colour = GroupID, group = GroupID) +
  geom_rect(
    data = stats_longitudinal, inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    alpha = 0.2, fill = "grey"
  ) +
  # scale_color_see(labels = legendLabels) +
  scale_fill_see(labels = legendLabels) +
  theme(legend.position.inside = c(0.81, 0.72), legend.text = element_text(size = 15)) +
  xlab("") +
  ylab("Range of final and normalized value per visualization parameter per Condition") +
  ylim(0.1, 1) +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9, position = position_dodge(width = 0.1)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.2) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
  scale_x_discrete(labels = labels_x_lab) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "blue", alpha = 0.5) +
  scale_y_continuous(breaks = seq(0, 1, .2)) +
  scale_color_manual(labels = legendLabels, values = c("#4d9068", "#d52e2e", "GroupA" = "#2d78b0", "GroupE" = "#fb8125", "GroupB" = "#fcd676")) +
  facet_wrap(~Run, ncol = 1, labeller = labeller(Run = dayLabels))
#ggsave("plots/parameters_per_run_longitudinal.pdf", width = 20, height = 14, device = cairo_pdf)





