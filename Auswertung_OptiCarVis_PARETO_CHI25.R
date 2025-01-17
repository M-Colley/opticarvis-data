library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))

# library(colorspace)
# q20 <- qualitative_hcl(20, "Warm")

library(data.table)
library(dplyr)
library(performance)
library(ggpmisc)
library(gganimate)
library(ggcorrplot)


create_correlation_heatmap_with_significance <- function(data, objectives, labels_map, title = "Correlation Heatmap with Significance") {
  # Select the relevant objective columns
  objective_data <- data %>%
    select(all_of(objectives))  # Ensure only the selected objectives are included
  # Compute the correlation matrix
  correlation_matrix <- cor(objective_data, use = "complete.obs")
  # Calculate the p-values for significance of correlations
  cor_test <- function(x, y) {
    cor_test_result <- cor.test(x, y)
    return(c(cor_test_result$estimate, cor_test_result$p.value))
  }
  # Apply correlation tests to all combinations of the objectives
  p_values <- matrix(NA, ncol = length(objectives), nrow = length(objectives))
  for (i in 1:length(objectives)) {
    for (j in 1:length(objectives)) {
      if (i != j) {
        test_result <- cor_test(objective_data[[i]], objective_data[[j]])
        p_values[i, j] <- test_result[2]
      } else {
        p_values[i, j] <- NA  # Leave diagonal cells as NA
      }
    }
  }
  
  # Significance stars based on p-value thresholds
  significance_levels <- ifelse(p_values < 0.001, "***",
                                ifelse(p_values < 0.01, "**",
                                       ifelse(p_values < 0.05, "*", "")))
  
  # Map labels to objectives
  colnames(correlation_matrix) <- labels_map[objectives]
  rownames(correlation_matrix) <- labels_map[objectives]
  
  # Generate the correlation heatmap with significance and stars
  heatmap_plot <- ggcorrplot(correlation_matrix,
                             method = "square",  # Create square tiles
                             type = "lower",  # Only show lower triangle
                             lab = TRUE,  # Show correlation coefficients
                             p.mat = p_values,  # Add significance levels
                             sig.level = 0.05,  # Set significance threshold
                             insig = "blank",  # Hide insignificant correlations
                             title = title,
                             ggtheme = theme_minimal(base_size = 15),  # Use default color gradient
                             lab_size = 4) 
  
  # Display the plot
  print(heatmap_plot)
  # Save the plot as a PDF
  ggsave(filename = paste0("plots/", gsub(" ", "_", title), "_heatmap_with_significance.pdf"), plot = heatmap_plot, width = 9, height = 6, device = cairo_pdf)
}





remove_group_outliers <- function(original_data, group, dependent_var) {
  # Subset the data for the current group
  group_data <- subset(original_data, group_id == group)

  # Identify outliers using the performance::check_outliers function
  outliers_list <- performance::check_outliers(group_data[[dependent_var]])

  # Create a dataframe containing only the outliers
  filtered_data <- group_data[as.vector(outliers_list), ]

  # Remove the outliers from the original data set for the current group
  cleaned_data <- anti_join(original_data, filtered_data, by = c("user_id", "group_id", "started"))

  return(cleaned_data)
}


library(devtools)
source_url("https://raw.githubusercontent.com/M-Colley/rCode/main/r_functionality.R")





# Mapping Strategies - Groups
# S1 ↔ A ↔ cold-start BO
# S2 ↔ E ↔ warm-start BO with expert designs
# S3 ↔ B ↔ warm-start BO with user designs
# S4 ↔ C ↔ expert Designs
# S5 ↔ F ↔ user Designs
# S6 (Baseline) ↔ D ↔ no visualizations

# S1 Cold-Start HITL MOBO
# S2 Expert-Informed Warm-Start HITL MOBO
# S3 User-Informed Warm-Start HITL MOBO
# S4 Custom design by experts
# S5 Custom design by end-users

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

# Directory path
dir_path <- "./Logging_data_OptiCarVis"

files <- list.files(
  path = dir_path,
  recursive = TRUE,
  pattern = ".*_QuestionnaireResults.csv$",
  full.names = TRUE
)


# Initialize an empty data.table
main_logs_questionnaire <- data.table()

# Specify the row interval
n <- 1 # Change this to read every nth row

# Loop through each file
for (i in seq_along(files)) {
  file_name <- files[i]

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
        main_logs_questionnaire <- rbindlist(list(main_logs_questionnaire, df_nth_rows), fill = TRUE)
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



main_logs_questionnaire$user_id <- as.factor(main_logs_questionnaire$user_id)
main_logs_questionnaire$group_id <- factor(main_logs_questionnaire$group_id, levels = c("GroupD", "GroupC", "GroupF", "GroupA", "GroupE", "GroupB"))
main_logs_questionnaire$longitudinal_id <- as.factor(main_logs_questionnaire$longitudinal_id)
main_logs_questionnaire$phase <- as.factor(main_logs_questionnaire$phase)
main_logs_questionnaire$run <- as.factor(main_logs_questionnaire$run)
main_logs_questionnaire$started <- as.POSIXct(main_logs_questionnaire$started, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")
main_logs_questionnaire$finished <- as.POSIXct(main_logs_questionnaire$finished, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")



main_logs_questionnaire <- subset(main_logs_questionnaire, user_id %!in% c("DANIEL"))


# Trust
main_logs_questionnaire$trust <- (main_logs_questionnaire$Trust_1 + main_logs_questionnaire$Trust_2) / 2.0

# predicatbility
main_logs_questionnaire$predictability <- (main_logs_questionnaire$Understanding_2 + main_logs_questionnaire$Understanding_3 + main_logs_questionnaire$Understanding_1 + main_logs_questionnaire$Understanding_4) / 4.0

# Perceived safety
main_logs_questionnaire$perceivedSafetyScore <- (main_logs_questionnaire$PerceivedSafety_1 + main_logs_questionnaire$PerceivedSafety_2 + main_logs_questionnaire$PerceivedSafety_3 + main_logs_questionnaire$PerceivedSafety_4) / 4.0



main_logs_questionnaire$acceptance <- (main_logs_questionnaire$Acceptance_1 + main_logs_questionnaire$Acceptance_2) / 2.0


# main_logs_questionnaire$Aesthetics_1
# main_logs_questionnaire$Acceptance_1 --> useful
# main_logs_questionnaire$Acceptance_2 --> satisfying
# main_logs_questionnaire$MentalLoad_1


levels(main_logs_questionnaire$group_id)
levels(main_logs_questionnaire$user_id)


user_count <- main_logs_questionnaire %>%
  dplyr::group_by(group_id) %>%
  dplyr::summarize(num_users = n_distinct(user_id))
user_count

sum(user_count$num_users)


# For the Groups C, D, and F, there were only 2 runs: one 33s and one 3 min. These were called "run == 1", which we filter here.
main_df <- main_logs_questionnaire %>%
  filter(
    (group_id %in% c("GroupA", "GroupB", "GroupE") & phase %in% c("exploration", "exploitation")) |
      (!(group_id %in% c("GroupA", "GroupB", "GroupE")) & run == 1)
  )


#main_df <- subset(main_logs_questionnaire, phase %in% c("exploration", "exploitation"))
# main_logs_questionnaire_day1_final <- subset(main_logs_questionnaire_day1_final, run == 1)




dependent_vars <- c("MentalLoad_1", "trust", "predictability", "perceivedSafetyScore", "Aesthetics_1", "Acceptance_1", "Acceptance_2")

objectives <- c(
  "trust", "predictability", "perceivedSafetyScore",
  "MentalLoad_1", "Aesthetics_1", "acceptance"
)




# Calculate the Pareto front for this participant
main_df <- main_df |> 
  group_by(user_id, group_id, longitudinal_id) |> 
  mutate(PARETO_EMOA = add_pareto_emoa_column(pick(everything()), objectives = objectives)$PARETO_EMOA) |> 
  ungroup()

main_df$PARETO_EMOA <- as.logical(main_df$PARETO_EMOA)

labelsmap <- c(
  "trust" = "Trust", "predictability" = "Predictability", "perceivedSafetyScore" = "Perceived Safety",
  "MentalLoad_1" = "Cognitive Load", "Aesthetics_1" = "Aesthetics", "acceptance" = "Acceptance", "Acceptance_2" = "Satisfying"
)

main_df_inverted_cog <- main_df
main_df_inverted_cog$MentalLoad_1 <- 21- main_df_inverted_cog$MentalLoad_1 

# ATTENTION: cor.vars.names must be in correct order
ggstatsplot::ggcorrmat(main_df |> select(all_of(objectives)), cor.vars.names = c("Trust", "Predictability","Perceived Safety", "Cognitive Load", "Aesthetics", "Acceptance"), matrix.type = "lower", )
ggsave("plots/correlations.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)

create_correlation_heatmap_with_significance(main_df, objectives, labels_map = labelsmap, title = "Correlation Heatmap with Significance")

# 
# main_df <- subset(main_df, !is.na(Aesthetics_1))
# main_df <- subset(main_df, !is.na(Acceptance_1))
# main_df <- subset(main_df, !is.na(Acceptance_2))







# Get unique levels of 'group_id'
unique_groups <- unique(main_df$group_id)
unique_groups

ggbetweenstatsWithPriorNormalityCheck <- function(data, x, y, ylab, xlabels, showPairwiseComp = TRUE, plotType = "boxviolin", palette = "glasbey") {
  assertthat::not_empty(data)
  assertthat::not_empty(x)
  assertthat::not_empty(y)
  assertthat::not_empty(ylab)
  assertthat::not_empty(xlabels)

  normality_test <- list() # Initialize empty list to store test results
  normallyDistributed <- TRUE
  group_all_data_equal <- FALSE

  # Iterate over each group in data[[x]]
  for (group in unique(data[[x]])) {
    subset_data <- data[data[[x]] == group, y, drop = TRUE]

    # Check if subset_data is a data frame or list, and convert to numeric vector if needed
    if (is.data.frame(subset_data) || is.list(subset_data)) {
      subset_data <- as.numeric(subset_data[[1]])
    }

    # Remove NA values if any conversion failed
    subset_data <- subset_data[!is.na(subset_data)]


    # Check if all values in the subset are equal
    if (length(unique(subset_data)) > 1) {
      normality_test[[group]] <- shapiro.test(subset_data)
    } else {
      normality_test[[group]] <- NULL
      group_all_data_equal <- TRUE
    }
  }

  # Check the p-value for each test result
  for (i in normality_test) {
    if (!is.null(i)) {
      if (i$p.value < 0.05) {
        normallyDistributed <- FALSE
        break
      }
    }
  }


  type <- ifelse(normallyDistributed, "p", "np")

  # if one group_all_data_equal then we use the var.equal = TRUE, see here: https://github.com/IndrajeetPatil/ggstatsplot/issues/880
  ggstatsplot::ggbetweenstats(
    data = data, x = !!x, y = !!y, type = type, centrality.type = "p", ylab = ylab, xlab = "", pairwise.comparisons = showPairwiseComp, var.equal = group_all_data_equal,
    centrality.point.args = list(size = 5, alpha = 0.5, color = "darkblue"), point.args = list(alpha = 0.9, position = ggplot2::position_jitterdodge(dodge.width = 0.6), size = 3, stroke = 0, na.rm = TRUE), plot.type = plotType,
    p.adjust.method = "holm", ggplot.component = list(theme(text = element_text(size = 16), plot.subtitle = element_text(size = 17, face = "bold"))), ggsignif.args = list(textsize = 4, tip_length = 0.01)
  ) + scale_x_discrete(labels = xlabels)
}



# p1 <- ggbetweenstatsWithPriorNormalityCheck(data = main_df_pareto_true, x = "group_id", y = "MentalLoad_1", ylab = "Cognitive Load", xlabels = xlabels)
# p1

levels(main_df$user_id)

user_entry_count <- main_df %>%
  dplyr::group_by(user_id) %>%
  dplyr::summarize(num_entries = n()) %>%
  dplyr::arrange(num_entries) # Orders by ascending number of entries

user_entry_count_before <- main_logs_questionnaire %>%
  dplyr::group_by(user_id) %>%
  dplyr::summarize(num_entries = n()) %>%
  dplyr::arrange(num_entries) # Orders by ascending number of entries


test_df <- main_logs_questionnaire %>%
  filter(!(group_id %in% c("GroupA", "GroupB", "GroupE")) & run != 1)

test_df_2 <- main_logs_questionnaire %>%
  filter(!(group_id %in% c("GroupA", "GroupB", "GroupE")) & run == 1)


unique_users_in_test_df <- unique(test_df$user_id)
unique_users_in_test_df_2 <- unique(test_df_2$user_id)

users_only_in_test_df <- setdiff(unique_users_in_test_df, unique_users_in_test_df_2)
users_only_in_test_df


main_df_pareto_true <- subset(main_df, PARETO_EMOA == TRUE)

# main_df_pareto_true_highest_cog <- main_df_pareto_true %>%
#   group_by(user_id, group_id, longitudinal_id) %>%
#   slice_max(order_by = MentalLoad_1, n = 1, with_ties = FALSE) %>% ungroup()


p <- ggbetweenstatsWithPriorNormalityCheck(data = main_df_pareto_true, x = "group_id", y = "MentalLoad_1", ylab = "Cognitive Load", xlabels = xlabels) + scale_color_manual(values = c("grey", "#4d9068", "#d52e2e", "#2d78b0", "#fb8125", "#fcd676"))
p
ggsave("plots/ggstats_mental_pareto_true.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)
# BayesFactor::anovaBF(MentalLoad_1 ~ group_id, data = main_df_pareto_true, whichRandom="user_id")  |> bayesfactor_models() |> report()
# strong

reportggstatsplot(p, iv = "optimization", dv = "MentalLoad_1")
reportggstatsplotPostHoc(main_df_pareto_true, p, iv = "group_id", dv = "MentalLoad_1", label_mappings = legendLabels)

# df_for_analysis <- main_df_pareto_true
# # Loop through each unique 'group_id'
# for(group in unique_groups) {
#
#   df_for_analysis <- remove_group_outliers(df_for_analysis, group = group, dependent_var = "trust")
# }

# df_for_analysis$group_id <- as.factor(df_for_analysis$group_id)
# ggbetweenstats(data = df_for_analysis, x = group_id, y = "trust", ylab = "Trust", xlabels = xlabels, var.equal = TRUE)
p <- ggbetweenstatsWithPriorNormalityCheck(data = main_df_pareto_true, x = "group_id", y = "trust", ylab = "Trust", xlabels = xlabels) + scale_color_manual(values = c("grey", "#4d9068", "#d52e2e", "#2d78b0", "#fb8125", "#fcd676"))
p
ggsave("plots/ggstats_trust_pareto_true.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)

main_df_pareto_true$user_id

user_count <- main_df_pareto_true %>%
  dplyr::group_by(group_id) %>%
  dplyr::summarize(num_users = n_distinct(user_id))
user_count

sum(user_count$num_users)



reportggstatsplot(p, iv = "optimization", dv = "trust")
reportggstatsplotPostHoc(main_df_pareto_true, p, iv = "group_id", dv = "trust", label_mappings = legendLabels)


# df_for_analysis <- main_df_pareto_true
# # Loop through each unique 'group_id'
# for(group in unique_groups) {
#
#   df_for_analysis <- remove_group_outliers(df_for_analysis, group = group, dependent_var = "predictability")
# }
p <- ggbetweenstatsWithPriorNormalityCheck(data = main_df_pareto_true, x = "group_id", y = "predictability", ylab = "Predictability", xlabels = xlabels) + scale_color_manual(values = c("grey", "#4d9068", "#d52e2e", "#2d78b0", "#fb8125", "#fcd676"))
p
ggsave("plots/ggstats_predictability_pareto_true.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)

reportggstatsplot(p, iv = "optimization", dv = "predictability")
reportggstatsplotPostHoc(main_df_pareto_true, p, iv = "group_id", dv = "predictability", label_mappings = legendLabels)




# df_for_analysis <- main_df_pareto_true
# # Loop through each unique 'group_id'
# for(group in unique_groups) {
#
#   df_for_analysis <- remove_group_outliers(df_for_analysis, group = group, dependent_var = "perceivedSafetyScore")
# }
p <- ggbetweenstatsWithPriorNormalityCheck(data = main_df_pareto_true, x = "group_id", y = "perceivedSafetyScore", ylab = "Perceived Safety", xlabels = xlabels) + scale_color_manual(values = c("grey", "#4d9068", "#d52e2e", "#2d78b0", "#fb8125", "#fcd676"))
p
ggsave("plots/ggstats_perceivedSafety_pareto_true.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)
reportggstatsplot(p, iv = "optimization", dv = "perceivedSafetyScore")
reportggstatsplotPostHoc(main_df_pareto_true, p, iv = "group_id", dv = "perceivedSafetyScore", label_mappings = legendLabels)


# df_for_analysis <- main_df_pareto_true_no_Aes_Acc
# # Loop through each unique 'group_id'
# for(group in unique_groups) {
#
#   df_for_analysis <- remove_group_outliers(df_for_analysis, group = group, dependent_var = "Aesthetics_1")
# }


ggbetweenstatsWithPriorNormalityCheck(data = main_df_pareto_true, x = "group_id", y = "Aesthetics_1", ylab = "Aesthetics", xlabels = xlabels)

p <- ggbetweenstatsWithPriorNormalityCheck(data = main_df_pareto_true, x = "group_id", y = "Aesthetics_1", ylab = "Aesthetics", xlabels = xlabels) + scale_color_manual(values = c("#4d9068", "#d52e2e", "#2d78b0", "#fb8125", "#fcd676"))
p
ggsave("plots/ggstats_Aesthetics_pareto_true.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)

reportggstatsplot(p, iv = "optimization", dv = "Aesthetics_1")
reportggstatsplotPostHoc(main_df_pareto_true, p, iv = "group_id", dv = "Aesthetics_1", label_mappings = legendLabels)


# df_for_analysis <- main_df_pareto_true_no_Aes_Acc
# # Loop through each unique 'group_id'
# for(group in unique_groups) {
#
#   df_for_analysis <- remove_group_outliers(df_for_analysis, group = group, dependent_var = "Acceptance_1")
# }
# p <- ggbetweenstatsWithPriorNormalityCheck(data = main_df_pareto_true, x = "group_id", y = "Acceptance_1", ylab = "Usefulness", xlabels = xlabels) + scale_color_manual(values = c("#4d9068", "#d52e2e", "#2d78b0", "#fb8125", "#fcd676"))
# p
# ggsave("plots/ggstats_Acceptance_1_pareto_true.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)
# 
# reportggstatsplot(p, iv = "optimization", dv = "Acceptance_1")
# reportggstatsplotPostHoc(main_df_pareto_true, p, iv = "group_id", dv = "Acceptance_1", label_mappings = legendLabels)
# 
# 
# # df_for_analysis <- main_df_pareto_true_no_Aes_Acc
# # # Loop through each unique 'group_id'
# # for(group in unique_groups) {
# #
# #   df_for_analysis <- remove_group_outliers(df_for_analysis, group = group, dependent_var = "Acceptance_2")
# # }
# 
# p <- ggbetweenstatsWithPriorNormalityCheck(data = main_df_pareto_true, x = "group_id", y = "Acceptance_2", ylab = "Satisfying", xlabels = xlabels) + scale_color_manual(values = c("#4d9068", "#d52e2e", "#2d78b0", "#fb8125", "#fcd676"))
# p
# ggsave("plots/ggstats_Acceptance_2_pareto_true.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)
# 
# reportggstatsplot(p, iv = "optimization", dv = "Acceptance_2")
# reportggstatsplotPostHoc(main_df_pareto_true, p, iv = "group_id", dv = "Acceptance_2", label_mappings = legendLabels)




p <- ggbetweenstatsWithPriorNormalityCheck(data = main_df_pareto_true, x = "group_id", y = "acceptance", ylab = "Acceptance", xlabels = xlabels) + scale_color_manual(values = c("#4d9068", "#d52e2e", "#2d78b0", "#fb8125", "#fcd676"))
p
ggsave("plots/ggstats_Acceptance_pareto_true.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)

reportggstatsplot(p, iv = "optimization", dv = "acceptance")
reportggstatsplotPostHoc(main_df_pareto_true, p, iv = "group_id", dv = "acceptance", label_mappings = legendLabels)




# BayesFactor::anovaBF(MentalLoad_1 ~ group_id, data = main_df_pareto_true, whichRandom="user_id")  |> bayesfactor_models() |> report()
# BayesFactor::anovaBF(trust ~ group_id, data = main_df_pareto_true, whichRandom="user_id")  |> bayesfactor_models() |> report()
# BayesFactor::anovaBF(predictability ~ group_id, data = main_df_pareto_true, whichRandom="user_id")  |> bayesfactor_models() |> report()
# BayesFactor::anovaBF(perceivedSafetyScore ~ group_id, data = main_df_pareto_true, whichRandom="user_id")  |> bayesfactor_models() |> report()
#
# BayesFactor::anovaBF(Aesthetics_1 ~ group_id, data = main_df_pareto_true, whichRandom="user_id")  |> bayesfactor_models() |> report()
# BayesFactor::anovaBF(Acceptance_1 ~ group_id, data = main_df_pareto_true, whichRandom="user_id")  |> bayesfactor_models() |> report()
# BayesFactor::anovaBF(Acceptance_2 ~ group_id, data = main_df_pareto_true, whichRandom="user_id")  |> bayesfactor_models() |> report()











main_logs_questionnaire_bo_start <- subset(main_logs_questionnaire, phase %in% c("exploration", "exploitation"))
main_logs_questionnaire_bo_start <- subset(main_logs_questionnaire_bo_start, group_id %!in% c("GroupC"))

main_logs_questionnaire_bo_start |>
  group_by(group_id, user_id) |>
  count() |>
  print(n = 200)


# Modify user_id based on their appearance in multiple groups

# --> this effectively makes that the group is added to the user_id
# main_logs_questionnaire_bo_start <- main_logs_questionnaire_bo_start %>%
#   group_by(user_id) %>%
#   mutate(num_groups = n_distinct(group_id)) %>%
#   ungroup() %>%
#   arrange(user_id, group_id, desc(run)) %>%
#   group_by(user_id, group_id) %>%
#   ungroup() %>%
#   mutate(user_id = ifelse(num_groups > 1,
#                               paste0(user_id, "_", group_id),
#                               as.character(user_id))) %>%
#   select(-num_groups)
#
# # Count the number of groupings for each user
# user_group_count <- main_logs_questionnaire_bo_start %>%
#   group_by(user_id, group_id) %>%
#   summarise(n = n()) %>%
#   ungroup() %>%
#   group_by(user_id) %>%
#   summarise(num_groups = n_distinct(group_id)) %>%
#   ungroup()
#
# # Filter users who appear in more than one group
# users_in_multiple_groups <- user_group_count %>%
#   filter(num_groups > 1)

# Print the result
# print(users_in_multiple_groups)




main_logs_questionnaire_bo_start$user_id <- as.factor(main_logs_questionnaire_bo_start$user_id)

checkAssumptionsForAnovaTwoFactors(data = main_logs_questionnaire_bo_start, y = "MentalLoad_1", factor_1 = "group_id", factor_2 = "run")

# TODO still problems with run

# lmer(MentalLoad_1 ~ group_id * run + (1 | user_id), data = main_logs_questionnaire_bo_start) |> easystats::model_dashboard(output_file = "lmer_MentalLoad_1.html")

# main_logs_questionnaire_bo_start$run <- as.numeric(main_logs_questionnaire_bo_start$run)
# test <- subset(main_logs_questionnaire_bo_start, run < 12)
# test$run <- as.factor(test$run)
# main_logs_questionnaire_bo_start$run <- as.factor(main_logs_questionnaire_bo_start$run)
#
# lmer(MentalLoad_1 ~ group_id * run + (1 | user_id), data = test) |> easystats::model_dashboard(output_file = "lmer_MentalLoad_1_test.html")


# modelArt <- art(MentalLoad_1 ~ group_id * run + Error(user_id / run), data = main_logs_questionnaire_bo_start) |> anova()
# modelArt
# reportART(modelArt, dv = "mental load")
#
# dunnTest(MentalLoad_1 ~ run, data = main_logs_questionnaire_bo_start, method = "holm") |> reportDunnTest(main_df = main_logs_questionnaire_bo_start, iv = "run", dv = "MentalLoad_1")


main_logs_questionnaire_bo_start$run <- as.numeric(as.character(main_logs_questionnaire_bo_start$run))

# TODO discuss
main_logs_questionnaire_bo_start$run <- ifelse(main_logs_questionnaire_bo_start$group_id == "GroupA", main_logs_questionnaire_bo_start$run, main_logs_questionnaire_bo_start$run + 4)

main_logs_questionnaire_bo_start |>
  group_by(user_id) |>
  summarise(max = max(run)) |>
  arrange(max) |>
  print(n = 200)
main_logs_questionnaire_bo_start |>
  group_by(user_id, group_id) |>
  summarise(max = max(run)) |>
  arrange(group_id, max) |>
  print(n = 200)

main_logs_questionnaire_bo_start %>%
  group_by(user_id) %>%
  summarise(max = max(run)) %>%
  filter(max < 15) %>%
  arrange(max) %>%
  print(n = 200)

# main_logs_questionnaire_bo_start$run <- ifelse(
#   main_logs_questionnaire_bo_start$group_id == "GroupA" | main_logs_questionnaire_bo_start$run == 1,
#   main_logs_questionnaire_bo_start$run,
#   main_logs_questionnaire_bo_start$run + 4
# )

main_logs_questionnaire_bo_start$run <- as.factor(main_logs_questionnaire_bo_start$run)


# # Calculate the means for the filtered groups (previous step)
# mean_per_group <- main_df %>%
#   filter(!(group_id %in% c("GroupA", "GroupB", "GroupE")) & run == 1) %>%
#   group_by(group_id) %>%
#   summarise(mean_MentalLoad_1 = mean(MentalLoad_1, na.rm = TRUE))
# 
# # Extract the mean values for each group to use in geom_hline
# mean_values <- setNames(mean_per_group$mean_MentalLoad_1, mean_per_group$group_id)
# 
# # Define the colors for each group from C1 to C6
# group_colors <- c("GroupD" = "grey", 
#                   "GroupF" = "#4d9068", 
#                   "GroupC" = "#d52e2e", 
#                   "GroupA" = "#2d78b0", 
#                   "GroupE" = "#fb8125", 
#                   "GroupB" = "#fcd676")
# 
# 
# short_legendLabels <- c(
#   "GroupC" = "C2-Custom design",
#   "GroupD" = "C1-No Vis.",
#   "GroupF" = "C3-End-user design"
# )
# 
# # Create a data frame for label positions
# label_positions <- data.frame(
#   group_id = c("GroupC", "GroupD", "GroupF"),
#   x = max(as.numeric(main_logs_questionnaire_bo_start$run)) + 0.5,  # Adjust as needed
#   y = mean_values[c("GroupC", "GroupD", "GroupF")],
#   label = short_legendLabels[c("GroupC", "GroupD", "GroupF")]
# )
# 
# # Ensure 'run' exists in your data
# max_run <- max(as.numeric(main_logs_questionnaire_bo_start$run, na.rm = TRUE))



main_logs_questionnaire_bo_start %>% ggplot() +
  aes(x = run, y = MentalLoad_1, fill = group_id, colour = group_id, group = group_id) +
  scale_fill_see(labels = legendLabels) +
  ylab("Cognitive load") +
  theme(legend.position.inside = c(0.65, 0.9)) +
  xlab("Iteration") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
  annotate("text", x = 3, y = 0.5, label = "Sampling") +
  geom_segment(aes(x = 1, y = 0.75, xend = 5.2, yend = 0.75), linetype = "dashed", color = "black") +
  annotate("text", x = 10, y = 0.5, label = "Optimization") +
  geom_segment(aes(x = 5.8, y = 0.75, xend = 15, yend = 0.75), color = "black") +
  # stat_smooth(method = "lm", alpha = 0.04, linetype = "dashed", linewidth = 0.5) +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  stat_poly_line(fullrange = FALSE, alpha = 0.1, linetype = "dashed", linewidth = 0.5) +
  geom_vline(aes(xintercept = 5), linetype = "dashed", color = "black", alpha = 0.5) +
  annotate("text", x = 5, y = 2, label = "Begin Warm Start Methods", vjust = 2, hjust = 0.5, color = "black") +
  scale_color_manual(labels = legendLabels, values = c("#2d78b0", "#fb8125", "#fcd676"))
  # # Add horizontal lines for group means
  # geom_hline(yintercept = mean_values[["GroupC"]], linetype = "dotted", color = group_colors[["GroupC"]], alpha = 0.7) +
  # geom_hline(yintercept = mean_values[["GroupD"]], linetype = "dotted", color = group_colors[["GroupD"]], alpha = 0.7) +
  # geom_hline(yintercept = mean_values[["GroupF"]], linetype = "dotted", color = group_colors[["GroupF"]], alpha = 0.7) 

ggsave("plots/bo_runs_day_1_mental.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)




anim_plot <- main_logs_questionnaire_bo_start %>%
  ggplot() +
  aes(x = run, y = MentalLoad_1, fill = group_id, colour = group_id, group = group_id) +
  scale_fill_see(labels = legendLabels) +
  ylab("Mental Workload") +
  theme(legend.title = element_blank(), axis.title = element_text(size = 20), axis.text = element_text(size = 18), plot.title = element_text(size = 28), plot.subtitle = element_text(size = 18), legend.background = element_blank(), legend.position = "inside", legend.position.inside = c(0.65, 0.28)) +
  xlab("Iteration") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9, position = position_dodge(width = 0.1)) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5, position = position_dodge(width = 0.1)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
  annotate("text", x = 3, y = 0.5, label = "Exploration") +
  geom_segment(aes(x = 1, y = 0.75, xend = 5.2, yend = 0.75), linetype = "dashed", color = "black") +
  annotate("text", x = 10, y = 0.5, label = "Optimization") +
  geom_segment(aes(x = 5.8, y = 0.75, xend = 15, yend = 0.75), color = "black") +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  stat_poly_line(fullrange = FALSE, alpha = 0.1, linetype = "dashed", linewidth = 0.5) +
  geom_vline(aes(xintercept = 5), linetype = "dashed", color = "black", alpha = 0.5) +
  annotate("text", x = 5, y = 2, label = "Begin Warm Start Methods", vjust = 2, hjust = 0.5, color = "black") +
  scale_color_manual(labels = legendLabels, values = c("#2d78b0", "#fb8125", "#fcd676")) +
  # Create animation
  transition_states(group_id, transition_length = 2, state_length = 1) +
  enter_fade() +
  shadow_mark() # shadow_mark to keep lines visible
# Save the animation as a video (MP4)
# animate(anim_plot, nframes = 100, width = 1920, height = 1080, renderer = av_renderer("plots/animation_mentalload.mp4"))



main_logs_questionnaire_bo_start %>% ggplot() +
  aes(x = run, y = trust, fill = group_id, colour = group_id, group = group_id) +
  scale_fill_see(labels = legendLabels) +
  ylab("Trust") +
  theme(legend.position.inside = c(0.65, 0.45)) +
  xlab("Iteration") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
  annotate("text", x = 3, y = 0.5, label = "Sampling") +
  geom_segment(aes(x = 1, y = 0.75, xend = 5.2, yend = 0.75), linetype = "dashed", color = "black") +
  annotate("text", x = 10, y = 0.5, label = "Optimization") +
  geom_segment(aes(x = 5.8, y = 0.75, xend = 15, yend = 0.75), color = "black") +
  # stat_smooth(method = "lm", alpha = 0.04, linetype = "dashed", linewidth = 0.5) +
  stat_poly_eq(use_label(c("eq", "R2")), label.y = c(0.3, 0.25, 0.2)) +
  stat_poly_line(fullrange = FALSE, alpha = 0.1, linetype = "dashed", linewidth = 0.5) +
  geom_vline(aes(xintercept = 5), linetype = "dashed", color = "black", alpha = 0.5) +
  annotate("text", x = 5, y = 1.2, label = "Begin Warm Start Methods", vjust = 2, hjust = 0.5, color = "black") +
  scale_color_manual(labels = legendLabels, values = c("#2d78b0", "#fb8125", "#fcd676"))
ggsave("plots/bo_runs_day_1_trust.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)




main_logs_questionnaire_bo_start %>% ggplot() +
  aes(x = run, y = predictability, fill = group_id, colour = group_id, group = group_id) +
  scale_fill_see(labels = legendLabels) +
  ylab("Predictability") +
  theme(legend.position.inside = c(0.65, 0.45)) +
  xlab("Iteration") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
  annotate("text", x = 3, y = 0.5, label = "Sampling") +
  geom_segment(aes(x = 1, y = 0.75, xend = 5.2, yend = 0.75), linetype = "dashed", color = "black") +
  annotate("text", x = 10, y = 0.5, label = "Optimization") +
  geom_segment(aes(x = 5.8, y = 0.75, xend = 15, yend = 0.75), color = "black") +
  # stat_smooth(method = "lm", alpha = 0.04, linetype = "dashed", linewidth = 0.5) +
  stat_poly_eq(use_label(c("eq", "R2")), label.y = c(0.3, 0.25, 0.2)) +
  stat_poly_line(fullrange = FALSE, alpha = 0.1, linetype = "dashed", linewidth = 0.5) +
  geom_vline(aes(xintercept = 5), linetype = "dashed", color = "black", alpha = 0.5) +
  annotate("text", x = 5, y = 1.2, label = "Begin Warm Start Methods", vjust = 2, hjust = 0.5, color = "black") +
  scale_color_manual(labels = legendLabels, values = c("#2d78b0", "#fb8125", "#fcd676"))
ggsave("plots/bo_runs_day_1_predictability.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


main_logs_questionnaire_bo_start %>% ggplot() +
  aes(x = run, y = perceivedSafetyScore, fill = group_id, colour = group_id, group = group_id) +
  scale_fill_see(labels = legendLabels) +
  ylab("Perceived safety") +
  theme(legend.position.inside = c(0.65, 0.25)) +
  xlab("Iteration") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
  annotate("text", x = 3, y = -0.5, label = "Sampling") +
  geom_segment(aes(x = 1, y = -0.25, xend = 5.2, yend = -0.25), linetype = "dashed", color = "black") +
  annotate("text", x = 10, y = -0.5, label = "Optimization") +
  geom_segment(aes(x = 5.8, y = -0.25, xend = 15, yend = -0.25), color = "black") +
  # stat_smooth(method = "lm", alpha = 0.04, linetype = "dashed", linewidth = 0.5) +
  stat_poly_eq(use_label(c("eq", "R2")), label.y = c(0.25, 0.2, 0.15)) +
  stat_poly_line(fullrange = FALSE, alpha = 0.1, linetype = "dashed", linewidth = 0.5) +
  geom_vline(aes(xintercept = 5), linetype = "dashed", color = "black", alpha = 0.5) +
  annotate("text", x = 5, y = 2.75, label = "Begin Warm Start Methods", vjust = 2, hjust = 0.5, color = "black") +
  scale_color_manual(labels = legendLabels, values = c("#2d78b0", "#fb8125", "#fcd676"))
ggsave("plots/bo_runs_day_1_perceived_safety.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



main_logs_questionnaire_bo_start %>% ggplot() +
  aes(x = run, y = Aesthetics_1, fill = group_id, colour = group_id, group = group_id) +
  scale_fill_see(labels = legendLabels) +
  ylab("Aesthetics") +
  theme(legend.position.inside = c(0.65, 0.45)) +
  xlab("Iteration") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
  annotate("text", x = 3, y = 0.5, label = "Sampling") +
  geom_segment(aes(x = 1, y = 0.75, xend = 5.2, yend = 0.75), linetype = "dashed", color = "black") +
  annotate("text", x = 10, y = 0.5, label = "Optimization") +
  geom_segment(aes(x = 5.8, y = 0.75, xend = 15, yend = 0.75), color = "black") +
  # stat_smooth(method = "lm", alpha = 0.04, linetype = "dashed", linewidth = 0.5) +
  stat_poly_eq(use_label(c("eq", "R2")), label.y = c(0.3, 0.25, 0.2)) +
  stat_poly_line(fullrange = FALSE, alpha = 0.1, linetype = "dashed", linewidth = 0.5) +
  geom_vline(aes(xintercept = 5), linetype = "dashed", color = "black", alpha = 0.5) +
  annotate("text", x = 5, y = 1.2, label = "Begin Warm Start Methods", vjust = 2, hjust = 0.5, color = "black") +
  scale_color_manual(labels = legendLabels, values = c("#2d78b0", "#fb8125", "#fcd676"))
ggsave("plots/bo_runs_day_1_aesthetics.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


main_logs_questionnaire_bo_start %>% ggplot() +
  aes(x = run, y = acceptance, fill = group_id, colour = group_id, group = group_id) +
  scale_fill_see(labels = legendLabels) +
  ylab("Acceptance") +
  theme(legend.position.inside = c(0.65, 0.45)) +
  xlab("Iteration") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
  annotate("text", x = 3, y = 0.5, label = "Sampling") +
  geom_segment(aes(x = 1, y = 0.75, xend = 5.2, yend = 0.75), linetype = "dashed", color = "black") +
  annotate("text", x = 10, y = 0.5, label = "Optimization") +
  geom_segment(aes(x = 5.8, y = 0.75, xend = 15, yend = 0.75), color = "black") +
  # stat_smooth(method = "lm", alpha = 0.04, linetype = "dashed", linewidth = 0.5) +
  stat_poly_eq(use_label(c("eq", "R2")), label.y = c(0.3, 0.25, 0.2)) +
  stat_poly_line(fullrange = FALSE, alpha = 0.1, linetype = "dashed", linewidth = 0.5) +
  geom_vline(aes(xintercept = 5), linetype = "dashed", color = "black", alpha = 0.5) +
  annotate("text", x = 5, y = 1.2, label = "Begin Warm Start Methods", vjust = 2, hjust = 0.5, color = "black") +
  scale_color_manual(labels = legendLabels, values = c("#2d78b0", "#fb8125", "#fcd676"))
ggsave("plots/bo_runs_day_1_acceptance.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



# main_logs_questionnaire_bo_start %>% ggplot() +
#   aes(x = run, y = Acceptance_2, fill = group_id, colour = group_id, group = group_id) +
#   scale_fill_see(labels = legendLabels) +
#   ylab("Satisfying") +
#   theme(legend.position.inside = c(0.65, 0.45)) +
#   xlab("Iteration") +
#   stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
#   stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.3) +
#   stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
#   annotate("text", x = 3, y = 0.5, label = "Sampling") +
#   geom_segment(aes(x = 1, y = 0.75, xend = 5.2, yend = 0.75), linetype = "dashed", color = "black") +
#   annotate("text", x = 10, y = 0.5, label = "Optimization") +
#   geom_segment(aes(x = 5.8, y = 0.75, xend = 15, yend = 0.75), color = "black") +
#   # stat_smooth(method = "lm", alpha = 0.04, linetype = "dashed", linewidth = 0.5) +
#   stat_poly_eq(use_label(c("eq", "R2")), label.y = c(0.3, 0.25, 0.2)) +
#   stat_poly_line(fullrange = FALSE, alpha = 0.1, linetype = "dashed", linewidth = 0.5) +
#   geom_vline(aes(xintercept = 5), linetype = "dashed", color = "black", alpha = 0.5) +
#   annotate("text", x = 5, y = 1.2, label = "Begin Warm Start Methods", vjust = 2, hjust = 0.5, color = "black") +
#   scale_color_manual(labels = legendLabels, values = c("#2d78b0", "#fb8125", "#fcd676"))
# ggsave("plots/bo_runs_day_1_acceptance_2_pareto_true.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)




write_xlsx(main_logs_questionnaire_bo_start, path = "bo_start_day1.xlsx")





#### Longitudinal ####

main_logs_questionnaire_no_bo_longitudinal$longitudinal_id <- as.factor(main_logs_questionnaire_no_bo_longitudinal$longitudinal_id)
main_logs_questionnaire_no_bo_longitudinal$run <- as.factor(main_logs_questionnaire_no_bo_longitudinal$run)


main_logs_questionnaire_no_bo_longitudinal <- subset(main_logs_questionnaire_no_bo_longitudinal, run != "0")

# Create a new column with the value after the underscore to know whether BO was available in the longitudinal version
main_logs_questionnaire_no_bo_longitudinal <- main_logs_questionnaire_no_bo_longitudinal %>%
  mutate(longitudinalBo = str_extract(longitudinal_id, "(?<=_)[A-Za-z0-9]+$"))

main_logs_questionnaire_no_bo_longitudinal$longitudinalBo <- as.factor(main_logs_questionnaire_no_bo_longitudinal$longitudinalBo)

main_logs_questionnaire_no_bo_longitudinal |>
  group_by(group_id, longitudinalBo, user_id) |>
  count() |>
  print(n = 200)


# TODO for test reasons
main_logs_filtered <- main_logs_questionnaire_no_bo_longitudinal %>%
  group_by(group_id, longitudinalBo, user_id) %>%
  filter(n() == 3) %>%
  ungroup()

unique(main_logs_filtered$user_id)

# TODO throws an error
# checkAssumptionsForAnovaThreeFactors(data = main_logs_filtered, y = "MentalLoad_1", factor_1 = "group_id", factor_2 = "run", factor_3 = "longitudinalBo")

main_logs_filtered$user_id <- as.factor(main_logs_filtered$user_id)
levels(main_logs_filtered$user_id)

main_logs_filtered |>
  group_by(user_id) |>
  count() |>
  print(n = 100)

modelArt <- art(MentalLoad_1 ~ group_id * longitudinalBo * run + Error(user_id / run), data = main_logs_filtered) |> anova()
modelArt
reportART(modelArt, dv = "mental load")

dunnTest(MentalLoad_1 ~ run, data = main_logs_filtered, method = "holm") |> reportDunnTest(main_df = main_logs_filtered, iv = "run", dv = "MentalLoad_1")
reportMeanAndSD(main_df = main_logs_filtered, iv = "longitudinalBo", dv = "MentalLoad_1")
reportMeanAndSD(main_df = main_logs_filtered, iv = "run", dv = "MentalLoad_1")



main_logs_filtered %>% ggplot() +
  aes(x = longitudinalBo, y = MentalLoad_1, fill = group_id, colour = group_id, group = group_id) +
  scale_fill_see(labels = legendLabels) +
  ylab("Cognitive load") +
  theme(legend.position.inside = c(0.35, 0.85)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.05), alpha = 0.5) +
  scale_color_manual(labels = legendLabels, values = c("grey", "#4d9068", "#d52e2e", "#2d78b0", "#fb8125", "#fcd676"))
ggsave("plots/tlx1_score_interaction_longitudinal_long_group.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)

main_logs_filtered %>% ggplot() +
  aes(x = run, y = MentalLoad_1, fill = longitudinalBo, colour = longitudinalBo, group = longitudinalBo) +
  scale_color_see(labels = legendLabels) +
  scale_fill_see(labels = legendLabels) +
  ylab("Cognitive load") +
  theme(legend.position.inside = c(0.65, 0.85)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.05), alpha = 0.5)
# stat_poly_eq(use_label(c("eq", "R2")), label.y = c(0.3, 0.25, 0.2, 0.15, 0.1, 0.05)) +
# stat_poly_line(fullrange = FALSE,  alpha = 0.1, linetype = "dashed", linewidth = 0.5)
ggsave("plots/tlx1_score_interaction_longitudinal_long_run.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)










checkAssumptionsForAnovaThreeFactors(data = main_logs_filtered, y = "trust", factor_1 = "group_id", factor_2 = "run", factor_3 = "longitudinalBo")

modelArt <- art(trust ~ group_id * longitudinalBo * run + Error(user_id / run), data = main_logs_filtered) |> anova()
modelArt
reportART(modelArt, dv = "trust")

dunnTest(trust ~ group_id, data = main_logs_filtered, method = "holm") |> reportDunnTest(main_df = main_logs_filtered, iv = "group_id", dv = "trust")
# dunnTest(trust ~ longitudinalBo, data = main_logs_filtered, method = "holm") #|> reportDunnTest(main_df = main_logs_filtered, iv = "longitudinalBo", dv = "trust")


# reportMeanAndSD(main_df = main_logs_filtered, iv = "group_id", dv = "trust")
reportMeanAndSD(main_df = main_logs_filtered, iv = "longitudinalBo", dv = "trust") # mit BO höher!!! wuhu

main_logs_filtered %>% ggplot() +
  aes(x = group_id, y = trust, fill = run, colour = run, group = run) +
  # scale_colour_manual(values = wes_palette("Cavalcanti1", n = 3)) +
  scale_color_see(labels = legendLabels) +
  scale_fill_see(labels = legendLabels) +
  ylab("Trust") +
  theme(legend.position.inside = c(0.65, 0.85)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
  scale_x_discrete(labels = labels_x_lab)





# checkAssumptionsForAnovaThreeFactors(data = main_logs_filtered, y = "predictability", factor_1 = "group_id", factor_2 = "run", factor_3 = "longitudinalBo")

modelArt <- art(predictability ~ group_id * longitudinalBo * run + Error(user_id / run), data = main_logs_filtered) |> anova()
modelArt
reportART(modelArt, dv = "predictability")

dunnTest(predictability ~ group_id, data = main_logs_filtered, method = "holm") |> reportDunnTest(main_df = main_logs_filtered, iv = "group_id", dv = "predictability")

# reportMeanAndSD(main_df = main_logs_filtered, iv = "group_id", dv = "predictability")
reportMeanAndSD(main_df = main_logs_filtered, iv = "longitudinalBo", dv = "predictability") # mit BO höher!!! wuhu



main_logs_filtered %>% ggplot() +
  aes(x = group_id, y = predictability, fill = run, colour = run, group = run) +
  # scale_colour_manual(values = wes_palette("Cavalcanti1", n = 3)) +
  scale_color_see(labels = legendLabels) +
  scale_fill_see(labels = legendLabels) +
  ylab("Predictability") +
  theme(legend.position.inside = c(0.65, 0.85)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
  scale_x_discrete(labels = labels_x_lab)







# checkAssumptionsForAnovaThreeFactors(data = main_logs_filtered, y = "perceivedSafetyScore", factor_1 = "group_id", factor_2 = "run", factor_3 = "longitudinalBo")

modelArt <- art(perceivedSafetyScore ~ group_id * longitudinalBo * run + Error(user_id / run), data = main_logs_filtered) |> anova()
modelArt
reportART(modelArt, dv = "perceivedSafetyScore")

dunnTest(perceivedSafetyScore ~ group_id, data = main_logs_filtered, method = "holm") |> reportDunnTest(main_df = main_logs_filtered, iv = "group_id", dv = "perceivedSafetyScore")


# reportMeanAndSD(main_df = main_logs_filtered, iv = "group_id", dv = "perceivedSafetyScore")
reportMeanAndSD(main_df = main_logs_filtered, iv = "longitudinalBo", dv = "perceivedSafetyScore") # mit BO höher!!! wuhu



main_logs_filtered %>% ggplot() +
  aes(x = group_id, y = perceivedSafetyScore, fill = run, colour = run, group = run) +
  # scale_colour_manual(values = wes_palette("Cavalcanti1", n = 3)) +
  scale_color_see(labels = legendLabels) +
  scale_fill_see(labels = legendLabels) +
  ylab("Perceived Safety") +
  theme(legend.position.inside = c(0.65, 0.85)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
  scale_x_discrete(labels = labels_x_lab)



# some groups do not have Aesthetics_1, Acceptance_1, and Acceptance_2 -> remove those rows for special df
main_logs_filtered_no_Aes_Acc <- subset(main_logs_filtered, !is.na(Aesthetics_1))
main_logs_filtered_no_Aes_Acc <- subset(main_logs_filtered_no_Aes_Acc, !is.na(Acceptance_1))
main_logs_filtered_no_Aes_Acc <- subset(main_logs_filtered_no_Aes_Acc, !is.na(Acceptance_2))



# checkAssumptionsForAnovaThreeFactors(data = main_logs_filtered_no_Aes_Acc, y = "Aesthetics_1", factor_1 = "group_id", factor_2 = "run", factor_3 = "longitudinalBo")

modelArt <- art(Aesthetics_1 ~ group_id * longitudinalBo * run + Error(user_id / run), data = main_logs_filtered_no_Aes_Acc) |> anova()
modelArt
reportART(modelArt, dv = "aesthetics")

dunnTest(Aesthetics_1 ~ group_id, data = main_logs_filtered_no_Aes_Acc, method = "holm") |> reportDunnTest(main_df = main_logs_filtered_no_Aes_Acc, iv = "group_id", dv = "Aesthetics_1")



main_logs_filtered_no_Aes_Acc %>% ggplot() +
  aes(x = run, y = Aesthetics_1, fill = longitudinalBo, colour = longitudinalBo, group = longitudinalBo) +
  # scale_colour_manual(values = wes_palette("Cavalcanti1", n = 3)) +
  scale_color_see(labels = legendLabels) +
  scale_fill_see(labels = legendLabels) +
  ylab("Aesthetics") +
  theme(legend.position.inside = c(0.65, 0.85)) +
  xlab("Day") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
  scale_x_discrete(labels = labels_x_lab)
# p +  facet_wrap(~group_id, labeller = labeller(group_id = legendLabels))
ggsave("plots/aesthectics_interaction_longitudinal_long_run_pareto_true.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



# checkAssumptionsForAnovaThreeFactors(data = main_logs_filtered_no_Aes_Acc, y = "Acceptance_1", factor_1 = "group_id", factor_2 = "run", factor_3 = "longitudinalBo")

modelArt <- art(Acceptance_1 ~ group_id * longitudinalBo * run + Error(user_id / run), data = main_logs_filtered_no_Aes_Acc) |> anova()
modelArt
reportART(modelArt, dv = "Usefulness")




main_logs_filtered_no_Aes_Acc %>% ggplot() +
  aes(x = run, y = Acceptance_1, fill = longitudinalBo, colour = longitudinalBo, group = longitudinalBo) +
  # scale_colour_manual(values = wes_palette("Cavalcanti1", n = 3)) +
  scale_color_see(labels = legendLabels) +
  scale_fill_see(labels = legendLabels) +
  ylab("Usefulness") +
  theme(legend.position.inside = c(0.65, 0.85)) +
  xlab("Day") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
  scale_x_discrete(labels = labels_x_lab)
# p +  facet_wrap(~group_id, labeller = labeller(group_id = legendLabels))
ggsave("plots/usefulness_interaction_longitudinal_long_run_pareto_true.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


library(transformr)

main_logs_filtered_no_Aes_Acc$runNumeric <- as.numeric(main_logs_filtered_no_Aes_Acc$run)

main_logs_filtered_no_Aes_Acc %>% ggplot() +
  aes(x = run, y = Acceptance_1, fill = longitudinalBo, colour = longitudinalBo, group = longitudinalBo) +
  # scale_colour_manual(values = wes_palette("Cavalcanti1", n = 3)) +
  scale_color_see(labels = legendLabels) +
  scale_fill_see(labels = legendLabels) +
  ylab("Usefulness") +
  theme(legend.position.inside = c(0.65, 0.85)) +
  xlab("Day") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
  scale_x_discrete(labels = labels_x_lab) +
  transition_states(states = run, transition_length = 2, state_length = 1) +
  ease_aes("linear")

# checkAssumptionsForAnovaThreeFactors(data = main_logs_filtered_no_Aes_Acc, y = "Acceptance_2", factor_1 = "group_id", factor_2 = "run", factor_3 = "longitudinalBo")

modelArt <- art(Acceptance_2 ~ group_id * longitudinalBo * run + Error(user_id / run), data = main_logs_filtered_no_Aes_Acc) |> anova()
modelArt
reportART(modelArt, dv = "satisfying")


dunnTest(Acceptance_2 ~ group_id, data = main_logs_filtered_no_Aes_Acc, method = "holm") |> reportDunnTest(main_df = main_logs_filtered_no_Aes_Acc, iv = "group_id", dv = "Acceptance_2")


# reportMeanAndSD(main_df = main_logs_filtered_no_Aes_Acc, iv = "group_id", dv = "Acceptance_2")
reportMeanAndSD(main_df = main_logs_filtered_no_Aes_Acc, iv = "longitudinalBo", dv = "Acceptance_2") # mit BO höher!!! wuhu









p <- main_logs_filtered_no_Aes_Acc %>% ggplot() +
  aes(x = run, y = Acceptance_2, fill = longitudinalBo, colour = longitudinalBo, group = longitudinalBo) +
  # scale_colour_manual(values = wes_palette("Cavalcanti1", n = 3)) +
  scale_color_see(labels = legendLabels) +
  scale_fill_see(labels = legendLabels) +
  ylab("Satisfying") +
  theme(legend.position.inside = c(0.65, 0.85)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) +
  scale_x_discrete(labels = labels_x_lab)
p + facet_wrap(~group_id, labeller = labeller(group_id = legendLabels))
