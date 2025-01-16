#first version

comparison_plot <- function(data, col1, col2, labels, title, response_levels = NULL, include_na = FALSE, show_totals = FALSE) {
  # Check if response_levels is provided, otherwise use unique levels from the data
  if (is.null(response_levels)) {
    response_levels <- unique(unlist(data %>% select(col1, col2)))
  }
  
  # Add NA as a level if include_na is TRUE
  if (include_na) {
    response_levels <- c(response_levels, NA)
  }
  
  # Pivot and prepare the data
  plot_data <- data %>%
    pivot_longer(
      cols = c(col1, col2),
      names_to = "MessageTheme",
      values_to = "Response"
    ) %>%
    mutate(
      MessageTheme = recode(MessageTheme,
                            !!col1 := labels[1],
                            !!col2 := labels[2]),
      Response = factor(Response, levels = response_levels, exclude = NULL)
    )
  
  # Include or exclude NA values
  if (!include_na) {
    plot_data <- plot_data %>% filter(!is.na(Response))
  }
  
  # Filter data to include only selected response levels
  plot_data <- plot_data %>% filter(Response %in% response_levels)
  
  # Calculate counts and percentages for selected responses
  summary_data <- plot_data %>%
    group_by(MessageTheme, Response) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(MessageTheme) %>%
    mutate(Percentage = Count / sum(Count) * 100)
  
  # Calculate total counts, NA counts, and fractions from the original dataset
  total_responses <- nrow(data) # Total number of observations in the dataset
  total_counts <- data %>%
    summarise(
      Scientific_Total = sum(.[[col1]] %in% response_levels, na.rm = TRUE), # Count responses in response_levels
      Scientific_NA = sum(is.na(.[[col1]])),                               # NA count for col1
      Environmental_Total = sum(.[[col2]] %in% response_levels, na.rm = TRUE), # Count responses in response_levels
      Environmental_NA = sum(is.na(.[[col2]]))                            # NA count for col2
    ) %>%
    mutate(
      ScientificFraction = paste0(Scientific_Total, "/", total_responses),
      ScientificNAFraction = paste0(Scientific_NA),
      EnvironmentalFraction = paste0(Environmental_Total, "/", total_responses),
      EnvironmentalNAFraction = paste0(Environmental_NA)     # copy this to the end to make it a fraction again [, "/", total_responses]
    )
  
  # Create the plot
  plot <- ggplot(summary_data, aes(x = Response, y = Count, fill = MessageTheme)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", width = 0.6) +
    geom_text(
      aes(label = sprintf("%.1f%%", Percentage), group = MessageTheme),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 2.5
    ) +
    scale_fill_manual(
      name = "Message Theme",
      values = setNames(c("#377eb8", "#4daf4a"), labels) # Assign colors to labels
    ) +
    labs(
      title = title,
      x = "Response Category",
      y = "Count"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "gray", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
  
  # Add total counts and NA counts if requested
  if (show_totals) {
    plot <- plot +
      annotate(
        "text",
        x = length(response_levels) + 1, # Adjust x-position near the end
        y = max(summary_data$Count) * 1.1, # Adjust y-position above the bars
        label = paste0(
          labels[1], ": Total = ", total_counts$ScientificFraction, 
          ", NA = ", total_counts$ScientificNAFraction, "\n",
          labels[2], ": Total = ", total_counts$EnvironmentalFraction,
          ", NA = ", total_counts$EnvironmentalNAFraction
        ),
        hjust = 1,
        size = 2 # Fixed text size
      )
  }
  
  return(plot)
}




create_message_comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"),
  include_na = FALSE,
  show_totals = TRUE
)

create_message_comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme",
  response_levels = c("Very likely", "Somewhat likely", "Somewhat unlikely", "Very unlikely"),
  include_na = FALSE,
  show_totals = TRUE
)



# second version so that we can allow combining different response levels.


comparison_plot <- function(data, col1, col2, labels, title, response_levels = NULL, include_na = FALSE, show_totals = FALSE, combine_levels = NULL) {
  # Check if response_levels is provided, otherwise use unique levels from the data
  if (is.null(response_levels)) {
    response_levels <- unique(unlist(data %>% select(col1, col2)))
  }
  
  # Add NA as a level if include_na is TRUE
  if (include_na) {
    response_levels <- c(response_levels, NA)
  }
  
  # Pivot and prepare the data
  plot_data <- data %>%
    pivot_longer(
      cols = c(col1, col2),
      names_to = "MessageTheme",
      values_to = "Response"
    ) %>%
    mutate(
      MessageTheme = recode(MessageTheme,
                            !!col1 := labels[1],
                            !!col2 := labels[2])
    )
  
  # Combine response levels if specified
  if (!is.null(combine_levels)) {
    plot_data <- plot_data %>%
      mutate(Response = recode(Response, !!!combine_levels))
  }
  
  # Convert Response to a factor with the specified levels
  plot_data <- plot_data %>%
    mutate(Response = factor(Response, levels = unique(unlist(c(response_levels, names(combine_levels)))))
    )
  
  # Include or exclude NA values
  if (!include_na) {
    plot_data <- plot_data %>% filter(!is.na(Response))
  }
  
  # Filter data to include only selected response levels
  plot_data <- plot_data %>% filter(Response %in% response_levels | Response %in% names(combine_levels))
  
  # Calculate counts and percentages for selected responses
  summary_data <- plot_data %>%
    group_by(MessageTheme, Response) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(MessageTheme) %>%
    mutate(Percentage = Count / sum(Count) * 100)
  
  # Recalculate total counts based on combined levels
  total_counts <- plot_data %>%
    group_by(MessageTheme) %>%
    summarise(
      Total = n(),                 # Total responses for the theme
      NA_Count = sum(is.na(Response)) # NA count for the theme
    ) %>%
    mutate(
      Fraction = paste0(Total, "/", nrow(data)),
      NA_Display = paste0(NA_Count) # Just display NA counts
    )
  
  # Create the plot
  plot <- ggplot(summary_data, aes(x = Response, y = Count, fill = MessageTheme)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", width = 0.6) +
    geom_text(
      aes(label = sprintf("%.1f%%", Percentage), group = MessageTheme),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 2.5
    ) +
    scale_fill_manual(
      name = "Message Theme",
      values = setNames(c("#377eb8", "#4daf4a"), labels) # Assign colors to labels
    ) +
    labs(
      title = title,
      x = "Response Category",
      y = "Count"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "gray", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
  
  # Add total counts and NA counts if requested
  if (show_totals) {
    plot <- plot +
      annotate(
        "text",
        x = length(response_levels) + 1, # Adjust x-position near the end
        y = max(summary_data$Count) * 1.1, # Adjust y-position above the bars
        label = paste0(
          labels[1], ": Total = ", total_counts$Fraction[1], 
          ", NA = ", total_counts$NA_Display[1], "\n",
          labels[2], ": Total = ", total_counts$Fraction[2],
          ", NA = ", total_counts$NA_Display[2]
        ),
        hjust = 1,
        size = 2 # Fixed text size
      )
  }
  
  return(plot)
}



create_message_comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme",
  response_levels = c("Likely", "Neutral", "Somewhat unlikely", "Very unlikely"), # Keep new combined level
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ),
  include_na = FALSE,
  show_totals = TRUE
)


create_message_comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme, Combined Likley and Unlikely",
  response_levels = c("Likely",  "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ),
  include_na = FALSE,
  show_totals = TRUE
)



# Third Version, allows Facets.



create_message_comparison_plot <- function(data, col1, col2, labels, title, response_levels = NULL, include_na = FALSE, show_totals = FALSE, combine_levels = NULL, facet_by = NULL, facet_levels = NULL) {
  # Check if response_levels is provided, otherwise use unique levels from the data
  if (is.null(response_levels)) {
    response_levels <- unique(unlist(data %>% select(col1, col2)))
  }
  
  # Add NA as a level if include_na is TRUE
  if (include_na) {
    response_levels <- c(response_levels, NA)
  }
  
  # Pivot and prepare the data
  plot_data <- data %>%
    pivot_longer(
      cols = c(col1, col2),
      names_to = "MessageTheme",
      values_to = "Response"
    ) %>%
    mutate(
      MessageTheme = recode(MessageTheme,
                            !!col1 := labels[1],
                            !!col2 := labels[2])
    )
  
  # Combine response levels if specified
  if (!is.null(combine_levels)) {
    plot_data <- plot_data %>%
      mutate(Response = recode(Response, !!!combine_levels))
  }
  
  # Convert Response to a factor with the specified levels
  plot_data <- plot_data %>%
    mutate(Response = factor(Response, levels = unique(unlist(c(response_levels, names(combine_levels)))))
    )
  
  # Include or exclude NA values
  if (!include_na) {
    plot_data <- plot_data %>% filter(!is.na(Response))
  }
  
  # Filter data to include only selected response levels
  plot_data <- plot_data %>% filter(Response %in% response_levels | Response %in% names(combine_levels))
  
  # Filter facet levels if specified
  if (!is.null(facet_by) && !is.null(facet_levels)) {
    plot_data <- plot_data %>%
      filter(!!sym(facet_by) %in% facet_levels)
  }
  
  # Calculate counts and percentages for selected responses
  if (!is.null(facet_by)) {
    summary_data <- plot_data %>%
      group_by(MessageTheme, Response, !!sym(facet_by)) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(MessageTheme, !!sym(facet_by)) %>%
      mutate(Percentage = Count / sum(Count) * 100)
  } else {
    summary_data <- plot_data %>%
      group_by(MessageTheme, Response) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(MessageTheme) %>%
      mutate(Percentage = Count / sum(Count) * 100)
  }
  
  # Recalculate total counts based on combined levels
  total_counts <- plot_data %>%
    group_by(MessageTheme) %>%
    summarise(
      Total = n(),                 # Total responses for the theme
      NA_Count = sum(is.na(Response)) # NA count for the theme
    ) %>%
    mutate(
      Fraction = paste0(Total, "/", nrow(data)),
      NA_Display = paste0(NA_Count) # Just display NA counts
    )
  
  # Create the plot
  plot <- ggplot(summary_data, aes(x = Response, y = Count, fill = MessageTheme)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", width = 0.6) +
    geom_text(
      aes(label = sprintf("%.1f%%", Percentage), group = MessageTheme),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 2.5
    ) +
    scale_fill_manual(
      name = "Message Theme",
      values = setNames(c("#377eb8", "#4daf4a"), labels) # Assign colors to labels
    ) +
    labs(
      title = title,
      x = "Response Category",
      y = "Count"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "gray", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
  
  # Add faceting if specified
  if (!is.null(facet_by)) {
    plot <- plot +
      facet_wrap(vars(!!sym(facet_by)), scales = "free_y")
  }
  
  # Add total counts and NA counts if requested
  if (show_totals) {
    plot <- plot +
      annotate(
        "text",
        x = length(response_levels) + 1, # Adjust x-position near the end
        y = max(summary_data$Count) * 1.1, # Adjust y-position above the bars
        label = paste0(
          labels[1], ": Total = ", total_counts$Fraction[1], 
          ", NA = ", total_counts$NA_Display[1], "\n",
          labels[2], ": Total = ", total_counts$Fraction[2],
          ", NA = ", total_counts$NA_Display[2]
        ),
        hjust = 1,
        size = 2 # Fixed text size
      )
  }
  
  return(plot)
}



create_message_comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"),
  facet_by = "Employment_Status", # Facet by employment status
  facet_levels = c("Employed", "Student"), # Only include "Employed" and "Student"
  combine_levels = NULL,
  include_na = FALSE,
  show_totals = TRUE
)


#forth version (fixing how total and NA is calculated per facet)

#rework and start showing different totals the more facets are added

