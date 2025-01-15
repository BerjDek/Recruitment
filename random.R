# Define the generalized function with customizable response levels
create_message_comparison_plot <- function(data, col1, col2, labels, title, response_levels = NULL) {
  # Check if response_levels is provided, otherwise use unique levels from the data
  if (is.null(response_levels)) {
    response_levels <- unique(unlist(data %>% select(col1, col2)))
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
      Response = factor(Response, levels = response_levels) # Use custom or default levels
    )
  
  # Create the plot
  ggplot(
    plot_data %>%
      group_by(MessageTheme, Response) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(MessageTheme) %>%
      mutate(Percentage = Count / sum(Count) * 100),
    aes(x = Response, y = Count, fill = MessageTheme)
  ) +
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
}



create_message_comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme",
  response_levels = c("Very likely", "Somewhat likely", "Very unlikely", "Somewhat unlikely")
)

create_message_comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme",
  response_levels = c("Very likely", "Somewhat likely","Neutral", "Very unlikely", "Somewhat unlikely")
)


create_message_comparison_plot(
  data = data_reduced,
  col1 = "Feelings_SciMsg",
  col2 = "Feelings_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Feelings by Message Theme",
  response_levels = c("Inspired", "Obligated", "Indifferent")
)







# Define the generalized function with NA control and filtered percentages
create_message_comparison_plot <- function(data, col1, col2, labels, title, response_levels = NULL, include_na = FALSE) {
  # Check if response_levels is provided, otherwise use unique levels from the data
  if (is.null(response_levels)) {
    response_levels <- unique(unlist(data %>% select(col1, col2)))
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
      Response = factor(Response, levels = response_levels)
    )
  
  # Include or exclude NA values
  if (!include_na) {
    plot_data <- plot_data %>% filter(!is.na(Response))
  }
  
  # Filter data to only include selected response levels
  plot_data <- plot_data %>% filter(Response %in% response_levels)
  
  # Calculate counts and percentages for selected responses
  summary_data <- plot_data %>%
    group_by(MessageTheme, Response) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(MessageTheme) %>%
    mutate(Percentage = Count / sum(Count) * 100)
  
  # Create the plot
  ggplot(summary_data, aes(x = Response, y = Count, fill = MessageTheme)) +
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
}




create_message_comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"),
  include_na = FALSE
)



create_message_comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme",
  response_levels = c("Very likely",  "Somewhat unlikely", "Very unlikely"),
  include_na = TRUE
)





# Define the function with proper NA handling
create_message_comparison_plot <- function(data, col1, col2, labels, title, response_levels = NULL, include_na = FALSE) {
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
      Response = factor(Response, levels = response_levels, exclude = if (!include_na) NULL else NULL)
    )
  
  # Filter data to include only selected response levels (NA included if specified)
  plot_data <- plot_data %>% filter(Response %in% response_levels | (include_na & is.na(Response)))
  
  # Calculate counts and percentages for selected responses
  summary_data <- plot_data %>%
    group_by(MessageTheme, Response) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(MessageTheme) %>%
    mutate(Percentage = Count / sum(Count) * 100)
  
  # Create the plot
  ggplot(summary_data, aes(x = Response, y = Count, fill = MessageTheme)) +
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
}


create_message_comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"),
  include_na = TRUE
)


create_message_comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme",
  response_levels = c("Very likely", "Somewhat likely", "Neutral",  "Very unlikely"),
  include_na = FALSE
)

summary(data_reduced)







create_message_comparison_plot <- function(data, col1, col2, labels, title, response_levels = NULL, include_na = FALSE, show_totals = FALSE) {
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
  
  # Calculate total counts and NA counts
  total_counts <- plot_data %>%
    group_by(MessageTheme) %>%
    summarise(TotalCount = n(), NA_Count = sum(is.na(Response)), .groups = "drop")
  
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
        x = length(response_levels) - 0.1, # Adjust x-position near the end
        y = max(summary_data$Count) * 1.1, # Adjust y-position above the bars
        label = paste0(
          labels[1], ": Total = ", total_counts$TotalCount[1], 
          ", NA = ", total_counts$NA_Count[1], "\n",
          labels[2], ": Total = ", total_counts$TotalCount[2],
          ", NA = ", total_counts$NA_Count[2]
        ),
        hjust = 1,
        size = 2
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

summary(data_reduced)


data_reduced <- data_reduced %>%
  mutate("Participation_Likelihood_SciMsg" = as.factor("Participation_Likelihood_SciMsg"))

data_reduced <- data_reduced %>%
  mutate(Participation_Likelihood_SciMsg = as.factor(Participation_Likelihood_SciMsg))




create_message_comparison_plot <- function(data, col1, col2, labels, title, response_levels = NULL, include_na = FALSE, show_totals = FALSE, total_text_size = 4) {
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
  
  # Calculate total counts, NA counts, and total number of responses
  total_responses <- nrow(data) # Total number of observations in the dataset
  total_counts <- plot_data %>%
    group_by(MessageTheme) %>%
    summarise(
      TotalCount = n(), 
      NA_Count = sum(is.na(Response)), 
      .groups = "drop"
    ) %>%
    mutate(
      TotalFraction = paste0(TotalCount, "/", total_responses),
      NAFraction = paste0(NA_Count, "/", total_responses)
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
        x = length(response_levels) - 0.1, # Adjust x-position near the end
        y = max(summary_data$Count) * 1.1, # Adjust y-position above the bars
        label = paste0(
          labels[1], ": Total = ", total_counts$TotalFraction[1], 
          ", NA = ", total_counts$NAFraction[1], "\n",
          labels[2], ": Total = ", total_counts$TotalFraction[2],
          ", NA = ", total_counts$NAFraction[2]
        ),
        hjust = 1,
        size = total_text_size
      )
  }
  
  return(plot)
}



create_message_comparison_plot <- function(data, col1, col2, labels, title, response_levels = NULL, include_na = FALSE, show_totals = FALSE, total_text_size = 4) {
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
  
  # Calculate total counts, NA # Calculate total counts, NA counts, and fractions from the original dataset
  total_responses <- nrow(data) # Total number of observations in the dataset
  total_counts <- data %>%
    summarise(
      Scientific_Total = sum(!is.na(.[[col1]])), # Total non-NA responses for col1
      Scientific_NA = sum(is.na(.[[col1]])),    # NA count for col1
      Environmental_Total = sum(!is.na(.[[col2]])), # Total non-NA responses for col2
      Environmental_NA = sum(is.na(.[[col2]]))  # NA count for col2
    ) %>%
    mutate(
      ScientificFraction = paste0(Scientific_Total, "/", total_responses),
      ScientificNAFraction = paste0(Scientific_NA, "/", total_responses),
      EnvironmentalFraction = paste0(Environmental_Total, "/", total_responses),
      EnvironmentalNAFraction = paste0(Environmental_NA, "/", total_responses)
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
        x = length(response_levels) - 0.1, # Adjust x-position near the end
        y = max(summary_data$Count) * 1.1, # Adjust y-position above the bars
        label = paste0(
          labels[1], ": Total = ", total_counts$ScientificFraction, 
          ", NA = ", total_counts$ScientificNAFraction, "\n",
          labels[2], ": Total = ", total_counts$EnvironmentalFraction,
          ", NA = ", total_counts$EnvironmentalNAFraction
        ),
        hjust = 1,
        size = total_text_size
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
  show_totals = TRUE,
  total_text_size = 2
)




create_message_comparison_plot <- function(data, col1, col2, labels, title, response_levels = NULL, include_na = FALSE, show_totals = FALSE) {
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
      EnvironmentalNAFraction = paste0(Environmental_NA, "/", total_responses)
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



#the last thing you did is removed the part in the function that makes the NA appear as a fraction in the sceintific total.