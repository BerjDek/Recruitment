
# Load Data and Packages --------------------------------------------------

data_reduced <- read.csv("data_reduced.csv")
library(tidyverse)


# Functions ---------------------------------------------------------------

## Function to Create Bar Charts for Categorical Demographic Variables -------------------------------------------
create_sorted_bar_chart <- function(data, column, title) {
  na_count <- sum(is.na(data[[deparse(substitute(column))]]))
  ggplot(
    data %>%
      filter(!is.na({{ column }})) %>%
      count({{ column }}) %>%
      mutate(Percentage = n / sum(n) * 100) %>%
      arrange(desc(Percentage)),
    aes(x = reorder(as.character({{ column }}), -Percentage), y = n, fill = as.factor({{ column }}))
  ) +
    geom_bar(stat = "identity", color = "black", width = 0.7, show.legend = FALSE) +
    geom_text(
      aes(label = sprintf("%.1f%%", Percentage)),
      position = position_stack(vjust = 0.5),
      size = 4
    ) +
    labs(
      title = paste(title, sprintf("(NA Count: %d)", na_count)),
      x = "Category",
      y = "Count"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}


## Function to Create a Comparison Plot ------------------------------------

# in order to work, there should be two separate columns getting similar answers, in this case for example how respondents feel after 
# reading the environmental message which is in one column and after reading the scientific which is in another, could also work for 
# the answers if they would recommend the project or participate in it after reading the message.


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
        x = length(response_levels) + 1, # Adjust x-position near the end
        y = max(summary_data$Count) * 1.1, # Adjust y-position above the bars
        label = paste0(
          labels[1], ": Total = ", total_counts$ScientificFraction, 
          ", NA = ", total_counts$ScientificNAFraction, "\n",
          labels[2], ": Total = ", total_counts$EnvironmentalFraction,
          ", NA = ", total_counts$EnvironmentalNAFraction
        ),
        hjust = 1,
        size = 2 # size of the total and NA 
      )
  }
  return(plot)
}


# Demographics ------------------------------------------------------------

## Gender ------------------------------------------------------------------

create_sorted_bar_chart(data_reduced, Gender, "Gender Distribution")


## Age Groups --------------------------------------------------------------


create_sorted_bar_chart(data_reduced, Age_Group, "Age Distribution")



## Education Level ---------------------------------------------------------

create_sorted_bar_chart(data_reduced, Education_Level, "Education Level")


## Employment Status -------------------------------------------------------


create_sorted_bar_chart(data_reduced, Employment_Status, "Employment Status Distribution")

## Occupation Type ---------------------------------------------------------


create_sorted_bar_chart(data_reduced, Occupation_Type, "Occupation Type Distribution")

## Neighborhood Type -------------------------------------------------------


create_sorted_bar_chart(data_reduced, Neighborhood_Type, "Neighborhood Type Distribution")


## Building Type -----------------------------------------------------------


create_sorted_bar_chart(data_reduced, BuildingType, "Building Type Distribution")


## State -------------------------------------------------------------------


create_sorted_bar_chart(data_reduced, Bundesland, "State Distribution")





