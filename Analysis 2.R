
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

#third version of function

comparison_plot <- function(data, col1, col2, labels, title, response_levels = NULL, include_na = FALSE, show_totals = FALSE, combine_levels = NULL, facet_by = NULL, facet_levels = NULL) {
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
      y = "Count",
    ) +
    scale_y_continuous(breaks = seq(0, max(summary_data$Count) * 1.2, by = 25)) + # Adjust `by` for tick steps
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 7),  # Y-axis tick size
      axis.title.x = element_text(size = 9), # X-axis label size
      axis.title.y = element_text(size = 9), # Y-axis label size
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5), # Title size, bold font, and center alignment
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



comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), # percentage will only consider chosen ones
  facet_by = "Employment_Status", # Facet by employment status
  facet_levels = c("Employed", "Student"), # Only include "Employed" and "Student"
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely"), #if no facets are required the answer should be NULL
  include_na = FALSE,   #includes bars for NA
  show_totals = TRUE
)



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




# Impact of Msg on Participation Likelihood --------


## Participating ------------------------------------------------------------


comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",   
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),    
  title = "Participation Likelihood by Message Theme",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  show_totals = TRUE
)

# Around one third is neutral regardless of message, similar results mostly 


##Combined Likely/Unlikely ------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme, Combined Likley and Unlikely",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ),
  include_na = FALSE,
  show_totals = TRUE
)


## Participation By Gender -------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Gender",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Gender", 
  facet_levels = list("Male", "Female"),
  include_na = FALSE,
)


## Participation By Education -------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Age",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Education_Level",
  facet_levels = list("A-levels", "Primary school", "Secondary school", "University (University/technical school)"),
  include_na = FALSE,
  show_totals = TRUE
)
