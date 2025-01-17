
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





#Variables --------
## Citizen Science Prior Participation-------------------------------------------------------------------
create_sorted_bar_chart(data_reduced, Participated_In_CitizenScience, "Prior Participation in CS")

#only 4.4% participated in citizen science.

## Citizen Science Prior Awareness-------------------------------------------------------------------
create_sorted_bar_chart(data_reduced, Awareness_Of_CitizenScience, "Prior Awareness of CS")

# only 7.4% heard of citizen science.

## Usefulness of Citizen Science -------------------------------------------------------------------
create_sorted_bar_chart(data_reduced, CitizenScience_Usefulness, "Usefulness of CS")

# 45% think its useful, only 12.2 think its not, the rest don't know.




## Group of First Read Message -------------------------------------------------------------------
create_sorted_bar_chart(data_reduced, Group , "Which Message They Read First")

# A bit more of the participants read the scientific message first (51.3%), don't know why there is an NA count here

#  Participation Likelihood --------



## Theme ------------------------------------------------------------


comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",   
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),    
  title = "Participation Likelihood by Message Theme",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  show_totals = FALSE
)

# Around one third is neutral regardless of message, similar results mostly 


## Combined Likely/Unlikely ------------------------------------------------

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
  show_totals = FALSE
)


## By Gender -------------------------------------------------

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


comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Gender, Combined Likley and Unlikely",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ),
  facet_by = "Gender", 
  facet_levels = list("Male", "Female"),
  include_na = FALSE,
  show_totals = FALSE
)


#Simplifying to likelihood, it seems in general males are more likely to participate, and both seem slightly more convinced by the scientifc message


## By Age -------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Age",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Age_Group",
  facet_levels = list("18-25", "26-44", "45-60", "60+"),
  include_na = FALSE,
)


comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Age, Combined Likley and Unlikely",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ),
  facet_by = "Age_Group",
  facet_levels = list("18-25", "26-44", "45-60", "60+"),
  include_na = FALSE,
  show_totals = FALSE
)


#Interestingly younger crowds are more likely to participate


## By Education -------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Education",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Education_Level",
  facet_levels = list("A-levels", "Secondary school", "University (University/technical school)"),
  include_na = FALSE,
  show_totals = FALSE
)


comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Education",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "Education_Level",
  facet_levels = list("A-levels", "Secondary school", "University (University/technical school)"),
  include_na = FALSE
)


# University students are way more likely to participate, with scientific message seemingly more effective to most

## By Employment Status -------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Employment Status",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Employment_Status",
  facet_levels = list("Employed", "Retired", 
                      "Homemaker", "Student"),
  include_na = FALSE,
  show_totals = FALSE
)


comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Employment Status, combined neutral exluded",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "Employment_Status",
  facet_levels = list("Employed", "Retired", 
                      "Homemaker", "Student"),
  include_na = FALSE
)


# Surprisingly not a lot of traction from Retired folk (also students and homemakers), with those employed being the most likley

## By Occupation -------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Occupation",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Occupation_Type",
  facet_levels = list("Other (please specify)", "Business, Finance, or Management (e.g., Accountant, HR, Sales, Marketing)", 
                      "Healthcare (e.g., Doctor, Nurse, Therapist)", "Information Technology (e.g., IT Support, Software Developer)", "Technical & Legal (e.g., Lawyer, Engineer, Governmental Worker)"),
  include_na = FALSE,
  show_totals = FALSE
)


comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Occupation",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "Occupation_Type",
  facet_levels = list("Other (please specify)", "Business, Finance, or Management (e.g., Accountant, HR, Sales, Marketing)", 
                      "Healthcare (e.g., Doctor, Nurse, Therapist)", "Information Technology (e.g., IT Support, Software Developer)", "Technical & Legal (e.g., Lawyer, Engineer, Governmental Worker)"),
  include_na = FALSE
)


# Those from information technology domain stand out as most likely to participate regardless of theme


## By Neighborhood  -------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Neighborhood Type",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Neighborhood_Type",
  facet_levels = list("Rural", "Suburban", 
                      "Urban (city center)", "Other (please specify)"),
  include_na = FALSE,
  show_totals = FALSE
)


comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Neighborhood Type, combined neutral exluded",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "Neighborhood_Type",
  facet_levels = list("Rural", "Suburban", 
                      "Urban (city center)", "Other (please specify)"),
  include_na = FALSE
)


# Those more in cit center more likely to participate, those in the suburbs seem to prefer the environmental message more



## By Building -------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Building Type",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "BuildingType",
  facet_levels = list("Apartment complex", "Detached single-family house", 
                      "Multifamily house", "Other (please specify)"),
  include_na = FALSE,
  show_totals = FALSE
)


comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Building Type, combined neutral exluded",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "BuildingType",
  facet_levels = list("Apartment complex", "Detached single-family house", 
                      "Multifamily house", "Other (please specify)"),
  include_na = FALSE
)



## By Read First-------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Read-First",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Group",
  facet_levels = list("SciMsgFirst", "EnvMsgFirst"),
  include_na = FALSE,
  show_totals = FALSE
)


comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Read-First, combined neutral exluded",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "Group",
  facet_levels = list("SciMsgFirst", "EnvMsgFirst"),
  include_na = FALSE
)

# In general it seems getting the message first makes it more unlikely to participate in that type of project

## By Usefulness-------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Usefulness",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "CitizenScience_Usefulness",
  facet_levels = list("Yes", "Not sure", "No"),
  include_na = FALSE,
  show_totals = FALSE
)


comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Usefulness, combined neutral exluded",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "CitizenScience_Usefulness",
  facet_levels = list("Yes", "Not sure", "No"),
  include_na = FALSE
)

# As expected those who think CS is not useful or unsure of usefulness not likely to participate

## By Awareness-------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Awareness",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Awareness_Of_CitizenScience",
  facet_levels = list("Yes", "Not sure", "No"),
  include_na = FALSE,
  show_totals = FALSE
)

comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Awareness, combined neutral exluded",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "Awareness_Of_CitizenScience",
  facet_levels = list("Yes", "Not sure", "No"),
  include_na = FALSE
)


#The few that are aware are way more likely to participate, scientific message seems to have the edge in both cases


## By Prior Participated-------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Prior participation",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Participated_In_CitizenScience",
  facet_levels = list("Yes", "Not sure", "No"),
  include_na = FALSE,
  show_totals = FALSE
)

comparison_plot(
  data = data_reduced,
  col1 = "Participation_Likelihood_SciMsg",
  col2 = "Participation_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Participation Likelihood by Message Theme And Prior Participation, combined neutral exluded",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "Participated_In_CitizenScience",
  facet_levels = list("Yes", "Not sure", "No"),
  include_na = FALSE
)


#Similar to awareness those that have previously participated in CS are way more likely to participate again,
#scientific message seems to have the edge for those that participated before and scientific by a bit for those who havenet




# Impact of Msg on Recommendation Likelihood --------
summary(data_reduced)

## General ------------------------------------------------------------


comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",   
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),    
  title = "Recommendation Likelihood by Message Theme",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  show_totals = FALSE
)

# Again around one third is neutral and does not care about recommending regardless of message, 
#unlike Recommendation, those very likely to recommend do so after reading the environmental message

##Combined Likely/Unlikely ------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme, Combined Likley and Unlikely",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ),
  include_na = FALSE,
  show_totals = FALSE
)


## By Gender -------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Gender",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Gender", 
  facet_levels = list("Male", "Female"),
  include_na = FALSE,
)


comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Gender, Combined Likley and Unlikely",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ),
  facet_by = "Gender", 
  facet_levels = list("Male", "Female"),
  include_na = FALSE,
  show_totals = FALSE
)


#Males are also more likely to recommend, and unlike participation both seem  more convinced by the Environmental message


## By Age -------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Age",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Age_Group",
  facet_levels = list("18-25", "26-44", "45-60", "60+"),
  include_na = FALSE,
)


comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Age, Combined Likley and Unlikely",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ),
  facet_by = "Age_Group",
  facet_levels = list("18-25", "26-44", "45-60", "60+"),
  include_na = FALSE,
  show_totals = FALSE
)


#The likelihood of recommending increases for older folk when compared to participating. Those younger than 25 more likley
# to recommend after the scientific message, with older than 25 more inclined after environmental


##  By Education -------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Education",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Education_Level",
  facet_levels = list("A-levels", "Secondary school", "University (University/technical school)"),
  include_na = FALSE,
  show_totals = FALSE
)


comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Education",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "Education_Level",
  facet_levels = list("A-levels", "Secondary school", "University (University/technical school)"),
  include_na = FALSE
)


# University students are still more likely to recommend, with environmental message seemingly more effective to most

## By Employment Status -------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Employment Status",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Employment_Status",
  facet_levels = list("Employed", "Retired", 
                      "Homemaker", "Student"),
  include_na = FALSE,
  show_totals = FALSE
)


comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Employment Status, combined neutral exluded",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "Employment_Status",
  facet_levels = list("Employed", "Retired", 
                      "Homemaker", "Student"),
  include_na = FALSE
)


# From Participation to recommendation, students switch being more likely to recommend than not, and contrary to others being more likley to do so after scientific message
# those employed still being the most likely to recommend in after both them, but mote more for environmental, 
# Retired folk are interesting, likely to recommend after environmental and unlikely after scientific.

## By Occupation -------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Occupation",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Occupation_Type",
  facet_levels = list("Other (please specify)", "Business, Finance, or Management (e.g., Accountant, HR, Sales, Marketing)", 
                      "Healthcare (e.g., Doctor, Nurse, Therapist)", "Information Technology (e.g., IT Support, Software Developer)", "Technical & Legal (e.g., Lawyer, Engineer, Governmental Worker)"),
  include_na = FALSE,
  show_totals = FALSE
)


comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Occupation",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "Occupation_Type",
  facet_levels = list("Other (please specify)", "Business, Finance, or Management (e.g., Accountant, HR, Sales, Marketing)", 
                      "Healthcare (e.g., Doctor, Nurse, Therapist)", "Information Technology (e.g., IT Support, Software Developer)", "Technical & Legal (e.g., Lawyer, Engineer, Governmental Worker)"),
  include_na = FALSE
)


# Business technical and healthcare workers would only be likely to recommend after env message
# those in Tech, still by far the highest for recommending, and unlike participation would do so after env message


## By Neighborhood  -------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Neighborhood Type",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Neighborhood_Type",
  facet_levels = list("Rural", "Suburban", 
                      "Urban (city center)", "Other (please specify)"),
  include_na = FALSE,
  show_totals = FALSE
)


comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Neighborhood Type, combined neutral exluded",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "Neighborhood_Type",
  facet_levels = list("Rural", "Suburban", 
                      "Urban (city center)", "Other (please specify)"),
  include_na = FALSE
)


# Unlike for participation, regardless of neighborhood type, most would recommend after environmental message,
#Those in rural areas wont after the scientific message.



## By Building -------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Building Type",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "BuildingType",
  facet_levels = list("Apartment complex", "Detached single-family house", 
                      "Multifamily house", "Other (please specify)"),
  include_na = FALSE,
  show_totals = FALSE
)


comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Building Type, combined neutral exluded",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "BuildingType",
  facet_levels = list("Apartment complex", "Detached single-family house", 
                      "Multifamily house", "Other (please specify)"),
  include_na = FALSE
)


# Those in detached homes most likely to and would recommend regardless of message theme, all others after environmental message only




## By Read First-------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Read-First",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Group",
  facet_levels = list("SciMsgFirst", "EnvMsgFirst"),
  include_na = FALSE,
  show_totals = FALSE
)


comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Read-First, combined neutral exluded",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "Group",
  facet_levels = list("SciMsgFirst", "EnvMsgFirst"),
  include_na = FALSE
)

# Weirdly reading the Scientific Message first, makes it way more likely to recommend the Environmental Project,


## By Usefulness-------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Usefulness",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "CitizenScience_Usefulness",
  facet_levels = list("Yes", "Not sure", "No"),
  include_na = FALSE,
  show_totals = FALSE
)


comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Usefulness, combined neutral exluded",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "CitizenScience_Usefulness",
  facet_levels = list("Yes", "Not sure", "No"),
  include_na = FALSE
)

# Those unsure of CS usefulness are way more likely to recommend it after the environmental message

## By Awareness-------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Awareness",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Awareness_Of_CitizenScience",
  facet_levels = list("Yes", "Not sure", "No"),
  include_na = FALSE,
  show_totals = FALSE
)

comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Awareness, combined neutral exluded",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "Awareness_Of_CitizenScience",
  facet_levels = list("Yes", "Not sure", "No"),
  include_na = FALSE
)


#In all categories of awareness the environmental message makes it more likely to recommended

## By Prior Participated-------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Prior participation",
  response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
  facet_by = "Participated_In_CitizenScience",
  facet_levels = list("Yes", "Not sure", "No"),
  include_na = FALSE,
  show_totals = FALSE
)

comparison_plot(
  data = data_reduced,
  col1 = "Recommend_Likelihood_SciMsg",
  col2 = "Recommend_Likelihood_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Recommendation Likelihood by Message Theme And Prior Participation, combined neutral exluded",
  response_levels = c("Likely", "Unlikely"), 
  combine_levels = list(
    "Very likely" = "Likely",
    "Somewhat likely" = "Likely",
    "Very unlikely" = "Unlikely",
    "Somewhat unlikely" = "Unlikely"
  ), 
  facet_by = "Participated_In_CitizenScience",
  facet_levels = list("Yes", "Not sure", "No"),
  include_na = FALSE
)
#For those who have participated as well, recommending is higher for environmental.

# Impact of Msg on Feeling --------
## General ------------------------------------------------------------


comparison_plot(
  data = data_reduced,
  col1 = "Feelings_SciMsg",   
  col2 = "Feelings_EnvMsg",
  labels = c("Scientific", "Environmental"),    
  title = "Feelings by Message Theme",
  response_levels = c("Inspired", "Indifferent", "Obligated", "Other (please specify)"), 
  show_totals = FALSE
)

# Again this time almost half (45 percent) are indifferent after reading the message, although messages 
#seem more inspiring than obligating, both types of messages are identical in how they make people feel.
#The other category should be checked for those that gave more detail.



## By Gender -------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Feelings_SciMsg",
  col2 = "Feelings_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Feelings by Message Theme And Gender",
  response_levels = c("Inspired", "Indifferent", "Obligated"), 
  facet_by = "Gender", 
  facet_levels = list("Male", "Female"),
  include_na = FALSE,
)


#Males generally feel more obligated than females, ad environmental messages seem more inspiring to them than 
#females, who feel more inspired by scientific messages



## By Age -------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Feelings_SciMsg",
  col2 = "Feelings_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Feelings by Message Theme And Age",
  response_levels = c("Inspired", "Indifferent", "Obligated"), 
  facet_by = "Age_Group",
  facet_levels = list("18-25", "26-44", "45-60", "60+"),
  include_na = FALSE,
)

#Obligation seems to drop with age, highest level of indifference for those between 46-60, 26 to 44 most inspired

## By Education -------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Feelings_SciMsg",
  col2 = "Feelings_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Feelings by Message Theme And Education",
  response_levels = c("Inspired", "Indifferent", "Obligated"), 
  facet_by = "Education_Level",
  facet_levels = list("A-levels", "Secondary school", "University (University/technical school)"),
  include_na = FALSE,
  show_totals = FALSE
)


# University students most inspired (more by environmental messages), secondary and a levels more inspired by scientific

## By Employment Status -------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Feelings_SciMsg",
  col2 = "Feelings_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Feelings by Message Theme And Employment Status",
  response_levels = c("Inspired", "Indifferent", "Obligated"), 
  facet_by = "Employment_Status",
  facet_levels = list("Employed", "Retired", 
                      "Homemaker", "Student"),
  include_na = FALSE,
  show_totals = FALSE
)


# employment status seems not too likely to differentiate feelings.


## By Occupation -------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Feelings_SciMsg",
  col2 = "Feelings_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Feelings by Message Theme And Occupation",
  response_levels = c("Inspired", "Indifferent", "Obligated"), 
  facet_by = "Occupation_Type",
  facet_levels = list("Other (please specify)", "Business, Finance, or Management (e.g., Accountant, HR, Sales, Marketing)", 
                      "Healthcare (e.g., Doctor, Nurse, Therapist)", "Information Technology (e.g., IT Support, Software Developer)", "Technical & Legal (e.g., Lawyer, Engineer, Governmental Worker)"),
  include_na = FALSE,
  show_totals = FALSE
)

#Healthcare workers feel the least obligated, generally scientific message more inspiring


## By Neighborhood  -------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Feelings_SciMsg",
  col2 = "Feelings_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Feelings by Message Theme And Neighborhood Type",
  response_levels = c("Inspired", "Indifferent", "Obligated"), 
  facet_by = "Neighborhood_Type",
  facet_levels = list("Rural", "Suburban", 
                      "Urban (city center)", "Other (please specify)"),
  include_na = FALSE,
  show_totals = FALSE
)




# neighberhood doesnt seem to impact feeling much



## By Building -------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Feelings_SciMsg",
  col2 = "Feelings_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Feelings by Message Theme And Building Type",
  response_levels = c("Inspired", "Indifferent", "Obligated"), 
  facet_by = "BuildingType",
  facet_levels = list("Apartment complex", "Detached single-family house", 
                      "Multifamily house", "Other (please specify)"),
  include_na = FALSE,
  show_totals = FALSE
)

#neither does building type


## By Read First-------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Feelings_SciMsg",
  col2 = "Feelings_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Feelings by Message Theme And Read-First",
  response_levels = c("Inspired", "Indifferent", "Obligated"), 
  facet_by = "Group",
  facet_levels = list("SciMsgFirst", "EnvMsgFirst"),
  include_na = FALSE,
  show_totals = FALSE
)



# Whichever message is read first is more inspiring, in general less indifferent rates when environmental message is read first


## By Usefulness-------------------------------------------------
summary(data_reduced)
comparison_plot(
  data = data_reduced,
  col1 = "Feelings_SciMsg",
  col2 = "Feelings_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Feelings by Message Theme And Usefulness",
  response_levels = c("Inspired", "Indifferent", "Obligated"),
  facet_by = "CitizenScience_Usefulness",
  facet_levels = list("Yes", "Not sure", "No"),
  include_na = FALSE,
  show_totals = FALSE
)


# Those unsure of CS usefulness feel more obligated 

## By Awareness-------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Feelings_SciMsg",
  col2 = "Feelings_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Feelings by Message Theme And Awareness",
  response_levels = c("Inspired", "Indifferent", "Obligated"),
  facet_by = "Awareness_Of_CitizenScience",
  facet_levels = list("Yes", "Not sure", "No"),
  include_na = FALSE,
  show_totals = FALSE
)



## By Prior Participated-------------------------------------------------

comparison_plot(
  data = data_reduced,
  col1 = "Feelings_SciMsg",
  col2 = "Feelings_EnvMsg",
  labels = c("Scientific", "Environmental"),
  title = "Feelings by Message Theme And Prior participation",
  response_levels = c("Inspired", "Indifferent", "Obligated"), 
  facet_by = "Participated_In_CitizenScience",
  facet_levels = list("Yes", "Not sure", "No"),
  include_na = FALSE,
  show_totals = FALSE
)


#For those who have not participated scientific is a bit more inspiring, but for those who have not bit more obligating



#repeat the codes above for number of participation
#for the variables include one that is for which one they prefer when asked directly.
#create a visualization for the motivation and barrier ratings.
#create a plot for relation between motivation/barrier and 