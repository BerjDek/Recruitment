
# Load Libraries ----------------------------------------------------------
library(MASS) 
library(tidyverse)



summary(data_reduced)

# Motivations & Barriers --------------------------------------------------


# Functions ---------------------------------------------------------------

## Function to Create likert Plot-------------------------------------------
create_average_plot <- function(data, columns, 
                                title = "Average Ratings", 
                                x_label = "Variables", 
                                y_label = "Average Rating") {
  
  # Select and calculate the mean of the specified columns
  avg_data <- data %>%
    select(all_of(columns)) %>%
    summarise(across(everything(), mean, na.rm = TRUE)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Mean") %>%
    arrange(desc(Mean))
  
  # Visualization
  ggplot(avg_data, aes(x = reorder(Variable, -Mean), y = Mean)) +  # Order by decreasing Mean
    geom_bar(stat = "identity", fill = "#377eb8", width = 0.6, color = "black") + 
    geom_text(aes(label = round(Mean, 2)), vjust = -0.5, size = 3.5) + # Add mean on top of bars
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      panel.grid.major = element_line(color = "gray", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
}

## Sample How to use it
create_average_plot(
  data = data_reduced, 
  columns = c("Motivation_TopicInterest", "Motivation_NewExperience", "Motivation_Enjoyable"), 
  x_label = "Motivators and Barriers",
  y_label = "Average Likert Rating"
)



# Motivators --------------------------------------------------------------

## Summary ---------------------------

motivation_cols  <-  c("Motivation_Topic_Interest", "Motivation_New_Experience", "Motivation_Enjoyable",
                      "Motivation_Share_Achievements", "Motivation_Join_Friends_Family", 
                      "Motivation_Improve_Community", "Motivation_Protect_Environment",
                      "Motivation_Meet_Other_Volunteers", "Motivation_Scientific_Research", "Motivation_Teach_Others",
                      "Motivation_Reduce_Risks")

create_average_plot(
  data = data_reduced, 
  columns = motivation_columns , 
  x_label = "Motivators",
  y_label = "Average Likert Rating"
)


# Interestingly the highest motivators very close to each other are scientific research first and protecting the environment second

## ANOVA ---------------------------

motivation_long <- data_reduced %>%     # Reshape the data from wide to long format
  select(all_of(motivation_cols)) %>%
  pivot_longer(cols = everything(),
               names_to = "Motivation",
               values_to = "Score")


anova_results <- aov(Score ~ Motivation, data = motivation_long)
summary(anova_results) # result is significant  at  p = <2e-16 ***

TukeyHSD(anova_results)

# based on Tukey multiple comparison of the means, both Scientific Research and Protecting the Environment are significantly
# rated higher than the 7 lowest motivators, while Topic of Interest and reducing risk significantly more important than the lowest three


## Ordinal regression model ---------------------------
### Answers to Likelihood of Participating After reading the Env Message, compared to Motivation ratings ----------------------
data_reduced$Participation_Likelihood_EnvMsg <- factor(data_reduced$Participation_Likelihood_EnvMsg,
                                                      levels = c("Very unlikely", 
                                                                 "Somewhat unlikely", 
                                                                 "Neutral", 
                                                                 "Somewhat likely", 
                                                                 "Very likely"),
                                                      ordered = TRUE)

model <- polr(Participation_Likelihood_EnvMsg ~ 
                data_reduced$Motivation_Topic_Interest + 
                data_reduced$Motivation_New_Experience + 
                data_reduced$Motivation_Enjoyable + 
                data_reduced$Motivation_Share_Achievements + 
                data_reduced$Motivation_Join_Friends_Family + 
                data_reduced$Motivation_Improve_Community + 
                data_reduced$Motivation_Protect_Environment + 
                data_reduced$Motivation_Meet_Other_Volunteers + 
                data_reduced$Motivation_Scientific_Research + 
                data_reduced$Motivation_Teach_Others + 
                data_reduced$Motivation_Reduce_Risks,
              data = data_reduced, 
              Hess = TRUE)

summary(model)


#ordinal regression analysis to investigate how various motivations predict the likelihood of respondents participating in 
#environmental activities, showed that  that motivations usually rated lower, such as gaining new experiences, teaching
#others, joining friends/family, and contributing to scientific research significantly predict a higher likelihood of 
#participation while motivations like protecting the environment or reducing risks appear less influential in driving participation likelihood


### Answers to Participation Frequency reading the Env Message, compared to Motivation ratings -------------

data_reduced$Participation_Likelihood_EnvMsg <- factor(data_reduced$Participation_Frequency_EnvMsg,
                                                       levels = c("Daily", 
                                                                  "Once per week",
                                                                  "Once per month",
                                                                  "Once or twice per year", 
                                                                  "Never"),
                                                       ordered = TRUE)

model <- polr(Participation_Frequency_EnvMsg ~ 
                data_reduced$Motivation_Topic_Interest + 
                data_reduced$Motivation_New_Experience + 
                data_reduced$Motivation_Enjoyable + 
                data_reduced$Motivation_Share_Achievements + 
                data_reduced$Motivation_Join_Friends_Family + 
                data_reduced$Motivation_Improve_Community + 
                data_reduced$Motivation_Protect_Environment + 
                data_reduced$Motivation_Meet_Other_Volunteers + 
                data_reduced$Motivation_Scientific_Research + 
                data_reduced$Motivation_Teach_Others + 
                data_reduced$Motivation_Reduce_Risks,
              data = data_reduced, 
              Hess = TRUE)

summary(model)

# again Motivations like enjoyment, teaching others, and improving the community drive higher participation frequency,

# Barriers --------------------------------------------------------------

## Summary ---------------------------

create_average_plot(
  data = data_reduced, 
  columns = c("Barrier_Lack_Of_Time", "Barrier_Unaware_Of_Projects", "Barrier_Not_Interested", 
              "Barrier_Insufficient_Resources", "Barrier_No_Aquaintances", "Barrier_DataSharing_Concerns", 
              "Barrier_No_Relevant_Data_To_Collect", "Barrier_Doesnt_Volunteer", "Barrier_CS_Not_Credible"), 
  x_label = "Barriers",
  y_label = "Average Likert Rating"
)

# The highest rated barrier seems to be lack of awareness, followed by lack of time and aquaintances.
