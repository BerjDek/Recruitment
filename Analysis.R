
install.packages("viridis")
library(viridis)
library(ggplot2)




# ---------------------------------------------------------------------
# 1. Message Effectiveness: Likelihood to Participate by Message Theme
# ---------------------------------------------------------------------


likelihood_data <- data_reduced %>%
  pivot_longer(
    cols = c("Participation_Likelihood_SciMsg", "Participation_Likelihood_EnvMsg"),
    names_to = "MessageTheme",
    values_to = "Response"
  ) %>%
  mutate(
    MessageTheme = recode(MessageTheme,
                          "Participation_Likelihood_SciMsg" = "Scientific",
                          "Participation_Likelihood_EnvMsg" = "Environmental"),
    Response = factor(Response, 
                      levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely", "NA"))
  )



# Grouped bar chart to compare message effectiveness for likelihood  to participate

ggplot(
  likelihood_data %>%
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
    values = c("Scientific" = "#377eb8", "Environmental" = "#4daf4a") # Custom colors
  ) +
  labs(
    title = "Participation Likelihood by Message Theme",
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



# COMPARISIN BY WHICH MESSAGE WAS RECIEVED FIRST to compare message effectiveness for likelihood to participate

ggplot(
  likelihood_data %>%
    filter(!is.na(Group)) %>%
    group_by(Group, MessageTheme, Response) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(Group, MessageTheme) %>%
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
    values = c("Scientific" = "#377eb8", "Environmental" = "#4daf4a") 
  ) +
  labs(
    title = "Participation Likelihood by Message Theme and Message Order",
    x = "Response Category",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  facet_wrap(~ Group, scales = "free_y")



# Summary of likelihood to Participate Based on Message
likelihood_data %>%
  group_by(MessageTheme, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(MessageTheme) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  arrange(MessageTheme, Response) %>%
  print()

# Percentage of Likely to Participate Responses for Each Message
likelihood_data %>%
  group_by(MessageTheme, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(MessageTheme) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  filter(Response %in% c("Very likely", "Somewhat likely")) %>%
  summarise(Total_Likely = sum(Percentage), .groups = "drop") %>%
  print()                                                            #Environmental 29.1    Scientific 29.7

# Percentage of  likelihood to Participate Based on Message Order 
likelihood_data %>%
  filter(!is.na(Group)) %>%  # Exclude rows with missing Group
  group_by(Group, MessageTheme, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Group, MessageTheme) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  filter(Response %in% c("Very likely", "Somewhat likely")) %>%
  summarise(Total_Likely = sum(Percentage), .groups = "drop") %>%
  arrange(Group, MessageTheme) %>%
  print()                                                         # SciMsgFirst: Environmental 30.1   Scientific  30.7;  EnvMsgFirst: Environmental  28.9  Scientific  29.5



#  Summary:

# Both Messages seem to have a similar effect when it came to their impact on likelihood of respondents to participate
# Looking at those that were likely to participate (combining Very and Somewhat Likely) Envoirnmental had 29.1% and Scientific 29.7%
# Which message got sent first did not really change the result. BUT its interesting to see that Scientific message first seems to
# yield higher likelihood to participate



# ---------------------------------------------------------------------
# 2. Demographic Preferences: Do demographic groups rate message themes differently?
# ---------------------------------------------------------------------

# Summary of Preferences by Gender

gender_summary <- likelihood_data %>%
  filter(Gender %in% c("Male", "Female")) %>%  
  group_by(Gender, MessageTheme, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Gender, MessageTheme) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  arrange(Gender, MessageTheme, Response)



# Visualization: Preferences by Gender

ggplot(
  gender_summary,  # Use the pre-existing dataset
  aes(x = Response, y = Percentage, fill = MessageTheme)
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
    values = c("Scientific" = "#377eb8", "Environmental" = "#4daf4a")
  ) +
  labs(
    title = "Participation Likelihood by Gender (Male and Female) and Message Theme",
    x = "Response Category",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  facet_wrap(~ Gender, scales = "free_y")



# Visualization excluding seeing the Neutral and NA answers (their percentages included still)
ggplot(
  gender_summary %>%
    filter(Response %in% c("Very likely", "Somewhat likely", "Somewhat unlikely", "Very unlikely")),  # Exclude Neutral and NA
  aes(x = Response, y = Percentage, fill = MessageTheme)
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
    values = c("Scientific" = "#377eb8", "Environmental" = "#4daf4a")
  ) +
  labs(
    title = "Participation Likelihood by Gender and Message Theme (Excluding Neutral Responses)",
    x = "Response Category",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  facet_wrap(~ Gender, scales = "free_y")



# Visualization of percentages if Neutral and NA answers were excluded

ggplot(
  likelihood_data %>%
    filter(Gender %in% c("Male", "Female")) %>%  
    group_by(Gender, MessageTheme, Response) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(Gender, MessageTheme) %>%
    mutate(Percentage = Count / sum(Count) * 100),
  aes(x = Response, y = Percentage, fill = MessageTheme)
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
    values = c("Scientific" = "#377eb8", "Environmental" = "#4daf4a")
  ) +
  labs(
    title = "Participation Likelihood by Gender (Male and Female) and Message Theme",
    x = "Response Category",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  facet_wrap(~ Gender, scales = "free_y")


# Summary of those likely to participate based on Gender 

likelihood_data %>%
  filter(Gender %in% c("Male", "Female")) %>%  
  group_by(Gender, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Gender) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  filter(Response %in% c("Very likely", "Somewhat likely")) %>%  
  summarise(Total_Likely = sum(Percentage), .groups = "drop") %>%
  arrange(Gender) %>%
  print()                                                          #  Female 26.4    Male 34.2


#summary of those likely to participate based on Gender & The Type of Message

likelihood_data %>%
  filter(Gender %in% c("Male", "Female")) %>%  
  group_by(Gender, MessageTheme, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Gender, MessageTheme) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  filter(Response %in% c("Very likely", "Somewhat likely")) %>%  
  summarise(Total_Likely = sum(Percentage), .groups = "drop") %>%
  arrange(Gender, MessageTheme) %>%
  print()                                  #Female: Environmental 26.2 Scientific 26.6; Male:   Environmental 33.7    Scientific 34.6


# Findings: Male respondents show a higher likelihood of participation (34.2%) compared to females (26.4%) 
# when combining "Very likely" and "Somewhat likely" responses

#Both genders display a slight preference for the Scientific message theme: Female E(26.2%) S(26.6%), Male E(33.7%) S(34.6%)


# Statistical Analysis

#Are males more likely to participate than females?

# we Combine "Very likely" and "Somewhat likely" into "Likely" & Combine "Somewhat unlikely" and "Very unlikely" into "Unlikely"
combined_table <- likelihood_data %>%
  filter(Gender %in% c("Male", "Female")) %>%  # Include only Male and Female
  filter(Response %in% c("Very likely", "Somewhat likely", "Somewhat unlikely", "Very unlikely")) %>%
  mutate(
    Response = case_when(
      Response %in% c("Very likely", "Somewhat likely") ~ "Likely",
      Response %in% c("Somewhat unlikely", "Very unlikely") ~ "Unlikely"
    )
  ) %>%
  group_by(Gender, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Response, values_from = Count, values_fill = 0)


chisq.test(as.matrix(combined_table %>% select(-Gender))) # p= 0.004987 Men statistically more likely to say they'd participate


# Perform chi-square test for Scientific Message
likelihood_data %>%
  filter(Gender %in% c("Male", "Female")) %>%
  filter(Response %in% c("Very likely", "Somewhat likely", "Somewhat unlikely", "Very unlikely")) %>%
  mutate(
    Response = case_when(
      Response %in% c("Very likely", "Somewhat likely") ~ "Likely",
      Response %in% c("Somewhat unlikely", "Very unlikely") ~ "Unlikely"
    )
  ) %>%
  filter(MessageTheme == "Scientific") %>%
  group_by(Gender, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Response, values_from = Count, values_fill = 0) %>%
  select(-Gender) %>%
  as.matrix() %>%
  chisq.test() %>%
  print() # Difference between Male and Female for Scientific Message p-value 0.04196

# Perform chi-square test for Environmental Message
likelihood_data %>%
  filter(Gender %in% c("Male", "Female")) %>%
  filter(Response %in% c("Very likely", "Somewhat likely", "Somewhat unlikely", "Very unlikely")) %>%
  mutate(
    Response = case_when(
      Response %in% c("Very likely", "Somewhat likely") ~ "Likely",
      Response %in% c("Somewhat unlikely", "Very unlikely") ~ "Unlikely"
    )
  ) %>%
  filter(MessageTheme == "Environmental") %>%
  group_by(Gender, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Response, values_from = Count, values_fill = 0) %>%
  select(-Gender) %>%
  as.matrix() %>%
  chisq.test() %>%
  print() #  Difference between Male and Female Environmental Message p-value 0.06197



# Findings: Although there is a statistical difference between males and females in likelihood of participation,
# if split by message type,  difference for scientific is SIGNIFICANT and for Environmental is INSIGNIFICANT


# to take a broader look we complete  a Two- Way Anova
# Convert Response to a numeric score
likelihood_data_numeric <- likelihood_data %>%
  filter(Response %in% c("Very likely", "Somewhat likely", "Somewhat unlikely", "Very unlikely")) %>%
  mutate(ResponseNumeric = case_when(
    Response == "Very likely" ~ 5,
    Response == "Somewhat likely" ~ 4,
    Response == "Neutral" ~ 3,
    Response == "Somewhat unlikely" ~ 2,
    Response == "Very unlikely" ~ 1
  ))

# Two-way ANOVA
anova_model <- aov(ResponseNumeric ~ Gender * MessageTheme, data = likelihood_data_numeric)

# Summary of the ANOVA
summary(anova_model)

 # Results: Men are more likely to participate than women overall, no matter the message type,  seems the type of message 
# doesn’t make a big difference when you look at everyone combined.

# BASED ON BOTH TESTS: it seems  The Scientific message resonates more with men than women, but gender differences in participation
# likelihood are primarily driven by men being generally more likely to participate, not by the specific message type. The Environmental message, on the other hand, seems to work equally well for both genders.




# Age

likelihood_data <- likelihood_data %>%
  filter(Response %in% c("Very likely", "Somewhat likely", "Somewhat unlikely", "Very unlikely")) %>%
  mutate(
    AgeGroup = case_when(
      Age >= 18 & Age <= 29 ~ "18–29",
      Age >= 30 & Age <= 44 ~ "30–44",
      Age >= 45 & Age <= 59 ~ "45–59",
      Age >= 60 ~ "60+"
    )
  )


age_summary <- likelihood_data %>%
  filter(!is.na(AgeGroup)) %>% 
  group_by(AgeGroup, MessageTheme, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(AgeGroup, MessageTheme) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  arrange(AgeGroup, MessageTheme, Response)


# Visualization: Preferences by Age Group

ggplot(
  age_summary,  
  aes(x = Response, y = Percentage, fill = MessageTheme)
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
    values = c("Scientific" = "#377eb8", "Environmental" = "#4daf4a")
  ) +
  labs(
    title = "Participation Likelihood by Age Group and Message Theme",
    x = "Response Category",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  facet_wrap(~ AgeGroup, scales = "free_y")



# Visualization excluding seeing the Neutral and NA answers (their percentages included still)
ggplot(
  age_summary %>%
    filter(Response %in% c("Very likely", "Somewhat likely", "Somewhat unlikely", "Very unlikely")),  
  aes(x = Response, y = Percentage, fill = MessageTheme)
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
    values = c("Scientific" = "#377eb8", "Environmental" = "#4daf4a")
  ) +
  labs(
    title = "Participation Likelihood by Age Group and Message Theme (Excluding Neutral Responses)",
    x = "Response Category",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  facet_wrap(~ AgeGroup, scales = "free_y")



# Visualization of percentages if Neutral and NA answers were exluded

ggplot(
  likelihood_data %>%
    filter(Gender %in% c("Male", "Female")) %>%  
    group_by(Gender, MessageTheme, Response) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(Gender, MessageTheme) %>%
    mutate(Percentage = Count / sum(Count) * 100),
  aes(x = Response, y = Percentage, fill = MessageTheme)
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
    values = c("Scientific" = "#377eb8", "Environmental" = "#4daf4a")
  ) +
  labs(
    title = "Participation Likelihood by Gender (Male and Female) and Message Theme",
    x = "Response Category",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  facet_wrap(~ Gender, scales = "free_y")





# Overall Likelihood of Participation by Age Group
likelihood_data %>%
  filter(!is.na(AgeGroup)) %>%
  mutate(
    Response = case_when(
      Response %in% c("Very likely", "Somewhat likely") ~ "Likely",
      Response %in% c("Somewhat unlikely", "Very unlikely") ~ "Unlikely"
    )
  ) %>%
  group_by(AgeGroup, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  arrange(AgeGroup, Response) %>%
  print()


# Overall Likelihood of Participation by Age Group
likelihood_data %>%
  filter(!is.na(AgeGroup)) %>%
  mutate(
    Response = case_when(
      Response %in% c("Very likely", "Somewhat likely") ~ "Likely",
      Response %in% c("Somewhat unlikely", "Very unlikely") ~ "Unlikely",
      TRUE ~ Response  # Keep Neutral as it is
    )
  ) %>%
  group_by(AgeGroup, MessageTheme, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(AgeGroup, MessageTheme) %>%
  mutate(
    Total = sum(Count),  # Total count for the age group and message theme
    Percentage = (Count / Total) * 100  # Calculate percentage
  ) %>%
  arrange(AgeGroup, MessageTheme, Response) %>%
  print()



likelihood_data %>%
  filter(!is.na(AgeGroup)) %>%
  group_by(AgeGroup, MessageTheme, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  arrange(AgeGroup, MessageTheme, Response) %>%
  print()




likelihood_data %>%
  filter(!is.na(AgeGroup)) %>%  # Remove rows with missing AgeGroup
  mutate(
    Response = case_when(
      Response %in% c("Very likely", "Somewhat likely") ~ "Likely",
      Response %in% c("Somewhat unlikely", "Very unlikely") ~ "Unlikely"
    )
  ) %>%
  group_by(AgeGroup, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Response, values_from = Count, values_fill = 0) %>%  # Fill missing categories with 0
  select(-AgeGroup) %>%  # Exclude AgeGroup before converting to matrix
  as.matrix() %>%
  chisq.test() %>%
  print()
