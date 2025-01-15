#Initital upload

library(tidyverse)
library(knitr)
library(kableExtra)

data <- read.csv("results_all_clean.csv")

summary(data)


data <- data %>%
  mutate(across(c(startlanguage, startdate, datestamp, Consent, Gender, HighestEducation, 
                  EmploymentStatus, OccupationCategory, Neighborhood, BuildingType, 
                  Bundesland, LikelihoodParticipScience, LikelihoodRecommScience, FeelingScience, 
                  LikelihoodParticipProtect, LikelihoodRecommProtect, FeelingProtect, 
                  MorePersuasive, HeardCS, DescribeCS_en, ParticipCS, UsefulCS, Group  ), 
                as.factor)) %>%
  mutate(Age_Group = case_when(
    Age >= 18 & Age <= 25 ~ "18-25",
    Age >= 26 & Age <= 44 ~ "26-44",
    Age >= 45 & Age <= 60 ~ "45-60",
    Age > 60 ~ "60+"
  ))


data <- data %>%
  mutate(across(c(LikelihoodParticipScience, LikelihoodRecommScience, FeelingScience, 
                  LikelihoodParticipProtect, LikelihoodRecommProtect, FeelingProtect, 
                  MorePersuasive, HeardCS, DescribeCS_en, ParticipCS, UsefulCS, 
                  OccupationCategory, Neighborhood, BuildingType, 
                  HighestEducation, EmploymentStatus,  FeelingProtect, MorePersuasive,HeardCS ), 
                ~ na_if(., ""))) %>%
  mutate(Group = recode(Group, `1` = "SciMsgFirst", `2` = "EnvMsgFirst"))


data_reduced <- data %>%
  select(-submitdate, -seed, -m, -startlanguage, -Consent,  
         -HighestEducation_comment_de, -HighestEducation_comment_en, -HighestEducation2, 
         -EmploymentStatus_comment_de, -EmploymentStatus_comment_en, -EmploymentStatus2, 
         -OccupationCategory_comment_de, -OccupationCategory_comment_en, -OccupationCategory2, 
         -Neighborhood_comment_de, -Neighborhood_comment_en, -Neighborhood2, 
         -BuildingType_comment_de, -BuildingType_comment_en, -BuildingType2, 
         -FeelingScience_comment_de, -DescribeCS_de,  
          -FeelingProtect_comment_de, -MotivatorsOther_de,
          -BarriersOther_de)



#Name Changes

data_reduced <- data_reduced %>%
  rename(
    SurveyCompletionPage = lastpage,
    SurveyStartTime = startdate,
    SurveyEndTime = datestamp,
    Education_Level = HighestEducation,
    Employment_Status = EmploymentStatus,
    Occupation_Type = OccupationCategory,
    Neighborhood_Type = Neighborhood,
    Participation_Likelihood_SciMsg = LikelihoodParticipScience,
    Recommend_Likelihood_SciMsg = LikelihoodRecommScience,
    Feelings_SciMsg = FeelingScience,
    FeelingsComment_SciMsg = FeelingScience_comment_en,
    Participation_Frequency_SciMsg = FreqParticipScience,
    Participation_Likelihood_EnvMsg = LikelihoodParticipProtect,
    Recommend_Likelihood_EnvMsg = LikelihoodRecommProtect,
    Feelings_EnvMsg = FeelingProtect,
    FeelingsComment_EnvMsg = FeelingProtect_comment_en,
    Participation_Frequency_EnvMsg = FreqParticipProtect,
    More_Persuasive_Msg = MorePersuasive,
    Awareness_Of_CitizenScience = HeardCS,
    Understanding_Of_CitizenScience = DescribeCS_en,
    Participated_In_CitizenScience = ParticipCS,
    CitizenScience_Usefulness = UsefulCS,
    Motivation_TopicInterest = M_TopicInteresting,
    Motivation_NewExperience = M_New,
    Motivation_Enjoyable = M_Enjoyable,
    Motivation_ShareAchievements = M_ShareWithOthers,
    Motivation_FriendsFamily = M_FriendsFamily,
    Motivation_ImproveCommunity = M_ImproveCommunity,
    Motivation_ProtectEnvironment = M_ProtectEnviro,
    Motivation_MeetVolunteers = M_MeetVolunteers,
    Motivation_ScientificResearch = M_ScientificResearch,
    Motivation_TeachOthers = M_TeachOthers,
    Motivation_ReduceRisks = M_ReduceRisks,
    Barrier_Lack_Of_Time = B_Time,
    Barrier_Unaware_Of_Projects = B_UnawareProjects,
    Barrier_Not_Interested = B_NotInterested,
    Barrier_Insufficient_Resources = B_Resources,
    Barrier_No_Aquaintances = B_KnowVolunteers,
    Barrier_DataSharing_Concerns = B_DataSharing,
    Barrier_No_Relevant_Data_To_Collect = B_NoData,
    Barrier_Doesnt_Volunteer = B_Volunteering,
    Barrier_CS_Not_Credible = B_NotCredible
  )

data_reduced <- data_reduced %>%
  mutate(across(c(Gender, Education_Level, Neighborhood_Type, Participation_Likelihood_SciMsg,Recommend_Likelihood_SciMsg,
                  Feelings_SciMsg, BuildingType, Participation_Likelihood_EnvMsg,Recommend_Likelihood_EnvMsg, Feelings_EnvMsg,
                  More_Persuasive_Msg, Awareness_Of_CitizenScience,Participated_In_CitizenScience, CitizenScience_Usefulness), ~ na_if(., "")))



summary(data_reduced)

write.csv(data_reduced, "data_reduced.csv", row.names = FALSE)


#General Results on which message makes people more likely to

#likelihood to participate
ggplot(data_reduced %>%
         pivot_longer(cols = c("Participation_Likelihood_SciMsg", "Participation_Likelihood_EnvMsg"),
                      names_to = "Theme",
                      values_to = "Response") %>%
         mutate(Theme = recode(Theme,
                               "Participation_Likelihood_SciMsg" = "Science",
                               "Participation_Likelihood_EnvMsg" = "Protect"))) +
  geom_bar(aes(x = factor(Response, levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely", "NA")), 
               fill = Theme),
           position = position_dodge(width = 0.8), 
           width = 0.6) +
  scale_fill_manual(name = "Theme", values = c("Science" = "skyblue", "Protect" = "purple")) +
  labs(title = "Comparison of Likelihood to Participate: Science vs Protection",
       x = "Response Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#mostly equal


#recommending initiative
ggplot(data_reduced %>%
         pivot_longer(cols = c("Recommend_Likelihood_SciMsg", "Recommend_Likelihood_EnvMsg"),
                      names_to = "Theme",
                      values_to = "Response") %>%
         mutate(Theme = recode(Theme,
                               "Recommend_Likelihood_SciMsg" = "Science",
                               "Recommend_Likelihood_EnvMsg" = "Protect"))) +
  geom_bar(aes(x = factor(Response, levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely", "NA")), 
               fill = Theme),
           position = position_dodge(width = 0.8), 
           width = 0.6) +
  scale_fill_manual(name = "Theme", values = c("Science" = "skyblue", "Protect" = "purple")) +
  labs(title = "Comparison of Reccomend to Participate: Science vs Protection",
       x = "Response Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#protect more likeley to be recommended



# feeling
ggplot(data_reduced %>%
         pivot_longer(cols = c("Feelings_SciMsg", "Feelings_EnvMsg"),
                      names_to = "Theme",
                      values_to = "Response") %>%
         mutate(Theme = recode(Theme,
                               "Feelings_SciMsg" = "Science",
                               "Feelings_EnvMsg" = "Protect"))) +
  geom_bar(aes(x = factor(Response, levels = c("Inspired", "Indifferent", "Obligated", "Other (please specify)", "NA")), 
               fill = Theme),
           position = position_dodge(width = 0.8), 
           width = 0.6) +
  scale_fill_manual(name = "Theme", values = c("Science" = "skyblue", "Protect" = "purple")) +
  labs(title = "Comparison of Feeling: Science vs Protection",
       x = "Response Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#The messages made the subjects feel in almost exactly the same way



#Comparison of Persuasiveness
ggplot(data_reduced) +
  geom_bar(aes(x = factor(More_Persuasive_Msg, levels = c("Protection", "Scientific", "Neither of the messages was persuasive", "The messages were equally persuasive", "NA")), 
               fill = "More_Persuasive_Msg"),
           color = "black", width = 0.6) +
  scale_fill_manual(name = "More_Persuasive_Msg", values = c("More_Persuasive_Msg" = "skyblue")) +
  labs(title = "Comparison of Persuasiveness",
       x = "Persuasiveness Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#scientific was a bit more convincing to the general but generally the answer is that both were equally persuasive.





### Which message was seen first


#likelihood to participate
ggplot(data_reduced %>%
         filter(!is.na(Group)) %>%
         pivot_longer(cols = c("Participation_Likelihood_SciMsg", "Participation_Likelihood_EnvMsg"),
                      names_to = "Theme",
                      values_to = "Response") %>%
         mutate(Theme = recode(Theme,
                               "Participation_Likelihood_SciMsg" = "Science",
                               "Participation_Likelihood_EnvMsg" = "Protect"))) +
  geom_bar(aes(x = factor(Response, levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely", "NA")), 
               fill = Theme),
           position = position_dodge(width = 0.8), 
           width = 0.6) +
  scale_fill_manual(name = "Theme", values = c("Science" = "skyblue", "Protect" = "purple")) +
  labs(title = "Comparison of Likelihood to Participate: Science vs Protection",
       x = "Response Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Group, scales = "free_y")
# still mostly equal regardless of which message presented first ( the message coming second  a bit more convincing)


#recommending initiative
ggplot(data_reduced %>%
         filter(!is.na(Group)) %>%
         pivot_longer(cols = c("Recommend_Likelihood_SciMsg", "Recommend_Likelihood_EnvMsg"),
                      names_to = "Theme",
                      values_to = "Response") %>%
         mutate(Theme = recode(Theme,
                               "Recommend_Likelihood_SciMsg" = "Science",
                               "Recommend_Likelihood_EnvMsg" = "Protect"))) +
  geom_bar(aes(x = factor(Response, levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely", "NA")), 
               fill = Theme),
           position = position_dodge(width = 0.8), 
           width = 0.6) +
  scale_fill_manual(name = "Theme", values = c("Science" = "skyblue", "Protect" = "purple")) +
  labs(title = "Comparison of Recommending to Participate: Science vs Protection",
       x = "Response Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Group, scales = "free_y")

# when science presented first protect message more likely to be recommended, very likely percentage much more and no more very unlikely (interesting)
#when protection message  was seen first,  very likely to recommend becomes a bit more skewed towards science


# feeling
ggplot(data_reduced %>%
         filter(!is.na(Group)) %>%
         pivot_longer(cols = c("Feelings_SciMsg", "Feelings_EnvMsg"),
                      names_to = "Theme",
                      values_to = "Response") %>%
         mutate(Theme = recode(Theme,
                               "Feelings_SciMsg" = "Science",
                               "Feelings_EnvMsg" = "Protect"))) +
  geom_bar(aes(x = factor(Response, levels = c("Inspired", "Indifferent", "Obligated", "Other (please specify)", "NA")), 
               fill = Theme),
           position = position_dodge(width = 0.8), 
           width = 0.6) +
  scale_fill_manual(name = "Theme", values = c("Science" = "skyblue", "Protect" = "purple")) +
  labs(title = "Comparison of Feeling: Science vs Protection",
       x = "Response Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Group, scales = "free_y")
#slightly more inspired by protect when presented first
#slightly more inspired by science when presented first


#Comparison of Persuasiveness
ggplot(data_reduced %>% 
         filter(!is.na(Group))) +
  geom_bar(aes(x = factor(More_Persuasive_Msg, levels = c("Protection", "Scientific", "Neither of the messages was persuasive", "The messages were equally persuasive", "NA")), 
               fill = "More_Persuasive_Msg"),
           color = "black", width = 0.6) +
  scale_fill_manual(name = "More_Persuasive_Msg", values = c("More_Persuasive_Msg" = "skyblue")) +
  labs(title = "Comparison of Persuasiveness",
       x = "Persuasiveness Category", y = "Count") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Group, scales = "free_y")

# again mostly equal, protection less persuasive than scientific
#Protection seems to be slightly more persuasive than before (still less than scientific) when scientific message is presented first 
#(interesting in that people seem to think scientific is more persuasive , but more likely to recommend others to participate when subject is env protection)




### If they participated in CS before or not

#likelihood to participate
ggplot(data_reduced %>%
         filter(!is.na(Participated_In_CitizenScience)) %>%
         pivot_longer(cols = c("Participation_Likelihood_SciMsg", "Participation_Likelihood_EnvMsg"),
                      names_to = "Theme",
                      values_to = "Response") %>%
         mutate(Theme = recode(Theme,
                               "Participation_Likelihood_SciMsg" = "Science",
                               "Participation_Likelihood_EnvMsg" = "Protect"))) +
  geom_bar(aes(x = factor(Response, levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely", "NA")), 
               fill = Theme),
           position = position_dodge(width = 0.8), 
           width = 0.6) +
  scale_fill_manual(name = "Theme", values = c("Science" = "skyblue", "Protect" = "purple")) +
  labs(title = "Comparison of Likelihood to Participate: Science vs Protection",
       x = "Response Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Participated_In_CitizenScience, scales = "free_y")

# on average way more likely to participate, science slightly more attractive


#recommending initiative
ggplot(data_reduced %>%
         filter(!is.na(Participated_In_CitizenScience)) %>%
         pivot_longer(cols = c("Recommend_Likelihood_SciMsg", "Recommend_Likelihood_EnvMsg"),
                      names_to = "Theme",
                      values_to = "Response") %>%
         mutate(Theme = recode(Theme,
                               "Recommend_Likelihood_SciMsg" = "Science",
                               "Recommend_Likelihood_EnvMsg" = "Protect"))) +
  geom_bar(aes(x = factor(Response, levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely", "NA")), 
               fill = Theme),
           position = position_dodge(width = 0.8), 
           width = 0.6) +
  scale_fill_manual(name = "Theme", values = c("Science" = "skyblue", "Protect" = "purple")) +
  labs(title = "Comparison of Recommending to Participate: Science vs Protection",
       x = "Response Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Participated_In_CitizenScience, scales = "free_y")

#the unlikeliness of recommending drops, ones very likely to recommend would do so more after hearing about protection
# it might be that people who join for science might be more likely to think that it would be easier to convince other to join by speaking about environmental benefits


# feeling
ggplot(data_reduced %>%
         filter(!is.na(Participated_In_CitizenScience)) %>%
         pivot_longer(cols = c("Feelings_SciMsg", "Feelings_EnvMsg"),
                      names_to = "Theme",
                      values_to = "Response") %>%
         mutate(Theme = recode(Theme,
                               "Feelings_SciMsg" = "Science",
                               "Feelings_EnvMsg" = "Protect"))) +
  geom_bar(aes(x = factor(Response, levels = c("Inspired", "Indifferent", "Obligated", "Other (please specify)", "NA")), 
               fill = Theme),
           position = position_dodge(width = 0.8), 
           width = 0.6) +
  scale_fill_manual(name = "Theme", values = c("Science" = "skyblue", "Protect" = "purple")) +
  labs(title = "Comparison of Feeling: Science vs Protection",
       x = "Response Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Participated_In_CitizenScience, scales = "free_y")
#both versions seem to be equally effecting how those who already in cs feel 



#Comparison of Persuasiveness
ggplot(data_reduced %>%
         filter(!is.na(Participated_In_CitizenScience))) +
  geom_bar(aes(x = factor(More_Persuasive_Msg, levels = c("Protection", "Scientific", "Neither of the messages was persuasive", "The messages were equally persuasive", "NA")), 
               fill = "More_Persuasive_Msg"),
           color = "black", width = 0.6) +
  scale_fill_manual(name = "More_Persuasive_Msg", values = c("More_Persuasive_Msg" = "skyblue")) +
  labs(title = "Comparison of Persuasiveness",
       x = "Persuasiveness Category", y = "Count") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Participated_In_CitizenScience, scales = "free_y")

#  protection less persuasive than scientific for people who have not participated in CS, Number of people saying not sure and yes are way less 
# protection more convincing for not sure and scientific by a small margin for those that have participated in CS





### Based on if they think CS is useful or not

ggplot(data_reduced %>%
         filter(!is.na(CitizenScience_Usefulness)) %>%
         pivot_longer(cols = c("Participation_Likelihood_SciMsg", "Participation_Likelihood_EnvMsg"),
                      names_to = "Theme",
                      values_to = "Response") %>%
         mutate(Theme = recode(Theme,
                               "Participation_Likelihood_SciMsg" = "Science",
                               "Participation_Likelihood_EnvMsg" = "Protect"))) +
  geom_bar(aes(x = factor(Response, levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely", "NA")), 
               fill = Theme),
           position = position_dodge(width = 0.8), 
           width = 0.6) +
  scale_fill_manual(name = "Theme", values = c("Science" = "skyblue", "Protect" = "purple")) +
  labs(title = "Comparison of Likelihood to Participate by CitizenScience_Usefulness Choices",
       x = "Response Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ CitizenScience_Usefulness, scales = "free_y") 
# those who think CS is not useful (lower numbers) are more likely to participate by protect message, 


#recommending initiative
ggplot(data_reduced %>%
         filter(!is.na(CitizenScience_Usefulness)) %>%
         pivot_longer(cols = c("Recommend_Likelihood_SciMsg", "Recommend_Likelihood_EnvMsg"),
                      names_to = "Theme",
                      values_to = "Response") %>%
         mutate(Theme = recode(Theme,
                               "Recommend_Likelihood_SciMsg" = "Science",
                               "Recommend_Likelihood_EnvMsg" = "Protect"))) +
  geom_bar(aes(x = factor(Response, levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely", "NA")), 
               fill = Theme),
           position = position_dodge(width = 0.8), 
           width = 0.6) +
  scale_fill_manual(name = "Theme", values = c("Science" = "skyblue", "Protect" = "purple")) +
  labs(title = "Comparison of Recommending to Participate: Science vs Protection",
       x = "Response Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ CitizenScience_Usefulness, scales = "free_y")

#Those who think CS is not useful would recommend after Protection, so would those not sure and those who think its Useful,
#it seems environmental protection message more recommendable in general.



# feeling
ggplot(data_reduced %>%
         filter(!is.na(CitizenScience_Usefulness)) %>%
         pivot_longer(cols = c("Feelings_SciMsg", "Feelings_EnvMsg"),
                      names_to = "Theme",
                      values_to = "Response") %>%
         mutate(Theme = recode(Theme,
                               "Feelings_SciMsg" = "Science",
                               "Feelings_EnvMsg" = "Protect"))) +
  geom_bar(aes(x = factor(Response, levels = c("Inspired", "Indifferent", "Obligated", "Other (please specify)", "NA")), 
               fill = Theme),
           position = position_dodge(width = 0.8), 
           width = 0.6) +
  scale_fill_manual(name = "Theme", values = c("Science" = "skyblue", "Protect" = "purple")) +
  labs(title = "Comparison of Feeling: Science vs Protection",
       x = "Response Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ CitizenScience_Usefulness, scales = "free_y")
#both versions seem to be equally effecting how those who already in cs feel, nothing really major regarding how messages make people feel



#Comparison of Persuasiveness
ggplot(data_reduced %>%
         filter(!is.na(CitizenScience_Usefulness))) +
  geom_bar(aes(x = factor(More_Persuasive_Msg, levels = c("Protection", "Scientific", "Neither of the messages was persuasive", "The messages were equally persuasive", "NA")), 
               fill = "More_Persuasive_Msg"),
           color = "black", width = 0.6) +
  scale_fill_manual(name = "More_Persuasive_Msg", values = c("More_Persuasive_Msg" = "skyblue")) +
  labs(title = "Comparison of Persuasiveness",
       x = "Persuasiveness Category", y = "Count") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ CitizenScience_Usefulness, scales = "free_y")

# nothing really new, people think Scientific message more persuasive.



summary(data_reduced)



### Based on Education

# Likelihood of participating after seeing each message

ggplot(data_reduced %>%
         filter(Education_Level %in% c("A-levels", "Secondary school", "University (University/technical school)")) %>%
         pivot_longer(cols = c("Participation_Likelihood_SciMsg", "Participation_Likelihood_EnvMsg"),
                      names_to = "Theme",
                      values_to = "Response") %>%
         mutate(Theme = recode(Theme,
                               "Participation_Likelihood_SciMsg" = "Science",
                               "Participation_Likelihood_EnvMsg" = "Protect"))) +
  geom_bar(aes(x = factor(Response, levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely", "NA")), 
               fill = Theme),
           position = position_dodge(width = 0.8), 
           width = 0.6) +
  scale_fill_manual(name = "Theme", values = c("Science" = "skyblue", "Protect" = "purple")) +
  labs(title = "Comparison of Likelihood to Participate by Education_Level Choices",
       x = "Response Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Education_Level, scales = "free_y") 



#recommending initiative
ggplot(data_reduced %>%
         filter(Education_Level %in% c("A-levels", "Secondary school", "University (University/technical school)")) %>%
         pivot_longer(cols = c("Recommend_Likelihood_SciMsg", "Recommend_Likelihood_EnvMsg"),
                      names_to = "Theme",
                      values_to = "Response") %>%
         mutate(Theme = recode(Theme,
                               "Recommend_Likelihood_SciMsg" = "Science",
                               "Recommend_Likelihood_EnvMsg" = "Protect"))) +
  geom_bar(aes(x = factor(Response, levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely", "NA")), 
               fill = Theme),
           position = position_dodge(width = 0.8), 
           width = 0.6) +
  scale_fill_manual(name = "Theme", values = c("Science" = "skyblue", "Protect" = "purple")) +
  labs(title = "Comparison of Recommending to Participate: Science vs Protection",
       x = "Response Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Education_Level, scales = "free_y")



# feeling
ggplot(data_reduced %>%
         filter(Education_Level %in% c("A-levels", "Secondary school", "University (University/technical school)")) %>%
         pivot_longer(cols = c("Feelings_SciMsg", "Feelings_EnvMsg"),
                      names_to = "Theme",
                      values_to = "Response") %>%
         mutate(Theme = recode(Theme,
                               "Feelings_SciMsg" = "Science",
                               "Feelings_EnvMsg" = "Protect"))) +
  geom_bar(aes(x = factor(Response, levels = c("Inspired", "Indifferent", "Obligated", "Other (please specify)", "NA")), 
               fill = Theme),
           position = position_dodge(width = 0.8), 
           width = 0.6) +
  scale_fill_manual(name = "Theme", values = c("Science" = "skyblue", "Protect" = "purple")) +
  labs(title = "Comparison of Feeling: Science vs Protection",
       x = "Response Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Education_Level, scales = "free_y")
#It seems the higher education the more inspired by env message and vice versa



#Comparison of Persuasiveness
ggplot(data_reduced %>%
         filter(Education_Level %in% c("A-levels", "Secondary school", "University (University/technical school)"))) +
  geom_bar(aes(x = factor(More_Persuasive_Msg, levels = c("Protection", "Scientific", "Neither of the messages was persuasive", "The messages were equally persuasive", "NA")), 
               fill = "More_Persuasive_Msg"),
           color = "black", width = 0.6) +
  scale_fill_manual(name = "More_Persuasive_Msg", values = c("More_Persuasive_Msg" = "skyblue")) +
  labs(title = "Comparison of Persuasiveness",
       x = "Persuasiveness Category", y = "Count") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Education_Level, scales = "free_y")
#university educated become more persuaded by env messaging

