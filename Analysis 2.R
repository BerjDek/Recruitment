
# Demographics ------------------------------------------------------------


### Function to Create Bar Charts -------------------------------------------
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
