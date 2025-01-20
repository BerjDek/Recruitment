
  
  
  comparison_plot <- function(data, col1, col2, labels, title, response_levels = NULL, include_na = FALSE, show_totals = FALSE, combine_levels = NULL, facet_by = NULL, facet_levels = NULL, legend_position = "top") { 
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
    
    # Helper function to calculate totals info
    calculate_totals_info <- function(data, response_levels, combine_levels, facet_by = NULL, facet_levels = NULL) {
      if (!is.null(facet_by) && !is.null(facet_levels)) {
        data <- data %>% filter(!!sym(facet_by) %in% facet_levels)
      }
      
      data %>%
        pivot_longer(
          cols = c(col1, col2),
          names_to = "MessageTheme",
          values_to = "Response"
        ) %>%
        group_by(!!sym(facet_by), MessageTheme) %>%
        summarise(
          Total = n(),
          NA_Count = sum(is.na(Response)),
          Fraction = Total - NA_Count - sum(!Response %in% c(response_levels, names(combine_levels)), na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    # Calculate totals
    totals_info <- calculate_totals_info(data, response_levels, combine_levels, facet_by, facet_levels)
    
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
        values = setNames(
          colorRampPalette(c("#377eb8", "#4daf4a", "#984ea3", "#ff7f00"))(length(unique(summary_data$MessageTheme))),
          unique(summary_data$MessageTheme)
        )
      ) +
      labs(
        title = title,
        x = "Response Category",
        y = "Count"
      ) +
      scale_y_continuous(
        breaks = seq(0, max(summary_data$Count) * 1.2, by = max(1, round(max(summary_data$Count) / 10)))
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7),  
        axis.title.x = element_text(size = 9), 
        axis.title.y = element_text(size = 9), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        legend.position = legend_position
      )
    
    # Add faceting if specified
    if (!is.null(facet_by)) {
      plot <- plot +
        facet_wrap(vars(!!sym(facet_by)), scales = "free_y")
    }
    
    # Add totals, fractions, and NA counts as annotations
    if (show_totals) {
      if (!is.null(facet_by)) {
        plot <- plot +
          geom_text(
            data = totals_info,
            aes(
              x = Inf,  # Place on the far right
              y = Inf,  # Place at the top
              label = paste0("NA = ", NA_Count, "\nTotal: ", Total)
            ),
            hjust = 1.3,  # Slightly inward from the right edge
            vjust = 1.2,  # Slightly inward from the top edge
            size = 2,     # Adjust text size
            inherit.aes = FALSE
          )
      } else {
        plot <- plot +
          geom_text(
            aes(
              x = Inf,  # Place on the far right
              y = Inf,  # Place at the top
              label = paste0("NA = ", NA_Count, "\nTotal: ", Total)
            ),
            hjust = 1.3,  # Slightly inward from the right edge
            vjust = 1.2,  # Slightly inward from the top edge
            size = 2,     # Adjust text size
            inherit.aes = FALSE
          )
      }
    }
    
    return(plot)
  }
  

  
  
  
  
  comparison_plot <- function(data, col1, col2, labels, title, response_levels = NULL, include_na = FALSE, show_totals = FALSE, combine_levels = NULL, facet_by = NULL, facet_levels = NULL, legend_position = "top") { 
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
    
    # Helper function to calculate totals info
    calculate_totals_info <- function(data, response_levels, combine_levels, facet_by = NULL, facet_levels = NULL) {
      if (!is.null(facet_by) && !is.null(facet_levels)) {
        data <- data %>% filter(!!sym(facet_by) %in% facet_levels)
      }
      
      data %>%
        pivot_longer(
          cols = c(col1, col2),
          names_to = "MessageTheme",
          values_to = "Response"
        ) %>%
        group_by(MessageTheme, !!sym(facet_by)) %>%
        summarise(
          Total = n(),
          NA_Count = sum(is.na(Response)),
          Fraction = Total - NA_Count - sum(!Response %in% c(response_levels, names(combine_levels)), na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    # Calculate totals
    if (show_totals) {
      totals_info <- calculate_totals_info(data, response_levels, combine_levels, facet_by, facet_levels)
    }
    
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
        values = setNames(
          colorRampPalette(c("#377eb8", "#4daf4a", "#984ea3", "#ff7f00"))(length(unique(summary_data$MessageTheme))),
          unique(summary_data$MessageTheme)
        )
      ) +
      labs(
        title = title,
        x = "Response Category",
        y = "Count"
      ) +
      scale_y_continuous(
        breaks = seq(0, max(summary_data$Count) * 1.2, by = max(1, round(max(summary_data$Count) / 10)))
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7),  
        axis.title.x = element_text(size = 9), 
        axis.title.y = element_text(size = 9), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        legend.position = legend_position
      )
    
    # Add faceting if specified
    if (!is.null(facet_by)) {
      plot <- plot +
        facet_wrap(vars(!!sym(facet_by)), scales = "free_y")
    }
    
    # Add totals, fractions, and NA counts as annotations
    if (show_totals && !is.null(facet_by)) {
      plot <- plot +
        geom_text(
          data = totals_info,
          aes(
            x = Inf,
            y = Inf,
            label = paste0("NA = ", NA_Count, "\nTotal: ", Total)
          ),
          hjust = 1.3,
          vjust = 1.2,
          size = 2,
          inherit.aes = FALSE
        )
    }
    
    return(plot)
  }
  
  
  
  
  
  
  
  ### General ------------------------------------------------------------
  
  
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
  
  ##  By Education -------------------------------------------------
  
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
  
  
  
  
  
    
  comparison_plot(
    data = data_reduced,
    col1 = "Participation_Likelihood_SciMsg",
    col2 = "Participation_Likelihood_EnvMsg",
    labels = c("Scientific", "Environmental"),
    title = "Participation Likelihood by Message Theme And Education Level",
    response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"), 
    facet_by = "Education_Level",
    facet_levels = c("A-levels", "Primary school", "Secondary school", "University (University/technical school)"),
    include_na = FALSE,
    show_totals = TRUE
  )
  
  comparison_plot(
    data = data_reduced,
    col1 = "Participation_Likelihood_SciMsg",
    col2 = "Participation_Likelihood_EnvMsg",
    labels = c("Scientific", "Environmental"),
    title = "Participation Likelihood by Message Theme And Education Level",
    response_levels = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely", "Very unlikely"),
    include_na = FALSE,
    show_totals = TRUE
  )

summary(data_reduced)  
