library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Homework Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("run_analysis", "Run Analysis")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("data")),
        tabPanel("Grades by Assignment", 
                 plotOutput("user_plot"),
                 plotOutput("user_attempt_plot")
        ),
        tabPanel("Assignments by Student", 
                 plotOutput("question_plot"), 
                 plotOutput("question_attempt_plot")
        ),
        tabPanel("Grades", tableOutput("grades"))
      )
    )
  )
)

server <- function(input, output) {
  data <- eventReactive(input$run_analysis, {
    homework_dir <- "./Assignments"
    
    filePaths <- list.files(path = homework_dir, full.names = TRUE, pattern = "*.csv")
    
    if(length(filePaths) == 0) {
      return(NULL)
    }
    
    data <- filePaths %>%
      set_names(nm = basename(.)) %>%
      map_df(~read_csv(.x, guess_max = 1e6), .id = 'assignment') %>%
      mutate(assignment = str_remove(assignment, ".csv")) %>%
      type_convert() # convert column types
    
    # Convert all answer columns to the same type
    names <- names(data)
    answer_cols <- grep("^Q.*_answer$", names, value = TRUE)
    data[answer_cols] <- lapply(data[answer_cols], function(col) as.character(col))
    data
  })
  
  analysis <- eventReactive(input$run_analysis, {
    df <- data()
    
    problems <- df %>%
      select(user_id = pin, starts_with("Q") & ends_with("_problem")) %>%
      pivot_longer(-user_id, names_to = "question", values_to = "problem", names_pattern = "(Q\\d+\\.\\d+)_problem")
    
    answers <- df %>%
      select(user_id = pin, starts_with("Q") & ends_with("_answer")) %>%
      pivot_longer(-user_id, names_to = "question", values_to = "answer", names_pattern = "(Q\\d+\\.\\d+)_answer")
    
    correctness <- df %>%
      select(user_id = pin, starts_with("Q") & ends_with("_correct")) %>%
      pivot_longer(-user_id, names_to = "question", values_to = "correct", names_pattern = "(Q\\d+\\.\\d+)_correct")
    
    points <- df %>%
      select(user_id = pin, starts_with("Q") & ends_with("_points")) %>%
      pivot_longer(-user_id, names_to = "question", values_to = "points", names_pattern = "(Q\\d+\\.\\d+)_points")
    
    # Combine reshaped data
    data_long <- full_join(problems, answers, by = c("user_id", "question")) %>%
      full_join(correctness, by = c("user_id", "question")) %>%
      full_join(points, by = c("user_id", "question"))
    
    # Convert correct column to logical and points to integer
    data_long$correct <- as.logical(data_long$correct)
    data_long$points <- as.numeric(data_long$points)
    
    # Filter out first correct attempts
    data_long <- data_long %>%
      group_by(user_id, str_extract(question, "^Q\\d+")) %>%
      arrange(as.numeric(str_extract(question, "\\d+$"))) %>%
      filter(if (any(correct == TRUE)) correct == TRUE else TRUE) %>%
      slice_min(order_by = as.numeric(str_extract(question, "\\d+$")))
    
    # Aggregate data by user_id and correct
    user_data <- data_long %>%
      group_by(user_id, correct) %>%
      summarise(n = n(), .groups = 'drop')
    
    # Aggregate data by question and correct
    question_data <- data_long %>%
      group_by(question = str_extract(question, "^Q\\d+"), correct) %>%
      summarise(n = n(), .groups = 'drop')
    
    # Calculate total points earned by each student
    total_points_earned <- data_long %>%
      filter(correct == TRUE) %>%
      group_by(user_id) %>%
      summarise(total_earned = sum(points, na.rm = TRUE))
    
    # Calculate total possible points for the assignment per student and find the maximum
    total_points_possible <- data_long %>%
      group_by(user_id) %>%
      summarise(total_possible = sum(points, na.rm = TRUE)) %>%
      summarise(max_total_possible = max(total_possible, na.rm = TRUE))
    
    # Assign the maximum total_possible to each student
    total_points_possible_per_student <- data.frame(user_id = unique(data_long$user_id), total_possible = total_points_possible$max_total_possible)
    
    # Join total_points_earned and total_points_possible_per_student
    grading <- full_join(total_points_earned, total_points_possible_per_student, by = "user_id")
    
    # Calculate percentage of points earned
    grading <- grading %>%
      mutate(percentage = (total_earned / total_possible) * 100)
    
    grading <- grading %>%
      mutate(letter_grade = case_when(
        percentage >= 93 ~ "A",
        percentage >= 90 ~ "A-",
        percentage >= 87 ~ "B+",
        percentage >= 83 ~ "B",
        percentage >= 80 ~ "B-",
        percentage >= 77 ~ "C+",
        percentage >= 73 ~ "C",
        percentage >= 70 ~ "C-",
        percentage >= 67 ~ "D+",
        percentage >= 63 ~ "D",
        percentage >= 60 ~ "D-",
        TRUE ~ "F"
      ))
    
    # Sort by percentage, highest to lowest
    grading <- grading %>%
      arrange(desc(percentage))
    
    # Calculate the maximum attempt number for each user and question, then average by question
    question_attempt_data <- data_long %>%
      mutate(attempt_number = as.numeric(str_extract(question, "\\d+$"))) %>%
      group_by(user_id, question = str_extract(question, "^Q\\d+")) %>%
      summarise(max_attempt = max(attempt_number)) %>%
      group_by(question) %>%
      summarise(avg_attempts = mean(max_attempt))
    
    # Calculate the maximum attempt number for each user and question, then average by user
    user_attempt_data <- data_long %>%
      mutate(attempt_number = as.numeric(str_extract(question, "\\d+$"))) %>%
      group_by(user_id, question = str_extract(question, "^Q\\d+")) %>%
      summarise(max_attempt = max(attempt_number)) %>%
      group_by(user_id) %>%
      summarise(avg_attempts = mean(max_attempt))
    
    return(list(user_data = user_data, question_data = question_data, grading = grading, question_attempt_data = question_attempt_data, user_attempt_data = user_attempt_data))
  })
  
  output$data <- renderTable({
    req(data())
    data()
  })
  
  output$grades <- renderTable({
    req(analysis())
    analysis()$grading
  })
  
  # To add: plot rendering.
}

shinyApp(ui = ui, server = server)
