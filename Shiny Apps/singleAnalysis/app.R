library(shiny)
library(tidyverse)
library(knitr)
library(forcats)

ui <- fluidPage(
  titlePanel("Homework Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('homework_file', 'Choose CSV File',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      fileInput('pin_file', 'Choose CSV File',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      actionButton("run_analysis", "Run Analysis")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("data")),
        tabPanel("Submissions by User", 
                 plotOutput("user_plot"),
                 plotOutput("user_attempt_plot")
        ),
        tabPanel("Submissions by Question", 
                 plotOutput("question_plot"), 
                 plotOutput("question_attempt_plot")
        ),
        tabPanel("Grades", tableOutput("grades"))
      )
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$run_analysis, {
    showNotification("Done!", type = "message")
  })
  
  pin_data <- reactive({
    req(input$pin_file)
    inFile <- input$pin_file
    df_pin <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    df_pin$name <- paste(df_pin$first_name, substr(df_pin$last_name, 1, 1), sep = " ")
    return(df_pin)
  })
  
  data <- reactive({
    req(input$homework_file)
    inFile <- input$homework_file
    df <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    answer_cols <- grep("_answer$", names(df), value = TRUE)
    df[answer_cols] <- lapply(df[answer_cols], as.character)
    
    # Join the data from the homework file and the pin file
    df <- left_join(df, pin_data(), by = c("pin" = "PIN"))
    
    # Drop the 'pin', 'first_name', and 'last_name' columns and keep the 'name' column
    df <- df %>% select(-pin, -first_name, -last_name)
    
    # Move 'name' column to the first position
    df <- df %>% select(name, everything())
    
    return(df)
  })
  
  output$data <- renderTable({
    data()
  })
  
  analysis <- eventReactive(input$run_analysis, {
    df <- data()
    
    # Separate columns into problems, answers, correctness and points
    problems <- df %>%
      select(user_id = name, starts_with("Q") & ends_with("_problem")) %>%
      pivot_longer(-user_id, names_to = "question", values_to = "problem", names_pattern = "(Q\\d+\\.\\d+)_problem")
    
    answers <- df %>%
      select(user_id = name, starts_with("Q") & ends_with("_answer")) %>%
      pivot_longer(-user_id, names_to = "question", values_to = "answer", names_pattern = "(Q\\d+\\.\\d+)_answer")
    
    correctness <- df %>%
      select(user_id = name, starts_with("Q") & ends_with("_correct")) %>%
      pivot_longer(-user_id, names_to = "question", values_to = "correct", names_pattern = "(Q\\d+\\.\\d+)_correct")
    
    points <- df %>%
      select(user_id = name, starts_with("Q") & ends_with("_points")) %>%
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
  
  output$user_plot <- renderPlot({
    user_data <- analysis()$user_data
    
    # Calculate total correct answers per user
    correct_totals <- user_data %>% 
      filter(correct == TRUE) %>%
      group_by(user_id) %>%
      summarise(total_correct = sum(n)) %>%
      arrange(total_correct)
    
    # Reorder user factor levels based on total correct
    user_data$user_id <- factor(user_data$user_id, levels = correct_totals$user_id)
    
    user_data <- user_data %>% 
      arrange(user_id, desc(correct)) %>%
      group_by(user_id) %>%
      mutate(n_cumsum = cumsum(n))
    
    ggplot(user_data, aes(x = user_id, y = n, fill = correct)) +
      geom_bar(stat = 'identity', position = 'stack') +
      geom_text(aes(y = n_cumsum, label = n), color = "white", size = 6, vjust = 1.2) +
      labs(x = "User ID", y = "Count", fill = "Correctness") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$question_plot <- renderPlot({
    question_data <- analysis()$question_data
    
    # Calculate total correct answers per question
    correct_totals <- question_data %>% 
      filter(correct == TRUE) %>%
      group_by(question) %>%
      summarise(total_correct = sum(n)) %>%
      arrange(total_correct)
    
    # Reorder question factor levels based on total correct
    question_data$question <- factor(question_data$question, levels = correct_totals$question)
    
    question_data <- question_data %>% 
      arrange(question, desc(correct)) %>%
      group_by(question) %>%
      mutate(n_cumsum = cumsum(n))
    
    ggplot(question_data, aes(x = as.factor(question), y = n, fill = correct)) +
      geom_bar(stat = 'identity', position = 'stack') +
      geom_text(aes(y = n_cumsum, label = n), color = "white", size = 6, vjust = 1.2) +
      labs(x = "Question", y = "Count", fill = "Correctness") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$user_attempt_plot <- renderPlot({
    user_attempt_data <- analysis()$user_attempt_data
    
    # Sort data by average attempts
    user_attempt_data <- user_attempt_data %>%
      arrange(avg_attempts)
    
    # Reorder factor levels
    user_attempt_data$user_id <- factor(user_attempt_data$user_id, levels = user_attempt_data$user_id)
    
    ggplot(user_attempt_data, aes(x = user_id, y = avg_attempts)) +
      geom_bar(stat = 'identity', fill = 'steelblue') +
      geom_text(aes(label=round(avg_attempts, 1)), vjust=1.5, color="white", size = 6) +
      labs(x = "User ID", y = "Average Attempts") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$question_attempt_plot <- renderPlot({
    question_attempt_data <- analysis()$question_attempt_data
    
    # Sort data by average attempts
    question_attempt_data <- question_attempt_data %>%
      arrange(avg_attempts)
    
    # Reorder factor levels
    question_attempt_data$question <- factor(question_attempt_data$question, levels = question_attempt_data$question)
    
    ggplot(question_attempt_data, aes(x = question, y = avg_attempts)) +
      geom_bar(stat = 'identity', fill = 'steelblue') +
      geom_text(aes(label=round(avg_attempts, 1)), vjust=1.5, color="white", size = 6) +
      labs(x = "Question", y = "Average Attempts") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$grades <- renderTable({
    grading <- analysis()$grading
    grading
  })
}

shinyApp(ui = ui, server = server)