library(shiny)
library(tidyverse)
library(knitr)
library(forcats)

ui <- fluidPage(
  titlePanel("Homework Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
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
        tabPanel("Submissions by User", plotOutput("user_plot")),
        tabPanel("Submissions by Question", plotOutput("question_plot")),
        tabPanel("Grades", tableOutput("grades"))
      )
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    req(input$file1)
    inFile <- input$file1
    df <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    answer_cols <- grep("_answer$", names(df), value = TRUE)
    df[answer_cols] <- lapply(df[answer_cols], as.character)
    return(df)
  })
  
  output$data <- renderTable({
    data()
  })
  
  analysis <- eventReactive(input$run_analysis, {
    df <- data()
    
    # Separate columns into problems, answers, correctness and points
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
    
    # Aggregate data by user_id and correct
    user_data <- data_long %>%
      group_by(user_id, correct) %>%
      summarise(n = n(), .groups = 'drop')
    
    # Aggregate data by question and correct
    question_data <- data_long %>%
      group_by(question, correct) %>%
      summarise(n = n(), .groups = 'drop')
    
    # Calculate total points earned by each student
    total_points_earned <- data_long %>%
      filter(correct == TRUE) %>%
      group_by(user_id) %>%
      summarise(total_earned = sum(points, na.rm = TRUE))
    
    # Calculate total possible points for the test
    total_points_possible <- data_long %>%
      group_by(user_id) %>%
      summarise(total_possible = sum(points, na.rm = TRUE))
    
    # Join total_points_earned and total_points_possible
    grading <- full_join(total_points_earned, total_points_possible, by = "user_id")
    
    # Calculate percentage of points earned
    grading <- grading %>%
      mutate(percentage = (total_earned / total_possible) * 100)
    
    # Assign letter grade
    grading <- grading %>%
      mutate(letter_grade = case_when(
        percentage >= 93 ~ "A",
        percentage >= 80 ~ "B",
        percentage >= 70 ~ "C",
        percentage >= 60 ~ "D",
        TRUE ~ "F"
      ))
    
    # Sort by percentage, highest to lowest
    grading <- grading %>%
      arrange(desc(percentage))
    
    return(list(user_data = user_data, question_data = question_data, grading = grading))
  })
  
  output$user_plot <- renderPlot({
    user_data <- analysis()$user_data
    # Bar plot of submissions by user, colored by whether they are correct or not
    ggplot(user_data, aes(x = user_id, y = n, fill = correct)) +
      geom_bar(stat = 'identity', position = 'stack') +
      labs(x = "User ID", y = "Count", fill = "Correctness") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$question_plot <- renderPlot({
    question_data <- analysis()$question_data
    # Bar plot of submissions by question, colored by whether they are correct or not
    ggplot(question_data, aes(x = as.factor(question), y = n, fill = correct)) +
      geom_bar(stat = 'identity', position = 'stack') +
      labs(x = "Question", y = "Count", fill = "Correctness") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$grades <- renderTable({
    grading <- analysis()$grading
    grading
  })
}

shinyApp(ui = ui, server = server)
