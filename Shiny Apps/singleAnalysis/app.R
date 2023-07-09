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
      actionButton("run_analysis", "Run Analysis")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("data")),
        tabPanel("Submissions by User", 
                 plotOutput("user_plot"),
                 plotOutput("user_attempts_plot") # New plot for average attempts by user
        ),
        tabPanel("Submissions by Question", 
                 plotOutput("question_plot"),
                 plotOutput("question_attempts_plot") # New plot for average attempts by question
        ),
        tabPanel("Grades", tableOutput("grades"))
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Initialize reactive values
  rv <- reactiveValues()
  
  # Observe the uploaded CSV file
  observeEvent(input$homework_file, {
    rv$data <- read_csv(input$homework_file$datapath)
    
    # Calculate the maximum point value for the assignment
    rv$max_score <- rv$data %>%
      filter(attempt_num == 1) %>% # Only include first attempts
      group_by(question_num) %>%
      summarize(total_points = max(points)) %>% # get max points per question
      ungroup() %>%
      summarize(max_score = sum(total_points)) %>% # get max possible score
      pull(max_score)
  })
  
  # Observe when the "Run Analysis" button is clicked
  observeEvent(input$run_analysis, {
    
    # Ensure the data and max_score have been loaded
    req(rv$data, rv$max_score)
    
    # Calculate each user's score on the assignment and average attempts
    user_scores <- rv$data %>%
      group_by(user_id) %>%
      summarise(score = sum(if_else(correct == TRUE, points, 0)), # Sum up the points for correct answers
                avg_attempts = mean(attempt_num)) %>% # Calculate the average attempts per user
      mutate(norm_score = score / rv$max_score) %>% # Normalize the score
      arrange(desc(norm_score)) # Arrange in descending order
    
    # Calculate the average score and average attempts for each question
    question_scores <- rv$data %>%
      group_by(question_num) %>%
      summarise(avg_score = mean(if_else(correct == TRUE, points, 0)), # Get the average score per question
                avg_attempts = mean(attempt_num)) %>% # Calculate the average attempts per question
      arrange(avg_score) # Arrange in ascending order
    
    # Render the data table
    output$data <- renderTable(rv$data)
    
    # Render the user scores plot
    output$user_plot <- renderPlot({
      ggplot(user_scores, aes(x = reorder(user_id, norm_score), y = norm_score, fill = user_id)) +
        geom_bar(stat = "identity") +
        ylab("Normalized Score") +
        xlab("User ID") +
        theme_minimal() +
        ggtitle("Assignment Scores by User")
    })
    
    # Render the user attempts plot
    output$user_attempts_plot <- renderPlot({
      ggplot(user_scores, aes(x = reorder(user_id, avg_attempts), y = avg_attempts, fill = user_id)) +
        geom_bar(stat = "identity") +
        ylab("Average Attempts") +
        xlab("User ID") +
        theme_minimal() +
        ggtitle("Average Attempts by User")
    })
    
    # Render the question scores plot
    output$question_plot <- renderPlot({
      ggplot(question_scores, aes(x = reorder(as.factor(question_num), avg_score), y = avg_score, fill = as.factor(question_num))) +
        geom_bar(stat = "identity") +
        ylab("Average Score") +
        xlab("Question Number") +
        theme_minimal() +
        ggtitle("Average Scores by Question")
    })
    
    # Render the question attempts plot
    output$question_attempts_plot <- renderPlot({
      ggplot(question_scores, aes(x = reorder(as.factor(question_num), avg_attempts), y = avg_attempts, fill = as.factor(question_num))) +
        geom_bar(stat = "identity") +
        ylab("Average Attempts") +
        xlab("Question Number") +
        theme_minimal() +
        ggtitle("Average Attempts by Question")
    })
    
    # Render the grades table
    output$grades <- renderTable({
      user_scores %>%
        mutate(grade = ifelse(norm_score >= 0.9, "A", 
                              ifelse(norm_score >= 0.8, "B", 
                                     ifelse(norm_score >= 0.7, "C", 
                                            ifelse(norm_score >= 0.6, "D", "F")))))
    })
  })
}

shinyApp(ui = ui, server = server)