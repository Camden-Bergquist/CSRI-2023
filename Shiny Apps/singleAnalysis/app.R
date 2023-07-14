## Shiny app that performs analysis on a single assignment CSV.

library(shiny)
library(tidyverse)
library(knitr)
library(forcats)


# UI with four tabs:
## Data, to load in the assignment CSV and render the raw data.
## Submissions by user graphs.
## Submissions by question graphs.
## Grades.
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
                 plotOutput("user_attempts_plot")
        ),
        tabPanel("Submissions by Question", 
                 div(style = "padding:15px;", plotOutput("question_plot")),
                 div(style = "padding:15px;", plotOutput("question_attempts_plot")) 
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
    
    # Calculate the maximum point value for the assignment. This is done by adding up the point
    # values for each first attempt of a question. We focus only on the first attempt to account
    # for point-degradation functionality.
    rv$max_score <- rv$data %>%
      filter(attempt_num == 1) %>% # Only include first attempts.
      group_by(question_num) %>%
      summarize(total_points = max(points)) %>% # Get max points per question.
      ungroup() %>%
      summarize(max_score = sum(total_points)) %>% # Get max possible score.
      pull(max_score)
  })
  
  # Observe when the "Run Analysis" button is clicked
  observeEvent(input$run_analysis, {
    
    # Ensure the data and max_score have been loaded
    req(rv$data, rv$max_score)
    
    # Calculate each user's score on the assignment and average attempts
    user_scores <- rv$data %>%
      group_by(user_id) %>%
      summarise(score = sum(if_else(correct == TRUE, points, 0)), # Sum up the points for correct answers.
                avg_attempts = mean(attempt_num)) %>% # Calculate the average attempts per user.
      mutate(norm_score = score / rv$max_score) %>% # Normalize the score.
      arrange(desc(norm_score)) # Arrange in descending order.
    
    # Calculate the average score, max score and average attempts for each question.
    question_scores <- rv$data %>%
      group_by(question_num) %>%
      summarise(avg_score = mean(if_else(correct == TRUE, points, 0)), # Get the average score per question.
                avg_attempts = mean(attempt_num), # Calculate the average attempts per question.
                max_question_score = max(points[attempt_num == 1])) %>% # Get max points per question from first attempts only.
      mutate(norm_avg_score = avg_score / max_question_score) %>% # Normalize the avg_score.
      arrange(norm_avg_score) # Arrange in ascending order.
    
    # Render the data table
    output$data <- renderTable(rv$data)
    
    # Render the user scores plot
    output$user_plot <- renderPlot({
      ggplot(user_scores, aes(x = reorder(user_id, norm_score), y = norm_score, fill = norm_score)) +
        geom_bar(stat = "identity") +
        scale_fill_gradient(low = "deepskyblue1", high = "blue") +
        ylab("Normalized Score") +
        xlab("User ID") +
        theme_minimal() +
        ggtitle("Assignment Scores by User")
    })
    
    # Render the user attempts plot
    output$user_attempts_plot <- renderPlot({
      ggplot(user_scores, aes(x = reorder(user_id, avg_attempts), y = avg_attempts, fill = avg_attempts)) +
        geom_bar(stat = "identity") +
        scale_fill_gradient(low = "deepskyblue1", high = "blue2") +
        ylab("Average Attempts") +
        xlab("User ID") +
        theme_minimal() +
        ggtitle("Average Attempts by User")
    })
    
    # Render the normalized question scores plot
    output$question_plot <- renderPlot({
      ggplot(question_scores, aes(x = reorder(as.factor(question_num), norm_avg_score), y = norm_avg_score, fill = norm_avg_score)) +
        geom_bar(stat = "identity") +
        scale_fill_gradient(low = "deepskyblue1", high = "blue2") +
        ylab("Normalized Average Score") +
        xlab("Question Number") +
        theme_minimal() +
        ggtitle("Normalized Average Scores by Question") +
        theme(
          plot.title = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text = element_text(size = 16)
        )
    })
    
    # Render the question attempts plot
    output$question_attempts_plot <- renderPlot({
      ggplot(question_scores, aes(x = reorder(as.factor(question_num), avg_attempts), y = avg_attempts, fill = avg_attempts)) +
        geom_bar(stat = "identity") +
        scale_fill_gradient(low = "deepskyblue1", high = "blue2") +
        ylab("Average Attempts") +
        xlab("Question Number") +
        theme_minimal() +
        ggtitle("Average Attempts by Question") +
        theme(
          plot.title = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text = element_text(size = 16)
        )
    })
    
    # (I'm sorry) Horrifyingly stacked ifelse statements to assign letter grades to point values.
    output$grades <- renderTable({
      user_scores %>%
        mutate(grade = ifelse(norm_score >= 0.93, "A", 
                              ifelse(norm_score >= 0.9, "A-", 
                                     ifelse(norm_score >= 0.87, "B+", 
                                            ifelse(norm_score >= 0.84, "B", 
                                                   ifelse(norm_score >= 0.8, "B-",
                                                          ifelse(norm_score >= 0.77, "C+",
                                                                 ifelse(norm_score >= 0.74, "C",
                                                                        ifelse(norm_score >= 0.7, "C-",
                                                                               ifelse(norm_score >= 0.67, "D+",
                                                                                      ifelse(norm_score >= 0.64, "D",
                                                                                             ifelse(norm_score >= 0.6, "D-", "F"))))))))))))
      
    })
  })
}

shinyApp(ui = ui, server = server)