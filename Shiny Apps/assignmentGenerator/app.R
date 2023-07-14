## Shiny app that generates learnr questions based on predefined templates.

library(shiny)
library(shinyWidgets)
library(stringr)
library(DT)

ui <- fluidPage(
  # Top title panel.
  titlePanel("Learnr Question Generator"),
  
  # Layout with sidebar and main panel.
  sidebarLayout(
    sidebarPanel(
      # Inputs for Rmd file name, points for question, question query, and question type.
      textInput('fileName', 'Enter Rmd file name', value = "Assignment.Rmd"),
      uiOutput("fileFound"), # Output for whether file is found or not.
      actionButton("newFileButton", "Create New File"), # Button for creating new file.
      numericInput('points', 'Points', value = 1, min = 1),
      textInput('query', 'Question Query'),
      radioButtons('questionType', 'Question type',
                   choices = c('Multiple Choice' = 1, 'Numeric' = 2)),
      
      # Conditional inputs for multiple choice questions.
      conditionalPanel(
        condition = "input.questionType == 1",
        sliderInput('numCorrect', 'Number of correct answers', min = 1, max = 10, value = 1),
        tags$span("Multiple correct answers create a checkbox question.", 
                  style = "font-size: 12px; padding-left: 5px;", 
                  title = "Multiple correct answers create a checkbox question."),
        sliderInput('numIncorrect', 'Number of incorrect answers', min = 1, max = 10, value = 3),
        uiOutput("correctAnswers"), # Output for text inputs for correct answers.
        uiOutput("incorrectAnswers") # Output for text inputs for incorrect answers.
      ),
      
      # Conditional inputs for numeric questions.
      conditionalPanel(
        condition = "input.questionType == 2",
        numericInput('correctAnswer', 'Correct answer', value = 1),
        numericInput('tolerance', 'Tolerance', value = 0.1)
      ),
      
      # Buttons for adding question and writing to file.
      actionButton("addQuestion", "Add Question"),
      actionButton("writeToFile", "Write to File")
    ),
    
    # Main panel with datatable and status text.
    mainPanel(
      DTOutput('table'),
      verbatimTextOutput("statusText")
    )
  )
)

server <- function(input, output, session) {
  
  # Define reactive value to store the selected file path.
  selectedFilePath <- reactiveVal(NULL)
  
  # Observer to check whether file exists and update the UI accordingly.
  observe({
    output$fileFound <- renderUI({
      invalidateLater(1000)  # Checks file status every second.
      if (file.exists(paste0("./Assignments/", input$fileName))) {
        tags$span("\u2714 File found!", 
                  style = "color: green; padding-left: 5px; padding-bottom: 3px;", 
                  title = "File has been found in the directory")
      } else {
        tags$span("\u2718 File not found.", 
                  style = "color: red; padding-left: 5px; padding-bottom: 3px;", 
                  title = "File not found in the directory")
      }
    })
  })
  
  # Create reactiveValues object to store question details.
  questions <- reactiveValues(data = data.frame(
    Type = character(0), Query = character(0), Answers = character(0), Points = numeric(0), Tolerance = numeric(0)
  ))
  
  # Observers to render text inputs for correct and incorrect answers based on slider inputs.
  observe({
    req(input$numCorrect)
    output$correctAnswers <- renderUI({
      lapply(seq_len(input$numCorrect), function(i) {
        textInput(paste0('correctAnswer', i), paste0('Correct answer ', i))
      })
    })
  })
  
  observe({
    req(input$numIncorrect)
    output$incorrectAnswers <- renderUI({
      lapply(seq_len(input$numIncorrect), function(i) {
        textInput(paste0('incorrectAnswer', i), paste0('Incorrect answer ', i))
      })
    })
  })
  
  # Observer to update selected file path when file name input changes.
  observeEvent(input$fileName, {
    selectedFilePath(paste0("./Assignments/", input$fileName))
  })
  
  # Observer for creating a new file when button is clicked. 
  # Writes template for setup block containing required libraries and function calls to file.
  observeEvent(input$newFileButton, {
    rmd_file <- selectedFilePath()
    if (file.exists(rmd_file)) {
      showNotification("File already exists. Please choose a different name.", type = "warning")
    } else {
      dir.create("./Assignments", showWarnings = FALSE)
      fileConn <- file(rmd_file, open = "w")
      setup_chunk <- '---
title: ""
output: learnr::tutorial
runtime: shiny_prerendered
---

```{r setup, include = FALSE}
library(learnr)
library(dplyr)
library(tidyr)
library(purrr)

source("~/CSRI-2023/Scripts/eventRecorderLocal.R")

# REQUIRED: Set the global option to use our event recorder function.
options(tutorial.event_recorder = eventRecorder)

# Set knitr options
knitr::opts_chunk$set(echo = FALSE)
```'
      cat(file=fileConn, setup_chunk, "\n")
      
      close(fileConn)
      showNotification("New file created!", type = "message")
    }
  })
  
  # Observer for adding a question to the list when button is clicked.
  observeEvent(input$addQuestion, {
    
    # 'req' is used to ensure that values are available before proceeding.
    # We need to make sure both 'questionType' and 'query' are specified by the user.
    req(input$questionType, input$query)
    if (input$questionType == 1) {
      req(input$numCorrect, input$numIncorrect)
    } else {
      req(input$correctAnswer, input$tolerance)
    }
    
    # Create a list to store the details of the question.
    # By default, 'Tolerance' is set to NA as it is not applicable for multiple choice questions.
    question <- list(
      Type = input$questionType,
      Query = input$query,
      Points = input$points,
      Tolerance = NA
    )
    
    # Stores specified question data for later use (answers, tolerance, etc).
    if (input$questionType == 1) {
      question$Answers <- paste(c(
        sapply(seq_len(input$numCorrect), function(i) input[[paste0('correctAnswer', i)]]),
        sapply(seq_len(input$numIncorrect), function(i) input[[paste0('incorrectAnswer', i)]])
      ), collapse = "|")
    } else {
      question$Answers <- input$correctAnswer
      question$Tolerance <- input$tolerance
    }
    
    # Append the new question to the data frame of questions and show a notification
    # to the user that the question has been added.
    questions$data <- rbind(questions$data, question)
    showNotification("Question added!", type = "message")
  })
  
  # Render the table showing current questions.
  output$table <- renderDT({
    questions$data
  })
  
  # Writes the questions to the Rmd file when button is clicked. Adds a last question number
  # identifier to the end of the file. This gets commented out of the markdown portion of the
  # assignment, and is used to pick up where the app left off when appending to an existing file.
  # Otherwise, the question number template starts back at one, which is problematic.
  observeEvent(input$writeToFile, {
    req(nzchar(selectedFilePath()))
    rmd_file <- selectedFilePath()
    if (file.exists(rmd_file)) {
      # Read the file content.
      file_content <- readLines(rmd_file)
      # Find the line with the last question number.
      last_question_index <- grep("<!-- Last question number: Question", file_content)
      last_question_number <- 0
      # If found, delete this line and all lines after it.
      if (length(last_question_index) > 0) {
        last_question_number <- as.integer(str_extract(file_content[last_question_index], "\\d+"))
        file_content <- file_content[-c(last_question_index:length(file_content))]
      }
      # Write the (possibly modified) file content back to the file.
      writeLines(file_content, rmd_file)
      
      fileConn <- file(rmd_file, open = "a")
      cat("\n", file = fileConn)
      
      # Generates questions based on predetermined template. Makes use of previously.
      # stored question data.
      for (i in seq_len(nrow(questions$data))) {
        
        # Compute the question number for the current question.
        question_number <- last_question_number + i
        
        # Retrieve the data for the current question.
        question <- questions$data[i, ]
        
        # If the question type is multiple choice.
        if (question$Type == 1) {
          
          # Split the answers into a list.
          answers <- strsplit(question$Answers, "\\|")[[1]]
          
          # Determine the number of correct answers.
          correct_count <- ifelse(question$Points > length(answers), length(answers), question$Points)
          
          # Prepare the R code for the answers.
          answers <- c(
            paste0("answer('", answers[1:correct_count], "', correct = TRUE)"),
            paste0("answer('", answers[(correct_count + 1):length(answers)], "', correct = FALSE)")
          )
          
          # Generate the R code block for the question.
          question_block <- paste0(
            "\n\n```{r question_", question_number, "}\n",
            "question(\"[Question ", question_number, "] [Points: ", question$Points, "] ", question$Query, "\",\n",
            "    ", paste(answers, collapse = ",\n    "), ",\n",
            "    allow_retry = TRUE,\n",
            "    random_answer_order = TRUE)\n",
            "```\n"
          )
          
          # Else, the question type is numeric. Generate the R code block for the question.
        } else {
          question_block <- paste0(
            "\n\n```{r question_", question_number, "}\n",
            "question_numeric(\"[Question ", question_number, "] [Points: ", question$Points, "] ", question$Query, "\",\n",
            "    answer(", question$Answers, ", correct = TRUE),\n",
            "    allow_retry = TRUE,\n",
            "    tolerance = ", question$Tolerance, ")\n",
            "```\n"
          )
        }
        
        # Write the generated R code block to the Rmd file.
        cat(question_block, file = fileConn, append = TRUE)
      }
      
      # Writes the last question number as a comment at the end of the file.
      cat(file=fileConn, paste0("<!-- Last question number: Question ", question_number, " -->"), "\n")
      
      close(fileConn)
      showNotification("Questions written to file!", type = "message")
      
      # Case for if the file does not exist.
    } else {
      showNotification("Please choose an existing file or enter a new file name.", type = "warning")
    }
  })
  
}

shinyApp(ui = ui, server = server)