generateQuestions <- function(file_name = "Assignment.Rmd") {
  # Initialize question number
  question_number <- 0
  if (file.exists(file_name)) {
    lines <- readLines(file_name)
    last_question_line <- grep("<!-- Last question number:", lines, value = TRUE)
    if (length(last_question_line) > 0) {
      question_number <- as.numeric(sub(".*Question ([0-9]+).*", "\\1", last_question_line))
      # Find the index of the last question comment
      last_line_index <- grep("<!-- Last question number:", lines)
      # Remove all lines from the last question comment onwards
      lines <- lines[1:(last_line_index - 1)]
    }
    # Write the lines back to the file
    writeLines(lines, file_name)
  } else {
    # If the file does not exist, write the setup code chunk to the file
    fileConn <- file(file_name, open = "a")
    setup_chunk <- '---
title: ""
output: learnr::tutorial
runtime: shiny_prerendered
---

```{r setup}
# Import required libraries
library(learnr)
library(dplyr)
library(tidyr)
library(purrr)
library(googlesheets4)
library(googledrive)

source("~/CSRI-2023/Scripts/EventRecorder.R")

# REQUIRED: Set the global option to use our event recorder function.
options(tutorial.eventRecorder = eventRecorder)

# Set knitr options
knitr::opts_chunk$set(echo = FALSE)
```'
cat(file=fileConn, setup_chunk, "\n")

while (TRUE) {
  cat("Would you like to add an authentication block? [1] Yes / [0] No: ")
  auth_answer <- readLines(n = 1)
  if (auth_answer == "1" || auth_answer == "0") {
    break
  } else {
    cat("Invalid response. Please enter 1 for Yes and 0 for No.\n")
  }
}

if (auth_answer == "1") {
  # Read pins from the CSV file
  pins <- t(as.vector(read.csv("~/CSRI-2023/Scripts/pinData.csv", header = FALSE)))
  
  # Generate authentication block
  pin_answers <- paste0("answer(", pins, ", correct = TRUE)", collapse = ",\n    ")
  
  authentication_block <- paste0(
    "\n\n```{r auth_question}\n",
    "question_text(\"Enter your PIN:\",\n",
    "    '", pin_answers, "',\n",
    "    allow_retry = TRUE,\n",
    "    tolerance = 0)\n",
    "```"
  )
  
  cat(file=fileConn, authentication_block, "\n")
}

tryCatch(close(fileConn), error=function(e) NULL)
  }
  
  repeat {
    # Increment question number
    question_number <- question_number + 1
    
    # Open the RMarkdown file
    fileConn <- file(file_name, open = "a")
    
    while (TRUE) {
      cat("Please enter the question type. [1] Multiple Choice / [2] Numeric: ")
      question_type <- readLines(n = 1)
      if (question_type == "1" || question_type == "2") {
        break
      } else {
        cat("Invalid response. Please enter 1 for Multiple Choice and 2 for Numeric.\n")
      }
    }
    
    cat("Please enter the points for this question: ")
    points <- gsub(",", "", readLines(n = 1))
    
    if (question_type == "1") {
      # Prompt for multiple choice question information
      cat("Please enter the question query: ")
      question_query <- readLines(n = 1)
      
      # Add question number and points to the query
      question_query <- paste0("[Question_", question_number, "] [Points: ", points, "] ", question_query)
      
      cat("Please enter the number of correct answers: ")
      num_correct <- as.integer(readLines(n = 1))
      
      # Create a list to hold all the answers
      answers <- list()
      
      # Loop to prompt for each correct answer
      for (i in 1:num_correct) {
        cat(paste0("Please enter correct answer ", i, ": "))
        correct_answer <- gsub(",", "", readLines(n = 1))
        answers[[i]] <- paste0("answer('", correct_answer, "', correct = TRUE)")
      }
      
      cat("Please enter the number of incorrect answers: ")
      num_incorrect <- as.integer(readLines(n = 1))
      
      # Loop to prompt for each incorrect answer
      for (i in 1:num_incorrect) {
        cat(paste0("Please enter incorrect answer ", i, ": "))
        incorrect_answer <- gsub(",", "", readLines(n = 1))
        answers[[i + num_correct]] <- paste0("answer('", incorrect_answer, "')")
      }
      
      # Generate the learnr multiple choice question syntax
      question_block <- paste0(
        "\n\n```{r question ", question_number, "}\n",
        "question(\"", question_query, "\",\n",
        "    ", paste(answers, collapse = ",\n    "), ",\n",
        "    allow_retry = TRUE,\n",
        "    random_answer_order = TRUE)\n",
        "```"
      )
    } else if (question_type == "2") {
      # Prompt for numeric question information
      cat("Please enter the question query: ")
      question_query <- readLines(n = 1)
      
      # Add question number and points to the query
      question_query <- paste0("[Question_", question_number, "] [Points: ", points, "] ", question_query)
      
      cat("Please enter the correct answer: ")
      correct_answer <- gsub(",", "", readLines(n = 1))
      
      cat("Please enter the tolerance: ")
      tolerance <- gsub(",", "", readLines(n = 1))
      
      # Generate the learnr numeric question syntax
      question_block <- paste0(
        "\n\n```{r question ", question_number, "}\n",
        "question_numeric(\"", question_query, "\",\n",
        "    answer(", correct_answer, ", correct = TRUE),\n",
        "    allow_retry = TRUE,\n",
        "    tolerance = ", tolerance, ")\n",
        "```"
      )
    }
    
    # Write the question block to the file
    cat(file=fileConn, question_block, "\n")
    
    # Close the RMarkdown file
    tryCatch(close(fileConn), error=function(e) NULL)
    
    while (TRUE) {
      cat("Would you like to generate another question? [1] Yes / [0] No: ")
      another <- readLines(n = 1)
      if (another == "1" || another == "0") {
        break
      } else {
        cat("Invalid response. Please enter 1 for Yes and 0 for No.\n")
      }
    }
    
    if (another != "1") break
  }
  
  # Open the RMarkdown file
  fileConn <- file(file_name, open = "a")
  
  # Write the last question number as a comment at the end of the file
  cat(file=fileConn, paste0("<!-- Last question number: Question ", question_number, " -->"), "\n")
  
  # Add submission block
  submission_block <- '```{r submission}
question("Submit Assignment?",
         answer("Yes", correct = TRUE)
         )
```'
  cat(file=fileConn, submission_block, "\n")
  
  # Close the RMarkdown file
  tryCatch(close(fileConn), error=function(e) NULL)
}
