# Initialize an empty data frame
df <- data.frame(
  pin = character(),
  problem = character(),
  points = integer(),
  answer = character(), 
  correct = logical(),
  question_num = character(),
  stringsAsFactors = FALSE  # Prevent strings being converted to factors
)

# Initialize pin to NA outside the function
pin <- NA

# Function to record tutorial events
eventRecorder <- function(tutorial_id, tutorial_version, event, data, user_id) {
  
  # Authenticate with Google Sheets
  gs4_auth(
    path = "~/CSRI-2023/Auth/csri-database-token.json",
    scopes = c(
      "https://www.googleapis.com/auth/spreadsheets",
      "https://www.googleapis.com/auth/drive"
    )
  )
  
  # Authenticate with Google Drive
  drive_auth(path = "~/CSRI-2023/Auth/csri-database-token.json")
  
  # Check if event is exercise_submission or question_submission
  if (event %in% c("exercise_submission", "question_submission")) {
    
    # Check if question contains [Authentication]
    if (grepl("\\[Authentication\\]", data$question)) {
      # Store the answer to the authentication question as the pin
      pin <<- data$answer
    } else if (data$question != "Submit Assignment?") {  # Ignore 'Submit Assignment?' question
      
      # Extract question number from the question
      question_num <- ifelse(grepl("\\[Question \\d+\\]", data$question), 
                             gsub(".*\\[Question (\\d+)\\].*", "\\1", data$question), NA)
      
      # Extract points from the question
      points <- ifelse(grepl("\\[Points: \\d+\\]", data$question), 
                       as.integer(gsub(".*\\[Points: (\\d+)\\].*", "\\1", data$question)), 1)
      
      # Check if there's already a row with the same pin and question_num
      previous_rows <- df[df$pin == pin & gsub("\\..*", "", df$question_num) == paste0("Q", question_num), ]
      if (nrow(previous_rows) > 0) {
        attempt_num <- max(as.numeric(gsub(".*\\.(\\d+)$", "\\1", previous_rows$question_num))) + 1
        # Adjust points for retries
        points <- points - 0.5 * (attempt_num - 1)
      } else {
        attempt_num <- 1
      }
      
      # Extract problem without [Question X] and [Points: X]
      problem <- gsub("\\[Question \\d+\\]|\\[Points: \\d+\\]", "", data$question)
      
      # Create new row with event data
      new_row <- data.frame(
        pin = pin,
        problem = problem,
        points = points,
        answer = data$answer, 
        correct = ifelse(data$correct, TRUE, FALSE),
        question_num = paste0("Q", question_num, ".", attempt_num),
        stringsAsFactors = FALSE
      )
      
      # Append new row to global data frame
      df <<- rbind(df, new_row)
    }
    
    # If it's the final question, reshape and write data frame to Google Sheets
    if(data$question == "Submit Assignment?") {
      
      # Reshape data frame to long format
      df_long <- df %>% 
        gather(key = "key", value = "value", -pin, -question_num)
      
      # Pivot the data frame back to wide format
      df_wide <- df_long %>% 
        pivot_wider(names_from = c("question_num", "key"), names_sep = "_", values_from = "value")
      
      # Generate the column order
      unique_questions <- sort(unique(df$question_num))
      col_order <- c("pin", unlist(lapply(unique_questions, function(i) paste0(i, c("_problem", "_answer", "_correct", "_points")))))
      
      # Reorder the columns
      df_wide <- df_wide[, col_order]
      
      # Create a new Google Sheet and write the data frame to it
      ss <- gs4_create(name = "logfile")
      sheet_write(df_wide, ss = ss, sheet = "Sheet1")
      
      # Share the new Google Sheet with a user
      drive_share(
        ss,
        role = "writer",
        type = "user",
        emailAddress = "cbergquist25@cornellcollege.edu"
      )
    }
  }
}
