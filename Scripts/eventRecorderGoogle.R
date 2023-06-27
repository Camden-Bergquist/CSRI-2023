# Initialize an empty data frame to record information.
df <- data.frame(
  pin = character(),
  problem = character(),
  points = integer(),
  answer = character(), 
  correct = logical(),
  question_num = character(),
  stringsAsFactors = FALSE  # Prevent strings being converted to factors.
)

# The variable 'pin' is initialized to NA. 
# It's used to handle cases where the tutorial event occurs before the user is authenticated,
# or else, there's no authentication question in the first place.
pin <- NA

# This function is designed to handle, process, and store incoming learnr tutorial events. 
eventRecorder <- function(tutorial_id, tutorial_version, event, data, user_id) {
  
  # This block authenticates with Google Sheets and Google Drive using a JSON token file.
  # The authentication allows the function to interact with Google services, such as storing data in Google Sheets.
  gs4_auth(
    path = "~/CSRI-2023/Auth/csri-database-token.json",
    scopes = c(
      "https://www.googleapis.com/auth/spreadsheets",
      "https://www.googleapis.com/auth/drive"
    )
  )
  
  drive_auth(path = "~/CSRI-2023/Auth/csri-database-token.json")
  
  # This 'if' condition filters incoming events. The goal is to only process 'exercise_submission' and 'question_submission' events,
  # as these contain meaningful user data. All other event types aren't useful for the current purposes and are ignored.
  if (event %in% c("exercise_submission", "question_submission")) {
    
    # The following condition checks for the authentication question. The user's submitted pin
    # is saved for later use to identify which user submitted the upcoming tutorial events.
    # For each non-authentication and non-submission question, data is extracted and stored.
    if (grepl("\\[Authentication\\]", data$question)) {
      pin <<- data$answer
    } else if (data$question != "Submit Assignment?") {  # Ignore 'Submit Assignment?' question since it carries no important info.
      
      # Extract question number and points from the regex in the question title.
      question_num <- ifelse(grepl("\\[Question \\d+\\]", data$question), 
                             gsub(".*\\[Question (\\d+)\\].*", "\\1", data$question), NA)
      
      points <- ifelse(grepl("\\[Points: \\d+\\]", data$question), 
                       as.integer(gsub(".*\\[Points: (\\d+)\\].*", "\\1", data$question)), 1)
      
      # To account for multiple attempts, check if there's already a row with a given question's identifiers.
      previous_rows <- df[df$pin == pin & gsub("\\..*", "", df$question_num) == paste0("Q", question_num), ]
      if (nrow(previous_rows) > 0) {
        attempt_num <- max(as.numeric(gsub(".*\\.(\\d+)$", "\\1", previous_rows$question_num))) + 1
        # For each attempt past the first, deduct half a point.
        points <- points - 0.5 * (attempt_num - 1)
      } else {
        attempt_num <- 1
      }
      
      # Extract problem query without regex identifiers.
      problem <- gsub("\\[Question \\d+\\]|\\[Points: \\d+\\]", "", data$question)
      
      # Creating and appending new row with the event data to add to our data frame.
      new_row <- data.frame(
        pin = pin,
        problem = problem,
        points = points,
        answer = data$answer, 
        correct = ifelse(data$correct, TRUE, FALSE),
        question_num = paste0("Q", question_num, ".", attempt_num),
        stringsAsFactors = FALSE
      )
    
      df <<- rbind(df, new_row)
    }
    
    # In reaction to the submission question, the function transforms the data and writes it to a Google Sheet.
    if(data$question == "Submit Assignment?") {
      
      # Reshape/pivot.
      df_long <- df %>% 
        gather(key = "key", value = "value", -pin, -question_num)
      
      df_wide <- df_long %>% 
        pivot_wider(names_from = c("question_num", "key"), names_sep = "_", values_from = "value")
      
      # Generate column order, reorder.
      unique_questions <- sort(unique(df$question_num))
      col_order <- c("pin", unlist(lapply(unique_questions, function(i) paste0(i, c("_problem", "_answer", "_correct", "_points")))))
      
      df_wide <- df_wide[, col_order]
      
      # Create a new Google Sheet and writing the reshaped data frame to it.
      # Then, share the new Google Sheet with a given user.
      ss <- gs4_create(name = "logfile")
      sheet_write(df_wide, ss = ss, sheet = "Sheet1")
      
      drive_share(
        ss,
        role = "writer",
        type = "user",
        emailAddress = "cbergquist25@cornellcollege.edu"
      )
    }
  }
}