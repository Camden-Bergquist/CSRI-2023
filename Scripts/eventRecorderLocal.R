# Initialize an empty data frame to record event information.
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
  
  # This 'if' condition filters incoming events. The goal is to only process 'exercise_submission' and 'question_submission' events,
  # as these contain meaningful user data. All other event types aren't useful for the current purposes and are ignored.
  if (event %in% c("exercise_submission", "question_submission")) {
    
    # For each non-submission question, data is extracted and stored.
    if (data$question != "[Enter PIN] Submit Assignment?") {  # Ignore 'Submit Assignment?' question since it carries no important info.
      
      # Extract question number and points from the regex in the question title.
      question_num <- ifelse(grepl("\\[Question \\d+\\]", data$question), 
                             gsub(".*\\[Question (\\d+)\\].*", "\\1", data$question), NA)
      
      points <- ifelse(grepl("\\[Points: \\d+\\]", data$question), 
                       as.integer(gsub(".*\\[Points: (\\d+)\\].*", "\\1", data$question)), 1)
      
      # To account for multiple attempts, check if there's already a row with a given question's identifiers.
      previous_rows <- df[gsub("\\..*", "", df$question_num) == paste0("Q", question_num), ]
      if (nrow(previous_rows) > 0) {
        attempt_num <- max(as.numeric(gsub(".*\\.(\\d+)$", "\\1", previous_rows$question_num)), na.rm = TRUE) + 1
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
    
    # In reaction to the submission question, the function transforms the data and writes it to a CSV file.
    if(data$question == "[Enter PIN] Submit Assignment?" && data$correct == TRUE) {
      
      pin <<- data$answer
      df$pin <<- pin
      
      # Reshape/pivot.
      df_long <- df %>% 
        gather(key = "key", value = "value", -pin, -question_num)
      
      df_wide <- df_long %>% 
        pivot_wider(names_from = c("question_num", "key"), names_sep = "_", values_from = "value")
      
      # Generate column order, reorder.
      unique_questions <- sort(unique(df$question_num))
      col_order <- c("pin", unlist(lapply(unique_questions, function(i) paste0(i, c("_problem", "_answer", "_correct", "_points")))))
      
      df_wide <- df_wide[, col_order]
      
      # Check if the CSV file exists. If it does, append the reshaped data frame to it. 
      # If it doesn't, write the reshaped data frame to a new CSV file.
      csv_file_path <- "./data/logfile.csv"
      if (file.exists(csv_file_path)) {
        write.table(df_wide, file = csv_file_path, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
      } else {
        write.table(df_wide, file = csv_file_path, sep = ",", append = FALSE, row.names = FALSE, col.names = TRUE)
      }
    }
  }
}
