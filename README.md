# CSRI 2023 Project — R-based assignment software (name undecided).

## Overview:

This repository contains an event recording script and a selection of Shiny applications that turn `learnr` tutorials into jerry-rigged homework problems capable of recording user submissions, assigning weighted point values to questions, and more. The data is collected in a format that allows for automatic grade calculation and more complex data analysis. It's important to acknowledge that this is mostly a shell tossed together in 8 weeks, and, while it's pretty much ready for deployment (save for the question of user-authentication — more on that later), there's a lot of room for polish and added functionalities.

## Contents:

### Event Recording:

The `eventRecorderLocal.R` script observes `learnr`'s tutorial events and writes the following information to a CSV file upon each question submission:

- **User ID:** this is determined by the server/browser, and identifies the person who submitted the question.
- **Question Number:** the question number, extracted from a regex identifier in the question. title.
- **Attempt Number:** the attempt number for a given question.
- **Correctness:** a boolean value for whether or not the user answered the question correctly.
- **Points:** the question's point value, extracted in the same way that the question number is.
- **Time:** the system time at the moment of submission. This isn't taken into account in any of the analysis apps and is more there for added information if desired/necessary.

In order for the script to function, it must be called in the setup chunk of an assignment like so: `options(tutorial.event_recorder = eventRecorder)`. This line of code runs the script each time a `learnr` tutorial event occurs.

There also exists an `eventRecorderGoogle.R` script. This is somewhat legacy code, from a time when we were trying to write to Google Sheets instead of a local CSV file. Our reason for doing so was that certain hosting services, such as shinyapps.io, don't allow for us to export a CSV file, and so this allows for a workaround. As we plan to host the assignments on our own server, this ended up becoming somewhat obsolete, but I'm keeping it in here in case someone needs to accomplish something similar in the future. Please note that the CSV output structure of `eventRecorderLocal.R` was recently modified, but that `eventRecorderGoogle.R` was not updated in stride, so it must be further modified in order to output CSV files that function with our analysis apps.

`eventRecorderGoogle.R` works by connecting to a Google service account through use of the `googledrive` and `googlesheets4` packages. In order to do so, an authentication token must be used, which is what `~/Authentication Token/csri-database-token.json` is for. That said, it serves only as an example, as this specific token is tied to a test account of mine.

### Assignment Generation:

The event recording scripts should, in practice, work with any and all `learnr` quiz questions, with one stipulation: the regular expression identifiers in the question title. The script expects to see the format `"[Question X] [Points: Y] <insert question here>"`. If it does not find the regex for question and point values, NAs will be written to the CSV, which isn't ideal.

To both solve the problem of questions requiring a specific template to be followed, and to allow for an added degree of user-friendliness. I wrote the `assignmentGenerator` Shiny application, which does just as the name implies. The app is capable of creating both multiple choice and numerical questions based on pre-defined templates. It's very important to note that because the app works based on pre-defined templates, any alteration to the formats of the regex identifiers, script names, or other parts of code requiring a degree of specificity or adherence to a template **must** be reflected in this application, as otherwise it will generate non-functional assignments.

### Analysis:

As assignment data is recorded in CSV files, manual analysis is of course possible. That said, for the sake of both user-friendliness and efficiency, two apps, both `singleAnalysis` and `multiAnalysis`, exist to created predetermined graphs of the data. Being honest, they should probably be combined into a single app — `multiAnalysis`, in particular, is quite bare-bones — but I ran out of time. `multiAnalysis` runs analysis on every CSV file in the `./Assignments` folder.

### User-Authentication (or Lack Thereof):

Currently, authentication — making sure we know who submitted what, and that they're who they say they are — is the biggest hurdle in the way of full-on deployment. Professor George believes that he has a solution involving the paid development versions of our Shiny and RStudio servers, but we've yet to test it.


