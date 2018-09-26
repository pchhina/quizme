# quizme: automatic test generator from user's text file

## This project contains functions to 
- take the user's text file in question/answer format and generates test questions for self quizzing
- since the input file is a plain vanilla ASCII text file, the questions are language agnostic. This opens up possibilities to mix/match any learning material that you are currently focusing on

## Usage
- Download test.R, test_file.R and test_functions.R in your working directory
- In R console, run `source("test_functions.R")`
- test.R contains list of questions tagged by #<id> while test_file.R contains both questions and answers
- In R console, use `ask()` to present you with a question and `tell(<id>)` to show you the answer
