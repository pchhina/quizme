[![Travis-CI Build Status](https://travis-ci.org/pchhina/quizme.svg?branch=master)](https://travis-ci.org/pchhina/quizme)

# quizme: solidify your learning through deliberate practice

## An R package to
- create a repository of question & answers on any subject
- create a quiz from the q&a repository that can be practiced from R console

## Usage
- Install the package from github: `install_github("pchhina/quizme")`
- `quizme()` to load q&a set. If q&a set does not exist, a skeleton file structure is created to which questions can be added (see below) 
- `addq()` to add a new question-answer. First line should be a question. Answer can be multiple lines. Carriage return on blank line adds your question
- `ask()` presents a question selected using spaced-repetition algorithm
- `hit()` if you answered correctly
- `miss()` if you do not know the answer
- `tell()` show the answer to the current question
- `bye()` updates the repository with new questions from the current session.
