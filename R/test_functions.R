utils::globalVariables(c("q_tbl", "sol_tbl", "data_tbl", "q_id", "<<-"), add = TRUE)
#' Load quiz data into R environment
#' 
#' If using for the first time, creates .quizme directory in users home directory for data storage. Also creates empty objects for storing question/answer data.
#' 
#' @return three objects: q_tbl, sol_tbl and data_tbl containing questions, answers and metadata
#' 
#' @importFrom tibble tibble
#' @importFrom readr read_rds
#' 
#' @examples
#' \dontrun{make_quiz()}
#' 
#' @export
make_quiz <- function() {
    if(!dir.exists("~/.quizme")) {
        dir.create("~/.quizme")
    }
    if(!file.exists("~/.quizme/quizdata")) {
    q_tbl <<- tibble(id = integer(0),
                question = character(0),
                prob = double(0))
    sol_tbl <<- list(id = integer(0), answer = list())
    data_tbl <<- list(id = integer(0),
                 t_create = character(0),
                 dt = double(0),
                 status = character(0))

    } else {
        data_obj <- read_rds("~/.quizme/quizdata")
        q_tbl <<- data_obj[[1]] 
        sol_tbl <<- data_obj[[2]] 
        data_tbl <<- data_obj[[3]] 
    }

}

#' Add question-answer
#' 
#' Add question in one line (without carriage return). Add answers in the following line(s). Carriage return in a blank line will save the question and answer to the data objects.
#' 
#' @return three objects: q_tbl, sol_tbl and data_tbl updated with the new question-answer.
#' 
#' @examples
#' \dontrun{addq()}
#' 
#' @export
addq <- function() {
    x <- scan(what = character(), sep = "\n")
    tot <- nrow(q_tbl)
    q_tbl[tot + 1, 1] <<- tot + 1L
    q_tbl[tot + 1, 2] <<- x[1]
    q_tbl[tot + 1, 3] <<- 120
    sol_tbl[[1]][tot + 1] <<- tot + 1L
    sol_tbl[[2]][[tot + 1]] <<- x[-1]
    data_tbl[[1]][tot + 1] <<- tot + 1L
    data_tbl[[2]][tot + 1] <<- Sys.time()
    data_tbl[[3]][tot + 1] <<- 120
    data_tbl[[4]][tot + 1] <<- "new"
}

#' Show a question from the Q&A repository
#' 
#' A randomly selected question will be shown. For R questions, you can simply enter your answer (code) at the R console directly. Answers are not evaluated by the package. This is up to the user to decide whether they answered the question correctly or not.
#' 
#' @return randomly selected question
#' 
#' @examples
#' \dontrun{ask()}
#' 
#' @importFrom dplyr pull
#' 
#' @export
ask <- function() {
    if(nrow(q_tbl) == 0) {
        cat("no questions exist yet \nplease use addq() to add questions\n")
    } else {
    q_id <<- sample(1:nrow(q_tbl), 1, prob = pull(q_tbl[, 3]))
    question <- q_tbl[q_id, 2]
    cat(paste(question, "\n"))
    }
}

#' Show answer
#' 
#' This will show the answer to the last question presented
#' 
#' @return answer to the last question presented. This helps the user to evaluate their answer.
#' 
#' @examples
#' \dontrun{tell()}
#' 
#' @export
tell <- function() {
    if(nrow(q_tbl) == 0) {
        cat("no questions exist yet \nplease use addq() to add questions\n")
    } else {
    answer <- sol_tbl[[2]][[q_id]]
    for(i in seq_along(answer)) {
        cat(paste(answer[i],"\n"))
    }
    }
}

#' Closes the quiz session
#' 
#' This is important as it updates the files on disk with any new questions added in current session. After updating the qa file, it clears out the R environment by removing the objects and functions related to this package.
#' 
#' @return None
#' 
#' @importFrom readr write_rds
#' 
#' @examples
#' \dontrun{bye()}
#' 
#' @export
bye <- function() {
    write_rds(list(q_tbl, sol_tbl, data_tbl), "~/.quizme/quizdata")
    vars <- c("q_tbl", "sol_tbl", "data_tbl", "q_id", "make_quiz",
              "addq", "ask", "tell")
    rm(list = vars, pos = ".GlobalEnv")
}
