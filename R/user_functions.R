utils::globalVariables(c("qtbl", "soltbl", "testlog", "ranktbl", "<<-"), add = TRUE)

#' Load quiz data into R environment
#' 
#' If using for the first time, creates .quizme directory in users home directory for data storage. Also creates empty objects for storing question/answer data.
#' 
#' @return three objects: q_tbl, sol_tbl and data_tbl containing questions, answers and metadata
#' 
#' @importFrom data.table data.table rbindlist
#' @importFrom readr read_rds
#' @importFrom lubridate as.duration
#' 
#' @examples
#' \dontrun{quizme()}
#' 
#' @export
quizme <- function() {
    suppressMessages(library(lubridate))
    if(!dir.exists("~/.quizme")) {
        dir.create("~/.quizme")
    }
    if(!file.exists("~/.quizme/quizdata")) {
    qtbl <<- data.table(id = integer(0),
                question = character(0),
                tags = character(0),
                timecreated = as.POSIXct(character(0)))
    soltbl <<- list(id = integer(0), answer = list())
    testlog <<- data.table(id = integer(0),
                               time = .POSIXct(character(0)),
                               dt = as.duration(double(0)),
                               response = factor(levels = c("Y", "N")))
    ranktbl <<- data.table(id = integer(0),
                           due = as.POSIXct(character(0)),
                           status = factor(levels = c("learning",
                                                      "new",
                                                      "learned",
                                                      "mastered"),
                                           ordered = TRUE),
                           rank = integer(0)
                           )
    } else {
        data_obj <- read_rds("~/.quizme/quizdata")
        qtbl <<- data_obj[[1]] 
        soltbl <<- data_obj[[2]] 
        testlog <<- data_obj[[3]] 
        ranktbl <<- data_obj[[4]] 
    }
}

#' Add question-answer
#' 
#' Add question in one line (without carriage return). Add answers in the following line(s). Carriage return in a blank line will save the question and answer to the data objects.
#' 
#' @param tags an optional string containing tags to classify question into a category(s)
#' 
#' @return three objects: q_tbl, sol_tbl and data_tbl updated with the new question-answer.
#' 
#' @importFrom lubridate now hour minute second
#' @importFrom data.table rbindlist
#' 
#' @examples
#' \dontrun{addq()}
#' 
#' @export
addq <- function(tags = c("")) {
    x <- scan(what = character(), sep = "\n")
    tot <- nrow(qtbl)
    timecreated = now()
    q <- data.table(id = tot + 1L,
                     question = x[1],
                     tags = tags,
                     timecreated = timecreated)
    qtbl <<- rbindlist(list(qtbl, q))
    soltbl[[1]][tot + 1] <<- tot + 1L
    soltbl[[2]][[tot + 1]] <<- x[-1]
    time_midnight <- timecreated
    hour(time_midnight) <- 0
    minute(time_midnight) <- 0
    second(time_midnight) <- 0
    addToRankingTbl(time = time_midnight)
}

#' Show a question from the Q&A repository
#' 
#' A randomly selected question will be shown. For R questions, you can simply enter your answer (code) at the R console directly. Answers are not evaluated by the package. This is up to the user to decide whether they answered the question correctly or not.
#' 
#' @importFrom lubridate now
#' 
#' @return randomly selected question
#' 
#' @examples
#' \dontrun{ask()}
#' 
#' 
#' @export
ask <- function() {
    if(nrow(qtbl) == 0) {
        cat("no questions exist yet \nplease use addq() to add questions\n")
    } else {
    qid <<- ranktbl[1, id]
    timeasked <<- now()
    question <- qtbl[qid, 2]
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
    if(nrow(qtbl) == 0) {
        cat("no questions exist yet \nplease use addq() to add questions\n")
    } else {
    answer <- soltbl[[2]][[qid]]
    for(i in seq_along(answer)) {
        cat(paste(answer[i],"\n"))
    }
    }
}

#' Correct answer response
#' 
#' This will log data about correct response
#' 
#' @importFrom lubridate now as.duration 
#' 
#' @return does not return anything, just updates the datalog
#' 
#' @examples
#' \dontrun{done()}
#' 
#' @export
hit <- function() {
    last <- list(id = qid,
                 time = timeasked,
                 dt = round(as.duration(now() - timeasked)),
                 response = "yes")
    testlog <<- rbindlist(list(testlog, last))
    updatetime()
}


#' Wrong answer response
#' 
#' This will log data about correct response
#' 
#' @importFrom lubridate as.duration now
#' @importFrom data.table rbindlist
#'
#' @return does not return anything, just updates the datalog
#' 
#' @export
miss <- function() {
    last <- list(id = qid,
                 time = timeasked,
                 dt = round(as.duration(now() - timeasked)),
                 response = "no")
    testlog <<- rbindlist(list(testlog, last))
    updaterank()
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
    shuffleWithinGroup()
    write_rds(list(qtbl, soltbl, testlog, ranktbl), "~/.quizme/quizdata")
    detach(package:lubridate)
}
