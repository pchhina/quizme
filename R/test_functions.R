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
#' \dontrun{make_quiz()}
#' 
#' @export
quizme <- function() {
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
#' @return three objects: q_tbl, sol_tbl and data_tbl updated with the new question-answer.
#' 
#' @importFrom lubridate now update
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
    time_midnight <- update(timecreated,
                            hour = 0,
                            minute = 0,
                            second = 0)
    addToRankingTbl(time = time_midnight)
}

#' Add question to ranking table
#' 
#' @importFrom data.table rbindlist
#' 
#' @return updated rankingtbl
#' 
addToRankingTbl <- function(time) {
    r <- data.table(id = qtbl[id == max(id), id],
                     due = time,
                     status = "new",
                     rank = max(1, ranktbl[, max(rank)] + 1, 1))
    ranktbl <<- rbindlist(list(ranktbl, r))
    updateranktbl()
}

#' Show a question from the Q&A repository
#' 
#' A randomly selected question will be shown. For R questions, you can simply enter your answer (code) at the R console directly. Answers are not evaluated by the package. This is up to the user to decide whether they answered the question correctly or not.
#' 
#' @importFrom lubridate now update
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
#' @importFrom lubridate now as.duration today date
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
    updateranktbl()
}


#' update time
#' 
#' @importFrom lubridate update today date
#'
updatetime <- function() {
    ncor <- testlog[id == qid &
                    date(time) == today() &
                    response == "yes", .N]
                if (ranktbl[id == qid, status] == "new") {
                    if (ncor <= 1) {
                        t = 0
                        updaterank()
                    } else if (ncor == 2) {
                        t =  2
                    } else {
                        t =  24
                        ranktbl[id == qid, status := "learning"]
                    }
                } else {
                    tlast <- days_elapsed(qid = qid)
                    t <- t_learning(tlast = tlast, qid = qid)
                }
    newdew <- update(ranktbl[id == qid, due],  hour = t)
    ranktbl[id == qid, due := newdew] # add t hours to current time
}

#' Number of days elapsed
#' 
#' Finds number of days elapsed between the last two askings of qid
#' 
#' @importFrom lubridate update today date
#'
days_elapsed <- function(qid) {
    timevec <- testlog[id == qid, time]
    t2 <- timevec[length(timevec)]
    t1 <- timevec[length(timevec) - 1]
    day(t2) - day(t1)
}

#' Number of days elapsed
#' 
#' Finds number of hours elapsed between the last two askings of qid
#' Finds new time(hours) to update for learning questions given tlast and qid
#' This will be called in by updatetime()
#' 
#' @importFrom lubridate as.duration
#'
t_learning <- function(tlast, qid) {
    dtvec <- testlog[id == qid, dt]
    dtlast <- dtvec[length(dtvec)]
    if (dtlast < as.duration(30)) {
        t <- tlast + 3
    } else if (dtlast < as.duration(120)) {
        t <- tlast + 2
    } else {
        t <- tlast + 1
    }
    t * 24 # to convert into hours which is what updatetime needs
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
    updateranktbl()
}

updaterank <- function() {
    maxrank <- ranktbl[, max(rank)]
    ranktbl[id == qid, rank := maxrank + 1]
}

updateranktbl <- function() {
    ranktbl <<- ranktbl[order(due, status, rank)]
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
    write_rds(list(qtbl, soltbl, testlog, ranktbl), "~/.quizme/quizdata")
}
