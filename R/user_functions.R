utils::globalVariables(c("qtbl", "soltbl", "testlog", "ranktbl", "slog", "<<-"), add = TRUE)

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
    slog <<- data.table(sid = integer(0),
                        sdate = as.Date(as.POSIXct(character(0))),
                        sstart = as.POSIXct(character(0)),
                        sdur = numeric(0),
                        sallq = integer(0),
                        snewq = integer(0),
                        snew2learningq = integer(0)
                        )
    slog <<- rbindlist(list(slog, startSession()))
    } else {
        data_obj <- read_rds("~/.quizme/quizdata")
        qtbl <<- data_obj[[1]] 
        soltbl <<- data_obj[[2]] 
        testlog <<- data_obj[[3]] 
        ranktbl <<- data_obj[[4]] 
        mvPastdueToToday(ranktbl)
        slog <<- data_obj[[5]]
        slog <<- rbindlist(list(slog, startSession()))
    }
    new2learn <<- 0L
}

#' Add question-answer
#' 
#' Add question in one line (without carriage return). Add answers in the following line(s). Carriage return in a blank line will save the question and answer to the data objects.
#' 
#' @param tags an optional string containing tags to classify question into a category(s)
#' 
#' @return three objects: q_tbl, sol_tbl and data_tbl updated with the new question-answer.
#' 
#' @importFrom lubridate now hour minute second ymd_h
#' @importFrom data.table rbindlist
#' 
#' @examples
#' \dontrun{addq()}
#' 
#' @export
addq <- function(tags = c("")) {
    tot <- nrow(qtbl)
    cat(paste("Adding Question ",tot + 1L, "\n"))
    x <- scan(what = character(), sep = "\n")
    timecreated = now()
    q <- data.table(id = tot + 1L,
                     question = x[1],
                     tags = tags,
                     timecreated = timecreated)
    qtbl <<- rbindlist(list(qtbl, q))
    soltbl[[1]][tot + 1] <<- tot + 1L
    soltbl[[2]][[tot + 1]] <<- x[-1]
    time_due <- ymd_h("2100-1-1 0", tz = "America/Chicago")
    addToRankingTbl(time = time_due)
}

#' Show a question from the Q&A test 
#' 
#' A test set will be generated at the start of each test session. This will include all questions due or past due. A randomly selected question will be shown from this set but preference will be given to 'learning' questions first followed by 'new' questions. For R questions, you can simply enter your answer (code) at the R console directly. Answers are not evaluated by the package. This is up to the user to decide whether they answered the question correctly or not.
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
    } else if(nrow(ranktbl[due <= now()]) == 0) {
        cat("Finished Quiz!!!\n\n")
        newMove()
    } else {
        ids_learning <- ranktbl[due <= now() & status == "learning", id]
        ids_new <- ranktbl[due <= now() & status == "new", id]
        if(length(ids_learning) > 1) {
            ids_learning_v <- sample(ids_learning)
        } else if(length(ids_learning) == 1) {
            ids_learning_v <- ids_learning
        } else {
            ids_learning_v <- NULL
        }
        if(length(ids_new) > 1) {
            ids_new_v <- sample(ids_new)
        } else if(length(ids_new) == 1) {
            ids_new_v <- ids_new
        } else {
            ids_new_v <- NULL
        }
        test_ids <- c(ids_learning_v, ids_new_v)
        qid <<- test_ids[1]
        timeasked <<- now()
        question <- qtbl[qid, 2]
        cat(paste(question, "\n"))
        if (grepl("img$", question)) {
            imgfile <- paste0("~/.quizme/images/", qid, "q.png")
            img <- png::readPNG(imgfile)
            grid::grid.raster(img)
    }
    }
}

#' Show answer
#' 
#' This will show the answer to the last question presented. If the answer is
#' stored in an image file then the image will be displated in the current
#' graphics window
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
    if (any(grepl("^img$", answer))) {
            imgfile <- paste0("~/.quizme/images/", qid, "a.png")
            img <- png::readPNG(imgfile)
            grid::grid.raster(img)
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
    endSession()
    show_status()
    write_rds(list(qtbl, soltbl, testlog, ranktbl, slog), "~/.quizme/quizdata")
    detach(package:lubridate)
}

#' Change question-answer
#' 
#' Change/replace the current question. First line will be the questio. Add answers in the following line(s). Carriage return in a blank line will save the question and answer to the data objects.
#' 
#' @return two objects: q_tbl and sol_tbl updated with the new question-answer.
#' 
#' @examples
#' \dontrun{changeq()}
#' 
#' @export
changeq <- function() {
    x <- scan(what = character(), sep = "\n")
    qtbl[id == qid, question := x[1]]
    soltbl[[2]][[qid]] <<- x[-1]
}

#' Test session status
#' 
#' Reports number of questions remaining
#' 
#' @return number of questions remaining
#' 
#' @examples
#' \dontrun{show_status()}
#' 
#' @export
show_status <- function() {
    cat("\nTOTAL:")
    print(table(ranktbl[, status]))
    cat("TOPICS:")
    print(table(substr(qtbl[, question], 1, 1)))
    cat("\n")
    cat("TOMORROW:")
    cat(ranktbl[due < Sys.Date() + 2 & due >= Sys.Date() + 1, .N])
    cat("\n\n")
    cat("TODAY:")
    cat(ranktbl[due < Sys.Date() + 1, .N])
    print(table(droplevels(ranktbl[due < Sys.Date() + 1, status])))
    cat("\n")
    remaining <- ranktbl[due <= now(), .N]    
    cat(paste(remaining, 'more to go...\nKeep going!\n'))
}

#' Due next 7 days
#' 
#' Reports number of questions due in the next 7 days
#' 
#' @return number of questions due in the next 7 days
#' 
#' @examples
#' \dontrun{week_ahead()}
#' 
#' @export
week_ahead <- function() {
    x <- ranktbl[due < Sys.Date() + 7]
    print(x[, due := as.Date(due)][, .N, by = due][, due := weekdays(due)][])
}

#' Delete last question
#' 
#' deletes the last added question to the database. 
#' 
#' @return updated rank table, question and answer tables
#' 
#' @examples
#' \dontrun{delete_last()}
#' 
#' @export
delete_last <- function() {
    id <- qtbl[, .N]
    qtbl <<- qtbl[!id]
    soltbl <<- lapply(soltbl, function(x) x[-id])
    ranktbl <<- ranktbl[!id]
}
