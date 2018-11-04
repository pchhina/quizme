utils::globalVariables(c("qtbl", "soltbl", "testlog", "ranktbl", "<<-"), add = TRUE)

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

#' update time
#' 
#' @importFrom lubridate today date now
#'
updatetime <- function() {
                if (ranktbl[id == qid, status] == "new") {
                ncor <- testlog[id == qid &
                    response == "yes", .N]
                    if (ncor <= 1) {
                        t <- 0
                        updaterank()
                    } else if (ncor == 2) {
                        t <- hour(now()) + 4
                    } else {
                        t <- 24
                        ranktbl[id == qid, status := "learning"]
                        new2learn <<- new2learn + 1
                    }
                } else {
                    tlast <- days_elapsed(qid = qid)
                    t <- t_learning(tlast = tlast, qid = qid)
                }
    newdew <- ranktbl[id == qid, due]
    hour(newdew)  <- t
    ranktbl[id == qid, due := newdew] # add t hours to current time
    updateranktbl()
}

#' Number of days elapsed
#' 
#' Finds number of days elapsed between the last two askings of qid
#' 
#' @importFrom lubridate day interval
#'
days_elapsed <- function(qid) {
    timevec <- testlog[id == qid, time]
    t2 <- timevec[length(timevec)]
    t1 <- timevec[length(timevec) - 1]
    int_t1t2 <- interval(t1, t2)
    round(as.numeric(int_t1t2)/(60*60*24))
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

# this is called on a missed question
# missed question is assigned the largest rank
# so it goes at the bottom within the same subgroup
updaterank <- function() {
    maxrank <- ranktbl[, max(rank)]
    ranktbl[id == qid, rank := maxrank + 1]
    updateranktbl()
}

# NOTE: this is obsolete now that ask is sampling the test group
# this functions shuffle rankings within a subgroup
# this is done to change the order in which the questins are asked in the later session
# this will be called in a call to bye
shuffleWithinGroup <- function() {
    ranktbl[, rank := as.double(sample(.N)), by = .(due, status)]
    # here the type at LHS and RHS of := must match 
    # i am coercing RHS to double
    # but may be it is better to coerce rank as ingeger?
    updateranktbl()
}

# this reorders the ranking table if anything changes in the rankingtbl
# this includes almost all calls - addq, shuffleWithinGroup, 
updateranktbl <- function() {
    ranktbl <<- ranktbl[order(due, status, rank)]
}


#'
#' move past due questions to today
#'
#' importFrom lubridate now today hour minute second
#'
#' this function update the due date of past due questions to today
#' this is needed for new questions that are many days past due
#' so that if they are practiced today, they are updated to learning tomorrow
#' and not some day in the past
mvPastdueToToday <- function(DT) {
    nowt  <- now()
    hour(nowt)  <- 0
    minute(nowt)  <- 0
    second(nowt)  <- 0
    DT[due < today(), due := nowt]
}

startSession <- function() {
    sid <- nrow(slog) + 1L
    sdate <- today()
    sstart <- now()
    send <- now()
    sdur <- round(as.numeric(interval(sstart, send)) / 60)
    sallq <- 0L
    snewq <- 0L
    snew2learningq <- 0L
    data.table(sid = sid,
               sdate = sdate,
               sstart = sstart,
               sdur = sdur,
               sallq = sallq,
               snewq = snewq,
               snew2learningq = snew2learningq)
}

endSession <- function() {
    sstart <- slog[sid == nrow(slog), sstart]
    send <- now()
    sdur <- round(as.numeric(interval(sstart, send)) / 60)
    slog[sid == nrow(slog), snew2learningq := new2learn]
}

newActive <- function() {
    time_due <- ymd_h("2100-1-1 0", tz = "America/Chicago")
    newq <- ranktbl[due < time_due & status == 'new', .N]
    newq + new2learn
}

newParked <- function() {
    time_due <- ymd_h("2100-1-1 0", tz = "America/Chicago")
    ranktbl[due == time_due & status == 'new', .N]
}

newLearned <- function() {
    slog[sdate == today(), sum(snew2learningq)]
}

newDelta <- function() {
    min(c(5, newParked(), 6 - newActive() - newLearned()))
}

learningToday <- function() {
    ranktbl[due < today() + 1, .N]
}

learningTomorrow <- function() {
    ranktbl[due <= today() + 1, .N] - learningToday()
}

newMove <- function() {
    np <- newParked()
    nd <- newDelta()
    tdy <- learningToday()
    tmr <- learningTomorrow()
    if(nd <= 0) {
        cat("You are learning:\n\n")
        cat(paste(newActive(), "new question(s)\n"))
        cat(paste(tdy, "total question(s) today\n"))
        cat(paste(tmr, "question(s) tomorrow\n\n"))
        cat("if you wish to add more...\n")
        input <- readline(prompt = "please enter number of questions to add (0 to exit): ")
        ifelse(input == 0, return(cat("\n")), nd <- input)

    }
    if(np == 0) {
        cat('\nthere are no new questions left in the repository\n')
        cat('you may add questions to repository using addq() function\n')
        return(cat('\n'))
    }
    if(np < nd) {
        cat(paste("\nonly", np, "question(s) remaining in repository\n"))
        cat(paste("adding", np, "question(s) to your test...\n"))
        nd <- np
    } else {
        cat(paste("adding", nd, "question(s) to your test...\n"))
    }
    time_due <- ymd_h("2100-1-1 0", tz = "America/Chicago")
    current_date <- now()
    hour(current_date) <- 0
    minute(current_date) <- 0
    second(current_date) <- 0
    qidnew <- ranktbl[due == time_due & status == "new", id]
        if(length(qidnew) > 1) {
            qidadd <- sample(qidnew, nd)
        } else if(length(qidnew) == 1) {
            qidadd <- qidnew
        } else {
            qidadd <- NULL
        }
    ranktbl[id %in% qidadd, due := current_date]
    updateranktbl()
    cat(paste(nd, "questions added to your test!\n"))
}
