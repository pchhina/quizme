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

# this function update the due date of past due questions to today
# this is needed for new questions that are many days past due
# so that if they are practiced today, they are updated to learning tomorrow
# and not some day in the past
mvPastdueToToday <- function(DT) {
    nowt  <- now()
    hour(nowt)  <- 0
    minute(nowt)  <- 0
    second(nowt)  <- 0
    DT[due < today(), due := nowt]
}
