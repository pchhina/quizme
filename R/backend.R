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
#' @importFrom lubridate today date
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
    newdew <- ranktbl[id == qid, due]
    hour(newdew)  <- t
    ranktbl[id == qid, due := newdew] # add t hours to current time
}

#' Number of days elapsed
#' 
#' Finds number of days elapsed between the last two askings of qid
#' 
#' @importFrom lubridate day
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

updaterank <- function() {
    maxrank <- ranktbl[, max(rank)]
    ranktbl[id == qid, rank := maxrank + 1]
}

updateranktbl <- function() {
    ranktbl <<- ranktbl[order(due, status, rank)]
}
