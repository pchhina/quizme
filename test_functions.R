library(stringr)
library(tibble)
library(dplyr)

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

ask <- function() {
    q_id <<- sample(1:nrow(q_tbl), 1, prob = pull(q_tbl[, 3]))
    question <- q_tbl[q_id, 2]
    cat(paste(question, "\n"))
}

tell <- function() {
    answer <- sol_tbl[[2]][[q_id]]
    for(i in seq_along(answer)) {
        cat(paste(answer[i],"\n"))
    }
    }

bye <- function() {
    write_rds(list(q_tbl, sol_tbl, data_tbl), "~/.quizme/quizdata")
    vars <- c("q_tbl", "sol_tbl", "data_tbl", "q_id", "make_quiz",
              "addq", "ask", "tell")
    rm(list = vars, pos = ".GlobalEnv")
}
