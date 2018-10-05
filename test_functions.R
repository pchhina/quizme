library(stringr)

make_quiz <- function(quizfile = "quiz.txt") {
    reposol <- readLines(quizfile)
    test <- reposol[str_detect(reposol, "^#")]
}

ask <- function() {
    question <- test[sample(1:length(test), 1)]
    cat(paste(question, "\n"))
}

tell <- function(n) {
    v1 <- which(str_detect(reposol, paste0("#",n, "\\s")))
    v2 <- which(str_detect(reposol, paste0("#",n+1, "\\s")))
    answer <- reposol[(v1+1):(v2-1)]
    for(i in seq_along(answer)) {
        cat(paste(answer[i],"\n"))
    }
    }
