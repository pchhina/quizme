library(stringr)

# read qa.txt
reposol <- readLines("qa.txt")

# create questions repo
questions <- reposol[str_detect(reposol, "^#")]

ask <- function() {
    question <- questions[sample(1:length(questions), 1)]
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
