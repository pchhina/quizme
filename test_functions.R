library(stringr)

# read the repo text file
repo <- readLines("test.R")

# read solutions
reposol <- readLines("test_file.R")

ask <- function() {
    question <- repo[sample(1:length(repo), 1)]
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
