% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_functions.R
\name{quizme}
\alias{quizme}
\title{Load quiz data into R environment}
\usage{
quizme()
}
\value{
three objects: q_tbl, sol_tbl and data_tbl containing questions, answers and metadata
}
\description{
If using for the first time, creates .quizme directory in users home directory for data storage. Also creates empty objects for storing question/answer data.
}
\examples{
\dontrun{quizme()}

}
