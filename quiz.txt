#1 variable names in iris data
iris %>% names()

#2 mean sepal length of each of the species in iris data
iris %>% group_by(Species) %>%
    summarize(mean_sepal_length = mean(Sepal.Length))

#3 scatterplot of highway fuel economy versus displacement from mpg data
library(ggplot2)
mpg %>% ggplot(aes(displ, hwy)) +
    geom_point()

#4 list all datasets in installed packages
data()

#5 sample 10 random observations from mpg data
library(dplyr)
mpg %>% sample_n(10)
