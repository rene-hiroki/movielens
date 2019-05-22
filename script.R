library(caret)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
edx <- readRDS("C:/R/Rproject/movielens/edx.rds")
validation <- readRDS("C:/R/Rproject/movielens/validation.rds")

str(edx)

# Quiz --------------------------------------------------------------------


dim(edx)  # dimention of edx data set
edx %>% filter(rating == 0) %>% tally() 
edx %>% filter(rating == 3) %>% tally()
table(edx$rating) # count each rating


# number of movies and users
edx$movieId %>% as_tibble() %>%  distinct() %>% nrow()
n_distinct(edx$movieId)
edx$userId %>% as_tibble() %>%  distinct() %>% nrow()
n_distinct(edx$userId)

# how many movies in these genres
get_genre <- c("Drama", "Comedy", "Thriller", "Romance")
edx %>% separate_rows(genres) %>% 
    filter(genres %in% get_genre) %>%
    group_by(genres) %>% 
    tally()
    
edx %>% separate_rows(genres, sep = "\\|") %>%
    group_by(genres) %>%
    summarize(count = n()) %>%
    arrange(desc(count))

?separate_rows
as_tibble(edx)


# Which movie has the greatest number of ratings?
str(edx)
edx %>% group_by(movieId) %>% 
    summarise(n = n()) %>% 
    right_join(edx, by = "movieId") %>% 
    distinct(movieId, .keep_all = TRUE) %>% 
    arrange(desc(n))

edx %>% group_by(movieId, title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))

# What are the five most given ratings in order from most to least?
edx %>% group_by(rating) %>% summarize(count = n()) %>% 
    arrange(desc(rating))  

edx %>%
    group_by(rating) %>%
    summarize(count = n()) %>%
    ggplot(aes(x = rating, y = count)) +
    geom_line()
