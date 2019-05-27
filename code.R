# load packages and data
library(dplyr)
library(ggplot2)
edx <- readRDS("C:/R/Rproject/movielens/edx.rds")
validation <- readRDS("C:/R/Rproject/movielens/validation.rds")

# glance at the data structure
glimpse(edx)
glimpse(validation)

# define the RMSE as this function
RMSE <- function(predicted_ratings, true_ratings){
    sqrt(mean((predicted_ratings - true_ratings)^2))
    }

# simplest model ----------------------------------------------------------


# rating distribution
edx %>% ggplot(aes(rating)) + 
    geom_histogram(bins = 10, color = I("black"))

# estimate mu_hat average rating
mu_hat <- mean(edx$rating)
mu_hat

# calcurate RMSE
naive_rmse <- RMSE(mu_hat, validation$rating)
naive_rmse

# save the result on table
rmse_results <- tibble(method = "Simplest Model", RMSE = naive_rmse)
rmse_results %>% knitr::kable()


# movie effects model -----------------------------------------------------


# estimate b_i movie effects 
mu <- mean(edx$rating) 
movie_avgs <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = mean(rating - mu))

# distribution of b_i
movie_avgs %>% ggplot(aes(b_i)) +
    geom_histogram(bins = 10, color = I("black"))
# predict rating by movie effects model
predicted_ratings <- mu + validation %>% 
    left_join(movie_avgs, by='movieId') %>%
    pull(b_i)

# calculate RMSE
RMSE(predicted_ratings, validation$rating)

# save the result on the table
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Effect Model",  
                                     RMSE = model_1_rmse))
rmse_results %>%  knitr::kable()


# movie and user effect model ---------------------------------------------


# distribution of b_u user effects
edx %>% 
    group_by(userId) %>% 
    filter(n()>=100) %>%
    summarize(b_u = mean(rating)) %>% 
    ggplot(aes(b_u)) + 
    geom_histogram(bins = 30, color = "black")
 
# estimate b_u
user_avgs <- edx %>% 
    left_join(movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_i))

# predict rating by movie and user effects model
predicted_ratings <- validation %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)

# calculate RMSE
RMSE(predicted_ratings, validation$rating)

# seve the result on the table
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse))
rmse_results %>% knitr::kable()