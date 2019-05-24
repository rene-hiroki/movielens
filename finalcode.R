library(dplyr)
library(ggplot2)
edx <- readRDS("C:/R/Rproject/movielens/edx.rds")
validation <- readRDS("C:/R/Rproject/movielens/validation.rds")

# glance at the data structure
names(edx)
str(edx)
str(validation)

# define the RMSE as this function
RMSE <- function(predicted_ratings, true_ratings){
    sqrt(mean((predicted_ratings - true_ratings)^2))
}


# simplest model ----------------------------------------------------------


# see the rating distribution
edx %>% ggplot(aes(rating)) + 
    geom_histogram(bins = 10, color = I("black"))

# estimate mu_hat
mu_hat <- mean(edx$rating)
mu_hat

# calcurate RMSE
naive_rmse <- RMSE(mu_hat, validation$rating)
naive_rmse

# save the result on table
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results %>% knitr::kable()


# movie effects model -----------------------------------------------------


# estimate b_i 
mu <- mean(edx$rating) 
movie_avgs <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = mean(rating - mu))

# see the distribution of b_i
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

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


# see the distribution of b_u
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

# predict rating
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


# Regularized movie and user effects model --------------------------------


lambda <- 2.5
mu <- mean(edx$rating)
movie_reg_avgs <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
    ggplot(aes(original, regularlized, size=sqrt(n))) + 
    geom_point(shape=1, alpha=0.5)

edx %>%
    count(movieId) %>% 
    left_join(movie_reg_avgs, by = "movieId") %>%
    left_join(movie_titles, by = "movieId") %>%
    arrange(desc(b_i)) %>% 
    select(title, b_i, n) %>% 
    slice(1:10)

edx %>%
    count(movieId) %>% 
    left_join(movie_reg_avgs, by = "movieId") %>%
    left_join(movie_titles, by="movieId") %>%
    arrange(b_i) %>% 
    select(title, b_i, n) %>% 
    slice(1:10) 

predicted_ratings <- validation %>% 
    left_join(movie_reg_avgs, by = "movieId") %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)

model_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse))
as_tibble( rmse_results )

lambdas <- seq(0, 10, 0.25)

mu <- mean(edx$rating)
just_the_sum <- edx %>% 
    group_by(movieId) %>% 
    summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
    predicted_ratings <- validation %>% 
        left_join(just_the_sum, by='movieId') %>% 
        mutate(b_i = s/(n_i+l)) %>%
        mutate(pred = mu + b_i) %>%
        pull(pred)
    return(RMSE(predicted_ratings, validation$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]
# 2.5

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
    
    mu <- mean(edx$rating)
    
    b_i <- edx %>% 
        group_by(movieId) %>%
        summarize(b_i = sum(rating - mu)/(n()+l))
    
    b_u <- edx %>% 
        left_join(b_i, by="movieId") %>%
        group_by(userId) %>%
        summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    
    predicted_ratings <- 
        validation %>% 
        left_join(b_i, by = "movieId") %>%
        left_join(b_u, by = "userId") %>%
        mutate(pred = mu + b_i + b_u) %>%
        pull(pred)
    
    return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, rmses)  
lambdas[which.min(rmses)]
# 5.25
min(rmses)

model_4_rmse <- min(rmses)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie and User Effect Model",  
                                     RMSE = model_4_rmse))
rmse_results %>% knitr::kable()
