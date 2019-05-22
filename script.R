library(caret)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
edx <- readRDS("C:/R/Rproject/movielens/edx.rds")
validation <- readRDS("C:/R/Rproject/movielens/validation.rds")

str(edx)
