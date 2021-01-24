
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) 
  install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")


# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Create train and test sets from the edx set with the ratio 80/20

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set

test <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# check edx for NA values
sum(is.na(edx))

# check validation for NA values
sum(is.na(validation))

# check first 5 rows of the data set
head(edx, 5) %>% 
  knitr::kable()

# check detentions of edx
dim(edx)

# check detentions of validation
dim(validation)

# check number unique users and unique movies
edx %>% 
  summarize(unique_users = n_distinct(userId),
            unique_movies = n_distinct(movieId))


top_movies <- edx %>%
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)

random_rankings <- edx %>%
  filter(userId %in% sample(unique(edx$userId), 20)) %>% 
  filter(movieId %in% top_movies) %>% 
  select(userId, title, rating) %>% 
  spread(title, rating)

random_rankings %>% knitr::kable()

# plot the sparse matrix
users <- sample(unique(edx$userId), 100) 
rafalib::mypar()
edx %>% filter(userId %in% users) %>%
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

# plot distribution of movies
edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, fill="steelblue", color = "black") + 
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Movies")

# display 20 movies with the lowest count of ratings
edx %>%
  group_by(title) %>%
  summarise(count = n(), rating = mean(rating)) %>%
  filter(count < 2) %>%
  arrange(desc(rating)) %>%
  slice(1:20) %>%
  knitr::kable()

# display 20 movies with the highest count of ratings
edx %>%
  group_by(title) %>%
  summarise(count = n(), rating = mean(rating)) %>%
  filter(count > 10000) %>%
  arrange(desc(rating)) %>%
  slice(1:20) %>%
  knitr::kable()

#  plot distribution of users
edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, fill="steelblue", color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

# plot distribution of ratings
ggplot(edx, aes(x = factor(rating))) +
  geom_bar(width=0.8, fill="steelblue", color = "black") +
  theme_minimal() +
  ggtitle("Ratings")

# plot average rating by movie
edx %>%
  group_by(movieId) %>%
  summarise(avg = mean(rating)) %>%
  ggplot(aes(x=avg)) +
  geom_histogram(bins = 10, fill="steelblue", color = "black") +
  theme_minimal() +
  ggtitle("Avg. Movie Ratings")

# plot average rating by users
edx %>%
  group_by(userId) %>%
  summarise(avg = mean(rating)) %>%
  ggplot(aes(x=avg)) +
  geom_histogram(bins = 30, fill="steelblue", color = "black") +
  theme_minimal() +
  ggtitle("Avg. User Ratings")


# plot average rating by genres
edx %>%
  group_by(genres) %>%
  summarise(count=n(), avg_rating = mean(rating)) %>%
  filter(count >= 35000) %>%
  mutate(genres= reorder(genres, avg_rating)) %>%
  ggplot(aes(x=genres, y=avg_rating)) +
  geom_point(size = 2, shape = 23) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Avg. Genre Ratings")

# create RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

###########
# Model 1 #
###########

mu <- mean(train$rating) 
mu

rmse_base <- RMSE(test$rating, mu) 
rmse_base

rmse_results <- data.frame(method = "Base(Naive) Model", RMSE = rmse_base)
rmse_results %>% knitr::kable()

###########
# Model 2 #
###########

# Find b_i bias for each movie
b_i <- train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# Predict movie ratings
predicted_ratings <- mu + test %>% 
  left_join(b_i, by='movieId') %>%
  pull(b_i)
# Find RMSE
rmse_movie <- RMSE(predicted_ratings, test$rating)
rmse_movie

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effects Model",  
                                     RMSE = rmse_movie))
rmse_results %>% knitr::kable()

###########
# Model 3 #
###########


# Find b_u bias for each user
b_u <- train %>%
  left_join(b_i, by="movieId") %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/n())

# Predict movie ratings
predicted_ratings <- test %>%
  left_join(b_i, by = "movieId") %>% 
  left_join(b_u, by = "userId") %>% 
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Find RMSE
rmse_user <- RMSE(predicted_ratings, test$rating)
rmse_user

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and User Effects Model",  
                                     RMSE = rmse_user))
rmse_results %>% knitr::kable()

###########
# Model 4 #
###########

# Find b_u bias for each user
b_k <- train %>%
  left_join(b_u, by="userId") %>%
  left_join(b_i, by = "movieId") %>% 
  group_by(genres) %>% 
  summarise(b_k = (sum(rating - b_i - b_u - mu))/(n()))

# Predicting movie ratings on test set
predicted_ratings <- test %>% 
  left_join(b_i, by = "movieId") %>% 
  left_join(b_u, by = "userId") %>%
  left_join(b_k, by="genres") %>% 
  mutate(b_k=replace_na(b_k,0)) %>%
  mutate(pred = mu + b_i + b_u + b_k) %>%
  pull(pred)


# Find RMSE
rmse_genre <- RMSE(predicted_ratings, test$rating)
rmse_genre

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie, User and Genre Effects Model",  
                                     RMSE = rmse_genre))
rmse_results %>% knitr::kable()


###########
# Model 5 #
###########

# lambda sequence from 0 to 6 with 0.25 step.
lambdas <- seq(0, 6, 0.25)
# sapply takes values from lambdas and uses them in the function to 
# find all possible RMSE values. 
rmses <- sapply(lambdas, function(l){
  
  b_i <- train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test$rating))
})

qplot(lambdas, rmses) 


# find the minimum value of lambda
lambda <- lambdas[which.min(rmses)] 
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie and User Effects Model",
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

###########
# Model 6 #
###########

# lambda sequence from 0 to 6 with 0.25 step.
lambdas <- seq(0, 6, 0.25)
# sapply takes values from lambdas and uses them in the function to 
# find all possible RMSE values. 
rmses <- sapply(lambdas, function(l){
  
  b_i <- train %>% 
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_k <- train %>%
    left_join(b_u, by="userId") %>%
    left_join(b_i, by = "movieId") %>% 
    group_by(genres) %>% 
    summarise(b_k = (sum(rating - b_i - b_u - mu))/(n()+l))
  
  predicted_ratings <- test %>% 
    left_join(b_i, by = "movieId") %>% 
    left_join(b_u, by = "userId") %>%
    left_join(b_k, by="genres") %>% 
    mutate(b_k=replace_na(b_k,0)) %>%
    mutate(pred = mu + b_i + b_u + b_k) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test$rating))
})

qplot(lambdas, rmses) 

# find the minimum value of lambda
lambda <- lambdas[which.min(rmses)] 
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie, User and Genre Effects Model",
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

####################
# Model Validation #
####################

mu_evl <- mean(edx$rating)

b_i <- validation %>% 
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu_evl)/(n()+lambda))

b_u <- validation %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - b_i - mu_evl)/(n()+lambda))

b_k <- validation %>%
  left_join(b_u, by="userId") %>%
  left_join(b_i, by = "movieId") %>% 
  group_by(genres) %>% 
  summarise(b_k = (sum(rating - b_i - b_u - mu_evl))/(n()+lambda))

predicted_ratings <- validation %>% 
  left_join(b_i, by = "movieId") %>% 
  left_join(b_u, by = "userId") %>%
  left_join(b_k, by = "genres") %>% 
  mutate(b_k=replace_na(b_k,0)) %>%
  mutate(pred = mu + b_i + b_u + b_k) %>%
  pull(pred)

model_val <- RMSE(validation$rating, predicted_ratings)

model_val




