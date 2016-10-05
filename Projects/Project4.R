library(recommenderlab)

# items & time ####
data(MovieLense)
ratings_movies <- MovieLense[rowCounts(MovieLense) > 20, colCounts(MovieLense) > 50]

which_set <- sample(x = 1:5,
                    size = nrow(ratings_movies),
                    replace = TRUE)
for(i_model in 1:5) {
  which_train <- which_set == i_model
  recc_data_train <- ratings_movies[which_train, ]
  recc_data_test <- ratings_movies[!which_train, ]
}

recc_model <- Recommender(data = recc_data_train, method = "IBCF")

recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = 20)

year_ref <- max(MovieLenseMeta$year, na.rm = TRUE)
year_diff <- year_ref - MovieLenseMeta$year
year_wt <- 1 / log(year_diff^2 + exp(1))
year_wt[is.na(year_wt)] <- 0
weights <- data.frame(title = MovieLenseMeta$title, wt = year_wt, stringsAsFactors = FALSE)

recc_df <- data.frame(user = sort(rep(1:length(recc_predicted@items), recc_predicted@n)), rating = unlist(recc_predicted@ratings), index = unlist(recc_predicted@items))
recc_df$title <- recc_predicted@itemLabels[recc_df$index]

recc_df2 <- recc_df %>% group_by(user) %>% arrange(desc(rating)) %>% top_n(5) %>% select(user, title, rating)

head(recc_df2, 10)

library(dplyr)
recc_wt <- inner_join(recc_df, weights, by = "title")

recc_wt <- recc_wt %>% mutate(wt_rating = rating * wt) %>% select(user, title, wt_rating) %>% group_by(user) %>% top_n(5)

head(recc_wt, 10)


# users & location ####
#http://files.grouplens.org/datasets/movielens/ml-100k-README.txt
movielens_ratings <- read.table('http://files.grouplens.org/datasets/movielens/ml-100k/u.data', header=FALSE, sep="\t")
names(movielens_ratings) <- c("user_id", "item_id", "rating", "timestamp")
movielens_matrix <- matrix(nrow = length(levels(as.factor(movielens_ratings$user_id))), ncol = length(levels(as.factor(movielens_ratings$item_id))))

for (n in 1:nrow(movielens_ratings)) {
  movielens_matrix[movielens_ratings[n, 1], movielens_ratings[n, 2]] <- movielens_ratings[n, 3]
}

movielens <- as(movielens_matrix, "realRatingMatrix")

users <- read.table('http://files.grouplens.org/datasets/movielens/ml-100k/u.user', header=FALSE, sep = "|", stringsAsFactors = FALSE)
names(users) <- c("user_id", "age", "gender", "occupation", "zip_code")

movielens_keep <- movielens[rowCounts(movielens) > 20, colCounts(movielens) > 50]

sets <- sample(x = 1:5,
               size = nrow(movielens_keep),
               replace = TRUE)
for(m in 1:5) {
  which_train <- sets == m
  data_train <- movielens_keep[which_train, ]
  data_test <- movielens_keep[!which_train, ]
}

user_model <- Recommender(data = data_train, method = "UBCF")
model_details <- getModel(user_model)
dim(model_details$sim)
