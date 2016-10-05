start_manual <- Sys.time()
# load data ####
library(recommenderlab)
data(Jester5k)
jester <- as.vector(Jester5k@data)
jester <- matrix(data = jester, nrow = Jester5k@data@Dim[1], ncol = Jester5k@data@Dim[2])
jester <- as.data.frame(jester)
rownames(jester) <- Jester5k@data@Dimnames[[1]]
names(jester) <- Jester5k@data@Dimnames[[2]]


# mean-centering ####
jester[jester == 0] <- NA
user_means <- rowMeans(jester, na.rm = TRUE)
for (i in 1:nrow(jester)) {
  jester[i, ] <- jester[i, ] - user_means[i]
}

#similarity function ####
vec_similarity <- function(v1, v2) {
  dot_product <- sum(v1 * v2, na.rm = TRUE)
  norm1 <- sqrt(sum(v1^2, na.rm = TRUE))
  norm2 <- sqrt(sum(v2^2, na.rm = TRUE))
  dot_product / (norm1 * norm2)
}

#item-based -- cosine

# similarity matrix ####
item_sim <- data.frame(matrix(NA, nrow = ncol(jester), ncol = ncol(jester)))
for (i in 1:nrow(item_sim)) {
  for(j in 1:ncol(item_sim)) {
    item_sim[i, j] <- vec_similarity(jester[, i], jester[, j])
  }
}
names(item_sim) <- names(jester) -> rownames(item_sim)
image(as.matrix(item_sim), axes = FALSE)


# run model ####
#only those with sufficient un-rated jokes
jester_rec <- subset(jester, rowSums(is.na(jester)) > 20)

#find recs
library(dplyr)

jester_rec <- subset(jester, rowSums(is.na(jester)) > 20)

manual_recs <- data.frame(matrix(nrow = 0, ncol = 5))
for (i in 1:20) {
  joke_name <- character(0)
  joke_guess <- numeric(0)
  for (j in 1:ncol(jester_rec)){
    if(is.na(jester_rec[i, j])) {
      joke_name <- c(joke_name, names(item_sim)[j])
      # locate max similarities for item
      sims <- item_sim[, j]
      sims_df <- data.frame(joke = names(item_sim), sims, stringsAsFactors = FALSE)
      sims_df <- sims_df %>% filter(joke != names(item_sim)[j]) %>% arrange(desc(sims)) %>% top_n(20)
      # get predicted rating
      pred <- NULL
      for (k in 1:20) {
        pred <- sum(pred, sims_df$sims[k] * jester_rec[i, sims_df$joke[k]], na.rm = TRUE)
      }
      joke_guess <- c(joke_guess, pred / sum(sims_df$sims, na.rm = TRUE))
    }
  }
  best_jokes <- data.frame(joke_name, joke_guess, stringsAsFactors = FALSE)
  best_jokes <- best_jokes %>% arrange(desc(joke_guess))
  for (n in 1:5) {
    manual_recs[i, n] <- best_jokes$joke_name[n]
  }
  rownames(manual_recs)[i] <- rownames(jester_rec)[i]
}
names(manual_recs) <- as.character(1:5)

stop_manual <- Sys.time()

duration_manual <- stop_manual - start_manual

start_premade <- Sys.time()

# package model ####
premade_model <- Recommender(data = Jester5k, method = "IBCF", parameter = list(method = "Cosine", k = 20))
Jester5k_rec <- Jester5k[100 - rowCounts(Jester5k) > 20]
premade_allrecs <- predict(object = premade_model, newdata = Jester5k_rec, n = 5)
premade_recs <- data.frame(matrix(nrow = 20, ncol = 5))
rownames(premade_recs) <- names(premade_allrecs@items)[1:20]

for(i in 1:20) {
  for (j in 1:5) {
    premade_recs[i, j] <- paste0("j", premade_allrecs@items[[i]][j])
  }
}
names(premade_recs) <- as.character(1:5)

stop_premade <- Sys.time()

duration_premade <- stop_premade - start_premade