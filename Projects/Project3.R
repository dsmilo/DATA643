library(recommenderlab)
data(Jester5k, package = "recommenderlab")

# users####
Jester5k_norm <- normalize(Jester5k)
jester_svd <- svd(Jester5k_norm@data)
Sigmak <- jester_svd$d; Uk <- jester_svd$u; Vk <- t(as.matrix(jester_svd$v))

norm_Sigma <- sqrt(sum(Sigma^2, na.rm = TRUE))
frac_norm <- NULL

for (i in 1:length(Sigma)) {
  frac_norm[i] <- sqrt(sum(Sigma[1:i]^2, na.rm = TRUE)) / norm_Sigma
}

library(ggplot2)
qplot(x = 1:100, y = frac_norm, geom = "line") + 
  geom_hline(yintercept = 0.8, lty = 2, col = 'green4') + 
  geom_vline(xintercept = min(which(x = frac_norm > 0.8)), lty = 3, col = "green4") + 
  geom_hline(yintercept = 0.9, lty = 2, col = 'red4') + 
  geom_vline(xintercept = min(which(x = frac_norm > 0.9)), lty = 3, col = "red4") + 
  scale_x_continuous('') + scale_y_continuous('') + theme_bw() + ggtitle(expression(Sigma))

k <- min(which(x = frac_norm > 0.8))

Sigmak <- Diagonal(x = Sigmak[1:k])
Uk <- Uk[, 1:k]
Vk <- Vk[, 1:k]

R <- Uk %*% t(sqrt(Diagonal(x = Sigma))) %*% sqrt(Diagonal(x = Sigma)) %*% t(Vk)
R@Dimnames <- Jester5k@data@Dimnames

R <- as(R, "matrix")

jester_matrix <- matrix(as.vector(Jester5k@data), nrow = Jester5k@data@Dim[1], ncol = Jester5k@data@Dim[2])

for (i in 1:nrow(R)) {
  for (j in 1:ncol(R)) {
    R[i, j] <- ifelse(jester_matrix[i, j] == 0, NA, R[i,j])
    if (R[i, j] < -10) {R[i, j] <- -10}
    if (R[i, j] > 10) {R[i, j] <- 10}
  }
}



library(reshape2)
ggplot(melt(R), aes(Var1, Var2, fill = value)) + geom_raster() + scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF"), name = 'Rating') + scale_x_discrete("Users", breaks = NULL, labels = NULL) + scale_y_discrete("Items", breaks = NULL, labels = NULL) + theme(legend.position = 'bottom') + ggtitle('Approximated Ratings')

ggplot(melt(jester_matrix), aes(Var1, Var2, fill = value)) + geom_raster() + scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF"), name = 'Rating') + scale_x_discrete("Users", breaks = NULL, labels = NULL) + scale_y_discrete("Items", breaks = NULL, labels = NULL) + theme(legend.position = 'bottom') + ggtitle('Actual Ratings')


R <- as(R, "realRatingMatrix")
calcPredictionAccuracy(x = R, data = Jester5k, byUser = FALSE)

#######

eval_sets <- evaluationScheme(data = Jester5k,
                              method = "cross-validation",
                              k = 4,
                              given = 15,
                              goodRating = 3)

eval_recommender <- Recommender(data = getData(eval_sets, "train"),
                                method = "UBCF",
                                parameter = NULL)

eval_prediction <- predict(object = eval_recommender,
                           newdata = getData(eval_sets, "known"),
                           n = 10,
                           type = "ratings")

calcPredictionAccuracy(
  x = eval_prediction,
  data = getData(eval_sets, "unknown"),
  byUser = FALSE)

# text####
library(tm)
joke_corpus <- Corpus(VectorSource(JesterJokes[1]))

for(i in 2:ncol(Jester5k)) {
  tmp_corpus <- Corpus(VectorSource(JesterJokes[i]))
  joke_corpus <- c(joke_corpus, tmp_corpus)
}

dtm <- DocumentTermMatrix(joke_corpus, 
                          control = list(removePunctuation = TRUE,
                                         removeNumbers = TRUE,
                                         stopwords = TRUE,
                                         tolower = TRUE,
                                         weighting = weightTfIdf))

dtm_matrix <- as.matrix(dtm)
dimnames(dtm_matrix)$Docs <- Jester5k@data@Dimnames[[2]]

term_svd <- svd(dtm_matrix)

Sigmak <- term_svd$d; Uk <- term_svd$u; Vk <- t(as.matrix(term_svd$v))
term_norm <- sqrt(sum(Sigma^2, na.rm = TRUE))

frac_norm <- NULL

for (i in 1:length(Sigma)) {
  frac_norm[i] <- sqrt(sum(Sigma[1:i]^2, na.rm = TRUE)) / term_norm
}
  
# plot(frac_norm, type = "l")
# abline(h = 0.9, lty = 2)
# abline(v = min(which(x = frac_norm > 0.9)), lty = 2)
# abline(h = 0.8, lty = 3)
# abline(v = min(which(x = frac_norm > 0.8)), lty = 3)

k <- min(which(x = frac_norm > 0.8))

Sigma <- Diagonal(x = Sigma[1:k])
Uk <- Uk[, 1:k]
Vk <- Vk[, 1:k]

joke_cat <- as.matrix(Uk)
dimnames(joke_cat) <- list(Jokes = paste0("j", 1:100), SVs = paste0("sv", 1:33))
