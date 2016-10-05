library(recommenderlab)
data(Jester5k, package = "recommenderlab")

Jester5k_rec <- Jester5k[100 - rowCounts(Jester5k) > 20]

Jester5k_rec <- normalize(Jester5k_rec)
train_test <- evaluationScheme(data = Jester5k_rec, method = "split", train = 0.8, given = 20, goodRating = 0.1)

models <- list(
  UBCF_cos = list(name = "UBCF", param = list(method = "cosine")), 
  IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
  UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
  IBCF_cor = list(name = "IBCF", param = list(method = "pearson")))

eval_results <- evaluate(x = train_test, method = models, n = 1:20)

model_performance <- lapply(eval_results, avg)

model_performance$IBCF[, 5:8]
model_performance$UBCF[, 5:8]

par(mfrow = c(2, 1))

plot(eval_results)
plot(eval_results, "prec/rec")

par(mfrow = c(1, 1))

library(ggplot2)
model_gg <- data.frame(rbind(model_performance$IBCF[, 5:8], model_performance$UBCF[, 5:8]))
model_gg$Model <- as.factor(c(rep("IBCF", 20), rep("UBCF", 20)))
ggplot(model_gg, aes(x = recall, y = precision, col = Model)) + geom_line() + geom_point() + theme(legend.position = "bottom") + ggtitle("Precision-Recall Curves\n")
ggplot(model_gg, aes(x = FPR, y = TPR, col = Model)) + geom_line() + geom_point() + theme(legend.position = "bottom") + ggtitle("ROC Curves\n")

library(tidyr)
tidy_models <- gather(model_gg, Metric, Value, 2:5)
tidy_models$Curve <- ifelse(tidy_models$Metric == "TPR" | tidy_models$Metric == "FPR", "ROC", "Precision-Recall")
tidy_models$Metric <- ifelse(tidy_models$Metric == "FPR" | tidy_models$Metric == "recall", "x", "y")
tidy_models <- spread(tidy_models, Metric, Value)
ggplot(tidy_models, aes(x = x, y = y, col = Model)) + geom_line() + geom_point() + facet_wrap(~ Curve, scales = "free") + theme(legend.position = 'bottom') + xlab("") + ylab("")
