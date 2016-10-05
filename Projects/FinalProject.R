set.seed(getOption("letter_seed"))
name_sub <- base::sample(LETTERS, 26)

anonymize <- function(old_vector) {
  library(stringr)
  new_vector <- character(0)
  for (i in 1:length(old_vector)) {
    new_nm <- NULL
    for (j in 1:str_length(old_vector[i])) {
      old_ltr <- substr(old_vector[i], j, j)
      new_ltr <- name_sub[which(LETTERS == old_ltr)]
      new_nm <- paste0(new_nm, new_ltr)
    }
    new_vector <- c(new_vector, new_nm)
  }
  new_vector
}

# ------------------------------------------------------
all_projects <- read.csv('C:/Users/Dan/Dropbox/Work/Database/AllProjects.csv', stringsAsFactors = FALSE)

all_projects$strCompanyID <- anonymize(str_trim(all_projects$strCompanyID))
all_projects$strProjectCategory <- as.factor(all_projects$strProjectCategory)
all_projects$Physical_Zip_Code <- str_sub(all_projects$Physical_Zip_Code, 1, 5)

all_projects <- subset(all_projects, strCustomerType != "NYPA" & Physical_Zip_Code != "" & Physical_Zip_Code != "11057", c(strCompanyID, Physical_Zip_Code, strProjectCategory, strCustomerType))

library(jsonlite)
projects_json <- toJSON(all_projects)
write(projects_json, "all_projects.json")

# ------------------------------------------------------

Sys.setenv(SPARK_HOME = "C:/Apache/Spark-1.6.2")
.libPaths(c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib"), .libPaths()))
library(SparkR)
sc <- sparkR.init(master = "local")
sqlContext <- sparkRSQL.init(sc)

projects_json <- toJSON(all_projects)
write(projects_json, "all_projects.json")

spark_projects <- read.df(sqlContext, path = "./all_projects.json", source = "json")
spark_projects

# ------------------------------------------------------

ratingtable <- crosstab(spark_projects, "strCompanyID", "strProjectCategory")

rownames(ratingtable) <- ratingtable[, 1]
ratingtable <- ratingtable %>% dplyr::select(-strCompanyID_strProjectCategory)

ratingtable <- as.matrix(ratingtable)

ratingtable[which(ratingtable == 0)] <- NA

#library(recommenderlab)
#projectratings <- as(ratingtable, "realRatingMatrix")
my_theme <- theme(legend.position = 'bottom', plot.title = element_text(lineheight=.8, face="bold"))
library(reshape)
library(ggplot2)
ggplot(melt(ratingtable), aes(X1, X2, fill = value)) + geom_raster() + scale_x_discrete("Users", breaks = NULL, labels = NULL) + scale_y_discrete("Items", breaks = NULL, labels = NULL) + theme(legend.position = 'bottom') + scale_fill_continuous(na.value = "white", low = "blue", high = "red", guide = "colourbar")

# ------------------------------------------------------
vec_similarity <- function(v1, v2) {
  dot_product <- sum(v1 * v2, na.rm = TRUE)
  norm1 <- sqrt(sum(v1^2, na.rm = TRUE))
  norm2 <- sqrt(sum(v2^2, na.rm = TRUE))
  dot_product / (norm1 * norm2)
}

user_sim <- matrix(NA, nrow = nrow(ratingtable), ncol = nrow(ratingtable))
for (i in 1:nrow(user_sim)) {
  for(j in 1:ncol(user_sim)) {
    user_sim[i, j] <- vec_similarity(ratingtable[i, ], ratingtable[j, ])
  }
}
rownames(user_sim) <- row.names(ratingtable) -> colnames(user_sim)


# ------------------------------------------------------

get.zip_info <- function(zip_code) {
  username <- getOption('geonamesUsername')
  api_query <- paste0('http://api.geonames.org/postalCodeSearchJSON?country=US&username=', username, '&postalcode=', zip_code)
  zip_info <- fromJSON(api_query)$postalCodes
  zip_info
}

registerTempTable(spark_projects, "all_projects")
zip <- sql(sqlContext, "SELECT DISTINCT Physical_Zip_Code FROM all_projects")
zip <- as.vector(collect(zip))

lat <- NULL; lng <- NULL

for (i in 1:length(zip)) {
  zip_data <- get.zip_info(zip[i])
  lat <- c(lat, zip_data$lat)
  lng <- c(lng, zip_data$lng)
}

zip_coords <- data.frame(zip, lat, lng, stringsAsFactors = FALSE)

dist_mile <- function(lat1, lng1, lat2, lng2) {
  circ_eq <- 24902.461
  circ_md <- 24859.730
  diff_lat <- (lat2 - lat1) / 360 * circ_md
  diff_lng <- (lng2 - lng1) / 360 * circ_eq
  diff_dist <- sqrt(diff_lat^2 + diff_lng^2)
  diff_dist
}

zip_dist <- matrix(NA, nrow = nrow(zip_coords), ncol = nrow(zip_coords), dimnames = list(zip_coords$zip, zip_coords$zip))

for (i in 1:nrow(zip_dist)) {
  for (j in 1:i) {
    zip_dist[i, j] <- dist_mile(zip_coords[which(zip_coords$zip == rownames(zip_dist)[i]), 2],
                                zip_coords[which(zip_coords$zip == rownames(zip_dist)[i]), 3],
                                zip_coords[which(zip_coords$zip == colnames(zip_dist)[j]), 2],
                                zip_coords[which(zip_coords$zip == colnames(zip_dist)[j]), 3])
    zip_dist[j, i] <- zip_dist[i, j]
  }
}


dist_sim <- matrix(nrow = nrow(ratingtable), ncol = nrow(ratingtable), dimnames = dimnames(user_sim))

company_zips <- sql(sqlContext, "SELECT strCompanyID, Physical_Zip_Code from all_projects")
company_zips <- collect(company_zips)

for (i in 1:nrow(dist_sim)) {
  for (j in 1:i) {
    dist_sim[i, j] <- zip_dist[which(rownames(zip_dist) == 
                                       company_zips[min(which(company_zips$strCompanyID 
                                                              == rownames(dist_sim)[i])),2]), 
                               which(colnames(zip_dist) == 
                                       company_zips[min(which(company_zips$strCompanyID
                                                              == rownames(dist_sim)[j])),2])]
    dist_sim[j, i] <- dist_sim[i, j]
  }
}

dist_sim <- (dist_sim - min(dist_sim))/(max(dist_sim) - min(dist_sim))
dist_sim <- 1 / (1 + dist_sim)^2


# ------------------------------------------------------
context_sim <- user_sim * dist_sim

sims <- data.frame(x = c(as.vector(user_sim), as.vector(dist_sim), as.vector(context_sim)), sim = c(rep("User", length(as.vector(user_sim))), rep("Location", length(as.vector(dist_sim))), rep("Context", length(as.vector(context_sim)))), row.names = NULL, stringsAsFactors = FALSE)

sims$sim <- factor(sims$sim, levels = c("User", "Location", "Context"))

library(scales)
ggplot(sims, aes(x = x, y = 3 * (..count..)/sum(..count..), fill = sim, col = sim)) + geom_histogram(alpha = 0.75, binwidth = 0.05, show.legend = FALSE) + scale_y_continuous('', labels=percent) + facet_wrap(~sim, nrow = 1, scales = "free_y") + scale_fill_hue(h.start = -120) + scale_color_hue(h.start = -120) + scale_x_continuous('Similarity') + ggtitle("Distribution of Similarity Matrix Values\n") + theme(legend.position = 'none', plot.title = element_text(lineheight=.8, face="bold"))

# ------------------------------------------------------
