library(rvest)

washington <- read_html("https://www.washingtonian.com/2016/05/05/best-cheap-restaurants-in-washington-dc/")

names <- 
  washington %>%
  html_nodes(".styled-list a") %>%
  html_text()

# build data frame
numberToLoad <- 100
restaurants <- data.frame(matrix(nrow = length(names[1:numberToLoad])))
restaurants$Names <- names[1:numberToLoad]
restaurants[1] <- NULL

restaurants$Stars <- NA
restaurants$Ratings <- NA
restaurants$Addresses <- NA
pb <- txtProgressBar(min = 0, max = length(restaurants$Names), style=3)
for (i in 1:length(restaurants$Names)) {
  search <- read_html(paste("http://www.yelp.com/search?find_desc=", URLencode(restaurants$Names[i]), "&find_loc=Washington%2C+DC&ns=1", sep = ""))
  
  # record number of stars
  starSet <- 
    search %>%
    html_nodes(".natural-search-result .star-img") %>%
    html_attr("class")
  stars <- as.numeric(gsub("_half", ".5",
                           gsub("star-img stars_", "", starSet[1])
                           )
                      )
  restaurants$Stars[i] <- stars
  
  # record number of ratings
  ratings <- 
    search %>%
    html_node(".yloca-search-result+ .regular-search-result .rating-qualifier") %>%
    html_text()
  ratings <- gsub("\n ","",as.character(ratings))
  ratings <- gsub("reviews","",as.character(ratings))
  ratings <- as.numeric(ratings)
  restaurants$Ratings[i] <- ratings
  
  # record addresses
  addresses <- 
    search %>%
    html_nodes(".yloca-search-result+ .regular-search-result address")
  addresses <- gsub("\n", "", addresses)
  addresses <- gsub("<address>", "", addresses)
  addresses <- gsub("</address>", "", addresses)
  addresses <- gsub("<br/>", " ", addresses)
  restaurants$Addresses[i] <- addresses
  
  # update progress bar
  setTxtProgressBar(pb, i)
}


# bayesian estimator
populationRatings <- sum(restaurants$Ratings)
populationMean <- sum(restaurants$Stars) / length(restaurants$Stars)

# squared bayesian removes errors but seems to give less of a spread
# restaurants$BayesianSquared <- (restaurants$Ratings ^ 2) / (restaurants$Ratings ^ 2 + populationRatings ^ 2) * restaurants$Stars +
#                         (populationRatings ^ 2) / (restaurants$Ratings ^ 2 + populationRatings ^ 2) * populationMean
  
restaurants$Bayesian <- (restaurants$Ratings) / (restaurants$Ratings + populationRatings) * restaurants$Stars +
                        (populationRatings) / (restaurants$Ratings + populationRatings) * populationMean
