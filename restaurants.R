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
populationRatings <- sum(restaurants$Ratings) / length(restaurants$Ratings)
populationMean <- sum(restaurants$Stars) / length(restaurants$Stars)

# squared bayesian removes errors but seems to give less of a spread
# restaurants$BayesianSquared <- (restaurants$Ratings ^ 2) / (restaurants$Ratings ^ 2 + populationRatings ^ 2) * restaurants$Stars +
#                         (populationRatings ^ 2) / (restaurants$Ratings ^ 2 + populationRatings ^ 2) * populationMean
  
restaurants$Bayesian <- (restaurants$Ratings) / (restaurants$Ratings + populationRatings) * restaurants$Stars +
                        (populationRatings) / (restaurants$Ratings + populationRatings) * populationMean
restaurants$NormalizedBayes <- (restaurants$Bayesian - populationMean) / var(restaurants$Stars)

library(ggmap)
boozAddresses <- c("901 15th Street Northwest, Washington, DC 20005", "8283 Greensboro Drive, McLean, VA 22102", "1550 Crystal Drive, Arlington, VA 22202", "20 M Street Southeast 1000, Washington, DC 20003")
boozPositions <- data.frame(matrix(nrow = 4))
for (i in 1:length(boozAddresses)) {
  position <- geocode(boozAddresses[i]);
  boozPositions$Latitudes[i] <- position$lat;
  boozPositions$Longitudes[i] <- position$lon;
}
boozPositions[1] <- NULL

for (i in 1:length(restaurants$Addresses)) {
  position <- geocode(restaurants$Addresses[i]);
  restaurants$Latitudes[i] <- position$lat
  restaurants$Longitudes[i] <- position$lon
  restaurants$Distances[i] <- mapdist(boozAddresses[1], restaurants$Addresses[i], mode = "driving")$minutes
}

meanDistance <- sum(restaurants$Distances) / length(restaurants$Distances)
restaurants$NormalizedDistances <- (restaurants$Distances - meanDistance) / var(restaurants$Distances)

restaurants$WeightedRating <- restaurants$NormalizedBayes - restaurants$NormalizedDistances * 10

library(ggplot2)
library(ggmap)

# getting the map
map <- get_map(location = c(lon = mean(restaurants$Longitudes), lat = mean(restaurants$Latitudes)), zoom = 12,
               maptype = "roadmap", scale = 2)

# plotting the map with some points on it
ggmap(map) +
  geom_point(data = restaurants, aes(x = Longitudes, y = Latitudes, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  geom_point(data = boozPositions, aes(x = Longitudes, y = Latitudes, fill = "blue", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

library(leaflet)
library(rgdal)

pal <- colorNumeric(
  palette = "Blues",
  domain = restaurants$WeightedRating
)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(m, lng = restaurants$Longitudes, lat = restaurants$Latitudes, popup = restaurants$Names, color = pal(restaurants$WeightedRating)) %>%
  addCircleMarkers(m, lng = boozPositions$Longitudes, lat = boozPositions$Latitudes, popup = "Booz Allen Office", color = "red", fillColor = "red")
m  # Print the map
