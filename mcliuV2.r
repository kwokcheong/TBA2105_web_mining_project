library(dplyr)
library(readxl)
library(stringr)
library(httr)
library(rvest)
library(tm)
library(wordcloud)
library(RColorBrewer)


# users' reviews dataset 
hotelsReviews <- read.csv("workfile_user_review_data.csv")

# hotels info dataset
hotelinfo <- read.csv("hotel_info_list.csv")

# remove duplicates 268 -> 211
hotels <- unique(hotelinfo)


#remove cancellation tag as this is causing some duplicates 211 -> 204
hotels$freeCancelTag <- NULL
hotels <- unique(hotels)



hotelNames <- cbind(hotels$hotelId, hotels$hotelName, hotels$location)
colnames(hotelNames) <- c("hotelID","hotelNames","location")
hotelNames <- as.data.frame(hotelNames)

# remove one more duplicate 204->203
hotelNames <- unique(hotelNames)


# manually filling in missing location data
hotelNames$location[hotelNames$hotelID=="712600"] <- "Chinatown"
hotelNames$location[hotelNames$hotelID=="31212318"] <- "Chinatown"
hotelNames$location[hotelNames$hotelID=="995050"] <- "Chinatown"
hotelNames$location[hotelNames$hotelID=="1359681"] <- "Little India"
hotelNames$location[hotelNames$hotelID=="687572"] <- "Tiong Bahru"

# list of hotel ID for building vector of links 
hotelIDList <- as.data.frame(hotelNames$hotelID)

# find mean overall ratings for hotels using user reviews data set
colnames(hotelsReviews)[1] <- "hotelID"
hotelsbyOverall <- hotelsReviews %>% group_by(hotelID) %>% mutate(meanRating = mean(ratingAll))
hotelsbyOverall <- as.data.frame(cbind(hotelsbyOverall$hotelID,hotelsbyOverall$meanRating))
hotelsbyOverall <- unique(hotelsbyOverall)
colnames(hotelsbyOverall) <- c("hotelID","OverallRating")
hotelsbyOverall$hotelID <- as.factor(hotelsbyOverall$hotelID)

hotelsbyOverall <- left_join(x=hotelsbyOverall,y=hotelNames)

rankedbyOverall <- hotelsbyOverall[order(hotelsbyOverall$OverallRating, decreasing=TRUE),]


# find mean overall location ratings
hotelsbyLocation <- hotelsReviews %>% group_by(hotelID) %>% mutate(meanLocationRating = mean(ratingLocation))
hotelsbyLocation <- as.data.frame(cbind(hotelsbyLocation$hotelID,hotelsbyLocation$meanLocationRating))
hotelsbyLocation <- unique(hotelsbyLocation)

colnames(hotelsbyLocation) <- c("hotelID","locationRating")
hotelsbyLocation$hotelID <- as.factor(hotelsbyLocation$hotelID)

hotelsbyLocation <- left_join(x=hotelsbyLocation,y=hotelNames)

rankedbyLocation <- hotelsbyLocation[order(hotelsbyLocation$locationRating, decreasing=TRUE),]


# find mean facilities ratings
hotelsbyFacility <- hotelsReviews %>% group_by(hotelID) %>% mutate(meanFacilityRating = mean(ratingFacility))
hotelsbyFacility <- as.data.frame(cbind(hotelsbyFacility$hotelID,hotelsbyFacility$meanFacilityRating))
hotelsbyFacility <- unique(hotelsbyFacility)

colnames(hotelsbyFacility) <- c("hotelID","facilityRating")
hotelsbyFacility$hotelID <- as.factor(hotelsbyFacility$hotelID)

hotelsbyFacility <- left_join(x=hotelsbyFacility,y=hotelNames)

rankedbyFacility <- hotelsbyFacility[order(hotelsbyFacility$facilityRating, decreasing=TRUE),]


# find mean service ratings
hotelsbyService <- hotelsReviews %>% group_by(hotelID) %>% mutate(meanServiceRating = mean(ratingService))
hotelsbyService <- as.data.frame(cbind(hotelsbyService$hotelID,hotelsbyService$meanServiceRating))
hotelsbyService <- unique(hotelsbyService)

colnames(hotelsbyService) <- c("hotelID","serviceRating")
hotelsbyService$hotelID <- as.factor(hotelsbyService$hotelID)

hotelsbyService <- left_join(x=hotelsbyService ,y=hotelNames)

rankedbyService <- hotelsbyService[order(hotelsbyService$serviceRating, decreasing=TRUE),]


# find mean rooms ratings
hotelsbyRoom <- hotelsReviews %>% group_by(hotelID) %>% mutate(meanRoomRating = mean(ratingRoom))
hotelsbyRoom <- as.data.frame(cbind(hotelsbyRoom$hotelID,hotelsbyRoom$meanRoomRating))
hotelsbyRoom <- unique(hotelsbyRoom)

colnames(hotelsbyRoom) <- c("hotelID","roomRating")
hotelsbyRoom$hotelID <- as.factor(hotelsbyRoom$hotelID)

hotelsbyRoom <- left_join(x=hotelsbyRoom ,y=hotelNames)

rankedbyRoom <- hotelsbyRoom[order(hotelsbyRoom$roomRating, decreasing=TRUE),]

# graphical distribution of all ratings
par(mfrow=c(3,2))
hist(hotelsbyLocation$locationRating, main="Distribution of Location Ratings")
hist(hotelsbyService$serviceRating, main="Distribution of Service Ratings")
hist(hotelsbyRoom$roomRating, main="Distribution of Room Ratings")
hist(hotelsbyFacility$facilityRating, main="Distribution of Facility Ratings")
hist(hotelsbyOverall$OverallRating, main="Distribution of Overall Ratings")


# generate word cloud for locations top 50 ratings
par(mfrow=c(1,1))
locations <- iconv(rankedbyLocation$location[1:50],"WINDOWS-1252","UTF-8")
corpus <- Corpus(VectorSource(locations))
tdm <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, 
                                                 stopwords = stopwords("english"), removeNumbers = TRUE, tolower = TRUE))
m = as.matrix(tdm)
word_freqs = sort(rowSums(m), decreasing=TRUE)
dm = data.frame(word=names(word_freqs), freq=word_freqs)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(3,"Dark2"))


# EDA of hotel info data set
#extract number of reviews as numeric
totalReviewNumeric <- str_match(hotels$totalReviews,"(.*?).reviews")
totalReviewNumeric[,2] <- gsub("\\,","",totalReviewNumeric[,2])
hotels$totalReviews <- totalReviewNumeric[,2]
hotels$totalReviews <- as.numeric(hotels$totalReviews)


par(mfrow=c(1,2))
plot(hotels$star,hotels$score,xlab="Hotel Star Rating", ylab="Hotel Score")
plot(hotels$score,hotels$totalReviews, xlab="Hotel Score", ylab="Number of Reviews")

#tble to see if average scores correspond to star rating
hotels <- hotels %>% group_by(star) %>% mutate(meanScorebyStar = mean(score))
scoretable <- as.data.frame(unique(cbind(hotels$star, hotels$meanScorebyStar)))
colnames(scoretable) <- c("Star","Average Score")
View(scoretable)


##############################################################################

# this part uses hotel info data set to extract amenities of each hotel to build features matrix
# possibly for content based recommander

generateFilenameFromURL <- function(url){
  #Complete implementation... 
  filename <- gsub("[:./?,&=]", "_", url)
  filename <- paste0(filename, ".html")
  return(filename)
}



url <- "https://sg.trip.com/hotels/detail/?cityId=73&hotelId="

#build vector of URL addresses to access individual html pages
for(i in hotelIDList){
  
  fullurl <- paste0(url,i)
  links <- fullurl
}


getHTML <- function(url, useCache = T){
  filename <- generateFilenameFromURL(url)
  
  if (useCache && file.exists(filename)){
    html <- readChar(filename, file.info(filename)$size)
    #return (html)
  }
  
  print("#downloading...")
  req <- GET(url)

  html <- content(req)

  html <- as.character(html)
  
  #cache the file 
  write(html, filename)
  #return (html)
}



#grabbing html pages of individual hotel to build a vector of all possible amenities

features <- NULL
features_df <- data.frame()

for(j in links[1:length(links)]){
  
  Sys.sleep(0.2)
  page <- getHTML(j)
  page <- read_html(page)
  amenities <- page %>% html_elements('.facilityDesc') %>% html_text()

  features <- c(features, amenities)
  features <- unique(features)
  
}

# empty matrix for amenities
features <- t(sort(features))
features_df <- data.frame(matrix(ncol=length(features)+3,nrow=203))
cols <- c('hotelURL','hotelID','hotelNames',features)
colnames(features_df) <- cols
features_df[,] <- 0


#for each hotel html, extract amenities and fill in the amenities matrix: 1 if present
rowcounter=0
features_counter=0

for(i in links[1:length(links)]){
  
  amenities <- NULL
  Sys.sleep(0.2)
  page <- getHTML(i)
  page <- read_html(page, options = "HUGE")
  amenities <- page %>% html_elements('.facilityDesc') %>% html_text()
  
  rowcounter=rowcounter+1
  
  amenities <- t(amenities)
 
  features_df[rowcounter,1] <- i
  
  #reset feature counter
  features_counter <- 4
  
  for(j in colnames(features_df[4:158])){
    
    for(k in amenities[1:length(amenities)]){
      if(j==k)
        features_df[rowcounter, features_counter] <- 1
    }
    
    features_counter=features_counter+1
  }
}

# padding both ends of the matrix with hotel IDs and hotel names
hotelID <- str_match(features_df$hotelURL,".*?hotelId=(.*).*?")
features_df$hotelID <- hotelID[,2]

hotelNames1 <- hotelNames
hotelNames1$location <- NULL
features_df <- left_join(x=features_df,y=hotelNames1,by="hotelID")
features_df$hotelNames.x <- features_df$hotelNames.y

write.csv(features_df,"amenitiesMatrix.csv")



######################################

features_df <- read.csv("amenitiesMatrix.csv")

userProfile <- unique(hotelsReviews)  # double checking no duplicates
userProfile$userId <- NULL
userProfile$content <- NULL
userProfile$checkinDate <- NULL

# find number of unique users in the User Review Data base
uniqueUsers <- unique(userProfile$userId_cleaned)

# select only hotels with ratings of >4.5 Assumption is that users like the amenities offered by these hotels
userProfile <- userProfile %>% filter(ratingAll >4.5) 


# manually examine user reviews data set to extract hotel preferences of users
# manually change user ID
user="_AP359560"

userProfile1 <- userProfile %>% filter(userProfile$userId_cleaned == user)

#extract hotel IDs from UserProfile1, select rows from amentities matrix

userPref <- as.data.frame(NULL)
j <- 1
for(k in userProfile1$hotelID){
  
  k <- features_df[features_df$HotelID==userProfile1$hotelID[j],]
  #print(t(k))
  j <- j+1
  userPref <- rbind(userPref,k)
  
}


userPrefMatrix <- userPref[,5:159]
featuresMatrix <- features_df[,5:159] 
rownames(featuresMatrix) <- features_df$HotelID


#condenses preferences of user into a single row to feed into recommander model
userTest <- apply(userPrefMatrix,2,sum)
userTest <- as.numeric(userTest)
userTest <- as.data.frame(t(ceiling(userTest/nrow(userPrefMatrix))))


# remove hotel IDs from featursMatrix containing hotel IDs of userPref
j <-1
for(h in userProfile1$hotelID){
  
  index <- which(rownames(featuresMatrix)==as.character(h))
  featuresMatrix <- featuresMatrix[-c(index),]

  j <- j+1
}


#define dataframe of cor coefficients between the user test and individual hotels

corTable <- data.frame(hotelID=as.character(), corCoeff=as.numeric())


for(r in rownames(featuresMatrix)){
  train <- featuresMatrix[r,] # individual row from featuresMatrix
  corRow <- c(r, cor(as.numeric(train),as.numeric(userTest))) #perform similarity test 
  corTable <- rbind(corTable,corRow)
}

# sort corTable
corTable <- corTable[order(corTable$corrCoeff,decreasing = TRUE),]
colnames(corTable) <- c("hotelID","corrCoeff")

corTable <- inner_join(corTable,hotelNames)

print(corTable$hotelNames[1:10])

