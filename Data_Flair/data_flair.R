#data flair example

library(recommenderlab)
library(tidyverse)                   
library(data.table)
library(reshape2)

setwd("C:\\Users\\seanc\\OneDrive\\Documents\\2020_Summer_Research\\Data_Flair")
movie_data <- read.csv("movies.csv",stringsAsFactors=FALSE)
rating_data <- read.csv("ratings.csv")



############ 
#Data Processing


names_genre <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime",
           "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical", 
           "Mystery","Romance", "Sci-Fi", "Thriller", "War", "Western")
list_genre <- str_split(movie_data$genres, "[|]")

genre = matrix(0, nrow(movie_data), ncol = length(names_genre)) %>% 
    `colnames<-`(names_genre) 
    
for (row in 1:nrow(genre)) { #can't seem to write a function to continue the pipe :(
    for (col in 1:ncol(genre)) {
        genre[row,col] = ifelse(any(names_genre[col] == list_genre[[row]]), 1, 0)
    }
}

genre = as.data.frame(genre)
str(genre)

SearchMatrix <- cbind(movie_data[,1:2], genre[])
head(SearchMatrix)


#####
#creating a sparse matrix for rating_data

ratingMatrix <- reshape2::dcast(rating_data, 
                                userId~movieId, 
                                value.var = "rating", 
                                na.rm=FALSE) %>% 
    select(-c('userId')) %>% 
    as.matrix() %>% #needs to be matrix to convert to sparse matrix
    as(.,"realRatingMatrix") #Convert rating matrix into a recommenderlab sparse matrix



#####
# exploring similar data

similarity_mat <- similarity(ratingMatrix[1:100 ], #get 100 users 
                             method = "cosine",
                             which = "users") #similar based on user
as.matrix(similarity_mat)
image(as.matrix(similarity_mat), main = "User's Similarities")



movie_similarity <- similarity(ratingMatrix[, 1:100],
                               method = "cosine",
                               which = "items") %>%  #similar based on movie
    as.matrix()
image(as.matrix(movie_similarity), main = "Movies similarity")



table_of_ratings <- as.vector(ratingMatrix@data) %>% 
    # extracting unique ratings
    #unique() %>%  
    table()
table_of_ratings



#####
# Most viewed visualization

table_view <- ratingMatrix %>% 
    colCounts() %>% 
    data.frame(movie = names(.),
               views = .) %>% 
    arrange(desc(views)) %>% 
    mutate(title = NA)
for (index in 1:10325){ #add title to table_views
    table_view[index,3] <- as.character(subset(movie_data,
                                                movie_data$movieId == table_view[index,1])$title)
}
table_view[1:6,]

ggplot(table_view[1:6,], aes(x = title, y = views))+
    geom_bar( stat="identity",fill = 'steelblue') +
    geom_text(aes(label=views), vjust=-0.3, size=3.5) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Total Views of the Top Films")+
    ylim(0, 350)
    

#####
# Heatmap of movie ratings

image(ratingMatrix[1:25,1:25], 
      axes = FALSE, 
      main = "Heatmap of the first 25 rows and 25 columns")


#####
#Data Preparation

movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50,
                              colCounts(ratingMatrix) > 50]
movie_ratings
minimum_movies<- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                    colCounts(movie_ratings) > minimum_users],
      main = "Heatmap of the top users and movies")



average_ratings <- rowMeans(movie_ratings)
qplot(average_ratings, fill=I("steelblue"), col=I("red")) +
    ggtitle("Distribution of the average rating per user")



#####
#normalizing

normalized_ratings <- normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)

image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
                         colCounts(normalized_ratings) > minimum_users],
      main = "Normalized Ratings of the Top Users")



#####
# Data Binarization

binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)
#movies_watched <- binarize(movie_ratings, minRating = 1)

good_rated_films <- binarize(movie_ratings, minRating = 3)
image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies,
                       colCounts(movie_ratings) > binary_minimum_users],
      main = "Heatmap of the top users and movies")



#####
#adjusting parameters/options

recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)

#ask about how latent factors are used, what might they be
lapply(recommendation_model, "[[", "description") 

recommendation_model$IBCF_realRatingMatrix$parameters



#####
#Building recommendation system in R

sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(movie_ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]

recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
                              parameter = list(k = 30))
recommen_model
class(recommen_model)



