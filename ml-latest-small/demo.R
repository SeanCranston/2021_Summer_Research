##Demo: How to build a recommendation engine in R
## setwd("C:/Users/User/Desktop/Data-Mania Blog Coding Demos/Recommendation Engine in R")
#Read all the datasets
setwd("D:\\Users\\seanc\\Documents\\STATS\\2021-Summer-Research\\ml-latest-small")
movies=read.csv("movies.csv")
links=read.csv("links.csv")
ratings=read.csv("ratings.csv")
tags=read.csv("tags.csv")
#Import the reshape2 library. Use the file install.packages("reshape2") if the package is not already installed
#install.packages("reshape2", dependencies=TRUE)
#install.packages("stringi", dependencies=TRUE)
library(stringi)
library(reshape2)
library(recommenderlab)
library(tidyverse)
#Create ratings matrix with rows as users and columns as movies. We don't need timestamp
ratingmat = dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)

#We can now remove user ids
ratingmat = as.matrix(ratingmat[,-1])

#Convert ratings matrix to real rating matrx which makes it dense
ratingmat = as(ratingmat, "realRatingMatrix")

#Normalize the ratings matrix
#ratingmat = normalize(ratingmat) 
#Recommender normalizes the data for us

#Create Recommender Model. The parameters are UBCF and Cosine similarity. We take 10 nearest neighbours
rec_mod = Recommender(ratingmat, method = "UBCF", param=list(method="Cosine",nn=10)) 

#Obtain top 5 recommendations for 1st user entry in dataset
Top_5_pred = predict(rec_mod, ratingmat[1], n=5)

Top_5_List = as(Top_5_pred, "list")
Top_5_List


#We convert the list to a dataframe and change the column name to movieId
Top_5_df=data.frame(Top_5_List)
colnames(Top_5_df)="movieId"


#Since movieId is of type integer in Movies data, we typecast id in our recommendations as well
Top_5_df$movieId=as.numeric(levels(Top_5_df$movieId))


#Merge the movie ids with names to get titles and genres
Top_5_df$movieId <- as.integer(Top_5_df$movieId)
name=left_join(Top_5_df, movies, by="movieId")


name

