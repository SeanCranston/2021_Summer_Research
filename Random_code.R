
# Building reco sys by splitting
```{r}

sampled_data<- sample(x = c(T, F),
                      size = nrow(movie_ratings),
                      replace = T,
                      prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]


recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
                              parameter = list(k = 30))#this is defualt
recommen_model
class(recommen_model)

```



# model exploring

```{r model exploring}

model_info <- getModel(recommen_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items],
      main = "Heatmap of the first rows and columns")

sum_rows <- rowSums(model_info$sim > 0)
#user has to have a movie that it is similar to 
table(sum_rows) #does 447 users have 30 movies in common?


#how many movies(y-axis) had sum_cols count views(or ratings)
#for 20 movies there was 50 views(or cumulative ratings)
sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill=I("steelblue"), col=I("red"))+ 
    ggtitle("Distribution of the column count")

```




# predict
```{r}

top_recommendations <- 10 # the number of items to recommend to each user
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations

# p <- predict(object = Models_results$UBCF_C,
#              getData(e,"known"),type ="ratings")

user1 <- predicted_recommendations@items[[1]] 
# recommendation for the first user
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10){
    movies_user2[index] <- as.character(
        subset(movie_data,movie_data$movieId == movies_user1[index])$title)
}
movies_user2



recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ 
                                    as.integer(colnames(movie_ratings)[x]) 
                                }) 
# matrix with the recommendations for each user
#dim(recc_matrix)
recommendation_matrix[,1:4]



#first unspecified output
number_of_items <- factor(table(recommendation_matrix))
chart_title <- "Distribution of the Number of Items for IBCF"
qplot(number_of_items, fill=I("steelblue"), col=I("red")) + 
    ggtitle(chart_title)


#second unspecified output
number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(as.integer(names(number_of_items_top)),
                        number_of_items_top)
for(i in 1:4) {
    table_top[i,1] <- as.character(subset(movie_data,
                                          movie_data$movieId == table_top[i,1])$title)
}
colnames(table_top) <- c("Movie Title", "No. of Items")
table_top

```