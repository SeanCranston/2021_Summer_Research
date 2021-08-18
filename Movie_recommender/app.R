library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(data.table)
library(tidyverse)
library(recommenderlab)

################################################################################
# Data importing
setwd("C:/Users/seanc/OneDrive/Documents/2021_Summer_Research") #set working directory, will have to change if ran on another computer
# should make a downloadable link so don't have to do this
movie_data <- read_csv("movies.csv",
                       col_types = cols(
                           movieId = col_double(),
                           title = col_character(),
                           genres = col_character()
                       ))
i = 1
movie_row <- movie_data[i,]

rating_data <- read_csv("ratings.csv",
                        col_types = cols(
                            userId = col_double(),
                            movieId = col_double(),
                            rating = col_double(),
                            timestamp = col_double()
                        )) %>% 
    select(-timestamp)

################################################################################

ui = navbarPage("Movie Recommender",
                tabPanel("Data Entry",fluidPage(theme = shinytheme("flatly")),
                         tags$head(
                             tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                         pageWithSidebar(
                             headerPanel('Rank movies'),
                             sidebarPanel(width = 4,
                                          
                                          textInput('NewID', 'Enter new ID',669), 
                                          radioGroupButtons(inputId = 'NewVal',label = 'Rate the Movie',choices = 1:5),
                                          
                                          actionButton("Previous","Previous"),
                                          actionButton("Next","Next"),
                                          hr(),
                                          actionButton("goButton", "Update Table")
                             ),
                             
                             mainPanel(
                                 dataTableOutput("movieID"),
                                 hr(),
                                 dataTableOutput("table")
                             )
                         )
                ),
                
                tabPanel("Predicting movies",fluidPage(theme = shinytheme("flatly")),
                         tags$head(
                             tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                         pageWithSidebar(
                             headerPanel('Rank movies'),
                             sidebarPanel(width = 4,
                                          selectInput("model",
                                                      "Select a model",
                                                      c("Popular","ALS","UBCF Cosine","SVD"))
                             ),
                             
                             mainPanel(
                                 dataTableOutput("df")
                             )
                         )
                         
                         
                ),
                
                tabPanel("Developer",
                         p(a("Sean Cranston", href="https://github.com/SeanCranston", target="_blank"),style = "font-size:25px"),
                         p("e-mail: seancranston0@gmail.com",style = "font-size:20px"),
                )
                
)

################################################################################
server = function(input,output){
    
    ##### this outputs the a row from movie_data that is to be rated
    values <- reactiveValues(movie_row = movie_data[i,])
    row_index <- reactiveValues(i = 1)
    observeEvent(input$Next, {
        if(row_index$i < 10329){#input$Nex>0 &&
            row_index$i = row_index$i + 1
            temp <- movie_data[row_index$i,]
            values$movie_row <- temp
        }
    })
    observeEvent(input$Previous,{
        if(row_index$i > 1){#input$Prev>0 &&
            row_index$i = row_index$i - 1
            temp <- movie_data[row_index$i,]
            values$movie_row <- movie_data[row_index$i,]
        }
    })
    output$movieID <- renderDT(values$movie_row)

    ##### This outputs rating_data with the updated ratings from above
    df_recactive <- reactiveValues()
    df <- eventReactive(input$goButton, {
        if(input$NewID!="" && !is.null(input$NewVal) && input$goButton>0){
            newrow = tibble(movieId = values$movie_row$movieId,
                                rating = input$NewVal,
                                userId = input$NewID)
            rating_data <<- rbind(newrow, rating_data, fill = T) #use <<- if you want the user to save there input
        }
        rating_data %>% 
            group_by(userId,movieId) %>% 
            distinct(., userId, .keep_all = T)
    }, ignoreNULL = FALSE)
    output$table <- renderDataTable( df())
    
    ##### Build model and make predictions
    # 
    # pred_matrix <- reactiveValues(ratingMatrix = reshape2::dcast(rating_data,
    #                                                              userId~movieId,
    #                                                              value.var = "rating",
    #                                                              na.rm=FALSE) %>%
    #                                   #select(-c('userId')) %>% # same as row index, just taking up space
    #                                   as.matrix() %>% #needs to be matrix to convert to sparse matrix
    #                                   as(.,"realRatingMatrix"))
    # 
    # movie_ratings <- reactiveValues(ratingMatrix <- pred_matrix$ratingMatrix[rowCounts(ratingMatrix) > 0,
    #                                                                          colCounts(ratingMatrix) > 50])
    # 
    # e <-  reactiveValues(eval_sheme <- evaluationScheme(movie_ratings$ratingMatrix, #evaluate will normalize for us
    #                                                     method="cross", 
    #                                                     k = 3,#3 fold cross validation
    #                                                     given=3, 
    #                                                     goodRating=3))
    # 
    # rec_model <- reactiveValues(rec <- Recommender(data = getData(e$eval_sheme,"train"),
    #                                                method = "POPULAR"))
    # 
    # top_recommendations <- reactiveValues(j = 10) # the number of items to recommend to each user
    # 
    # predicted_recommendations <- reactiveValues(pred <- predict(object = rec_model$rec,
    #                                                             newdata = getData(e$eval_sheme,"known"),
    #                                                             n = top_recommendations$j))
    # 
    # # 10 recommendations for the first user
    # user1 <- reactiveValues(pred <- predicted_recommendations$pred@items[[669]])
    # movies_user1 <- reactiveValues(a <- predicted_recommendations$pred@itemLabels[user1$pred])
    # movies_user2 <- reactiveValues(one <- movies_user1)
    # for (index in 1:10){
    #     movies_user2$one[index] <- as.character(
    #         subset(movie_data,movie_data$movieId == movies_user1[index])$title)
    # }
    # movies_user2
}

shinyApp(ui,server)

