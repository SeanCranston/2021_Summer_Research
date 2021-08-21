####################################### Packages ###############################


library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(data.table)
library(tidyverse)
library(recommenderlab)
library(xtable)


#################################### Imports ###################################


movie_data <- read_csv("https://raw.githubusercontent.com/SeanCranston/2021_Summer_Research/master/Data/movies.csv",
                       col_types = cols(
                           movieId = col_double(),
                           title = col_character(),
                           genres = col_character()
                       ))
i = 1
movie_row <- movie_data[i,]

rating_data <- read_csv("https://raw.githubusercontent.com/SeanCranston/2021_Summer_Research/master/Data/ratings.csv",
                        col_types = cols(
                            userId = col_double(),
                            movieId = col_double(),
                            rating = col_double(),
                            timestamp = col_double()
    #                     )) %>% 
    # select(-timestamp)
))

################################### user interface #############################


ui = navbarPage("Movie Recommender",
                
                
                ################## page one ####################################
                
                
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
                                 uiOutput("movieID"),
                                 #dataTableOutput("movieID"),
                                 hr(),
                                 dataTableOutput("table")
                             )
                         )
                ),
                
                
                ################## page two ####################################
                
                
                tabPanel("Predicting movies",fluidPage(theme = shinytheme("flatly")),
                         tags$head(
                             tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                         pageWithSidebar(
                             headerPanel('Rank movies'),
                             sidebarPanel(width = 3,
                                          
                                          textInput('USERID', 'Enter user to be predicted',669),
                                          
                                          sliderTextInput(
                                              inputId = "train_submit",
                                              label = "What proportion of data should be in training dataset?", 
                                              choices = seq(from = 0, to = 1, by = .05),
                                              grid = TRUE,
                                              selected = .8
                                          ),
                                          
                                          pickerInput(inputId = "model",
                                              #label = "Select a Model", 
                                              choices = c("Popular","ALS","UBCF","SVD"),
                                              options = list(title = "Select a model"),
                                              choicesOpt = list(subtext = c("","","Cosine",""))),
                                          
                                          actionButton("pred_submit", "Make predictions")
                                          ),
                             
                             mainPanel(
                                 textOutput("Make sure to rate some movies"),
                                 uiOutput("pred")
                                 )
                         )
                ),
                tabPanel("Info",
                         p("Developer",style = "font-size:30px"),
                         p(a("Sean Cranston", href="https://github.com/SeanCranston", target="_blank"),
                           style = "font-size:25px"),
                         p("e-mail: seancranston0@gmail.com",style = "font-size:20px"),
                         hr(),
                         p("Further Readings",style ="font-size:30px"),
                         p("For more details on how we decided to use certain models 
                           with certain paramaters please visit this github repository", 
                           a("https://github.com/SeanCranston/2021_Summer_Research",
                             href="https://github.com/SeanCranston/2021_Summer_Research"), "or the 
                           knitted rmd that it producted", a("https://rpubs.com/Sean_Cranston/Movie_reco",
                                                             href="https://rpubs.com/Sean_Cranston/Movie_reco"))
                ) 
)


########################################### server #############################


server = function(input,output){
    
    
    ##################################### Page one #############################
    
    
    
    ##### this outputs the a row from movie_data that is to be rated
    values <- reactiveValues(movie_row = movie_data[i,])
    row_index <- reactiveValues(i = 1)
    
    #### this allows the user to go forward and rate the next movie
    observeEvent(input$Next, {
        if(row_index$i < 10329){
            row_index$i = row_index$i + 1
            temp <- movie_data[row_index$i,]
            values$movie_row <- temp
        }
    })
    #### this allows the user to go back to the previous movie to be rated
    observeEvent(input$Previous,{
        if(row_index$i > 1){
            row_index$i = row_index$i - 1
            temp <- movie_data[row_index$i,]
            values$movie_row <- movie_data[row_index$i,]
        }
    })
    #### this controls how the matrix looks that the user is rating
    output$movieID <- renderUI({
        M <- values$movie_row %>% as.matrix()
        M <- print(xtable(M, align = rep("c", ncol(M)+1)
                          ),
                   floating = F, tabular.environment = "array", comment = F, print.results = F
                   )
        html <- paste0("$$",M,"$$")
        html %>% 
            HTML() %>% 
            withMathJax() %>% 
            list()
    })
    
    
    ##### This outputs rating_data with the updated ratings from the user
    df_recactive <- reactiveValues()
    df <- eventReactive(input$goButton, {
        if(input$NewID!="" && !is.null(input$NewVal) && input$goButton>0){
            newrow = tibble(movieId = values$movie_row$movieId,
                            rating = input$NewVal,
                            userId = input$NewID,
                            timestamp = 0)
            rating_data <<- rbind(newrow, rating_data, fill = T) #use <<- if you want the user to save there input
        }
        rating_data %>% 
            group_by(userId,movieId) %>% 
            distinct(., userId, .keep_all = T)
    }, ignoreNULL = FALSE)
    output$table <- renderDataTable( df())
    
    
    ######################################### Page two #########################
    

        pred <- eventReactive(input$pred_submit, {
        rating_data <- df() %>% as_tibble()
        
        #### reshape data
        movie_ratings <- reshape2::dcast(rating_data,
                                         userId~movieId,
                                         value.var = "rating",
                                         na.rm=FALSE) %>%
            select(-c('userId')) %>% # same as row index, just taking up space
            as.matrix() %>% #needs to be matrix to convert to sparse matrix
            as(.,"realRatingMatrix")
        
        ####splitting data
        mid = as.numeric(input$train_submit) * dim(movie_ratings)[1]
        mid = if_else(floor(mid) == mid, mid, floor(mid))
        train <- movie_ratings[1:mid]
        test <- movie_ratings[(mid+1):dim(movie_ratings)[1]]
        
        ##### Build model and make predictions
        if (input$model == "Popular"){
            param_list = list()
        } else if (input$model == "ALS"){
            param_list = list(n_factors = 40)
        } else if (input$model == "UBCF"){
            param_list = list(method = "Cosine", nn = 1)
        } else if (input$model == "SVD"){
            param_list = list(k = 10)
        }
        rec_model <- Recommender(data = train,
                                 method = as.character(input$model),
                                 param = param_list)

        User_pred <- if_else(as.numeric(input$USERID) > mid,
                             as.numeric(input$USERID) - mid,
                             as.numeric(input$USERID))
        
        movies_user <- as(predict(rec_model,
                                  User_pred,#this is the user ID
                                  data = test),
                          "list")
        for (index in 1:10){
            movies_user[index] <- as.character(
                subset(movie_data,movie_data$movieId == movies_user1[index])$title)
        }

        #print nice table
        #how do i get the x out of the colname spot???
        M <- movies_user %>% as.matrix()
        M <- print(xtable(M),
                   floating = F, tabular.environment = "array", comment = F, print.results = F)
        html <- paste0("$$",M,"$$")
        html %>%
            HTML() %>%
            withMathJax() %>%
            list()
    })
    
    output$pred <- renderUI( pred())
    
   
}

shinyApp(ui,server)

