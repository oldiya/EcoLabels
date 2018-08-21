library(shiny)

  # Read the survey questions
  Qlist <- read.csv("Qlist.csv")
  # Qlist <- Qlist[1,]

  shinyServer(function(input, output) {
  
    # Create an empty vector to hold survey results
    results <<- rep("", nrow(Qlist))
    # Name each element of the vector based on the
    # second column of the Qlist
    #names(results)  <<- Qlist[,2]
    
    # Hit counter
    output$counter <- 
      renderText({
        if (!file.exists("counter.Rdata")) counter <- 0
        if (file.exists("counter.Rdata")) load(file="counter.Rdata")
        counter <- counter <<- counter + 1
        
        save(counter, file="counter.Rdata")     
        paste0("Hits: ", counter)
      })
    
    # Define questions 
    
    Q1 <- list(h5("A vs B"),
                      shinyWidgets::sliderTextInput(inputId = "decade", 
                                                    label = "Time (decade):", 
                                                    choices = c(5, 4, 3, 2, 1, 0, 1, 2, 3, 4, 5), 
                                                    selected = 0))
    
    
    Q2 <- list(h5("B vs C"),
                      shinyWidgets::sliderTextInput(inputId = "decade", 
                                                    label = "Time (decade):", 
                                                    choices = c(5, 4, 3, 2, 1, 0, 1, 2, 3, 4, 5), 
                                                    selected = 0))
    
    Q3 <- list(h5("C vs D"),
               shinyWidgets::sliderTextInput(inputId = "decade", 
                                             label = "Time (decade):", 
                                             choices = c(5, 4, 3, 2, 1, 0, 1, 2, 3, 4, 5), 
                                             selected = 0))
    # Question order 
    a <- a[-sample(a, size = 1, replace = FALSE, prob = NULL)]
    
    
    # This renderUI function holds the primary actions of the
    # survey area.
    output$MainAction <- renderUI( {
      dynamicUi()
    })
    
    # Dynamic UI is the interface which changes as the survey
    # progresses.  
    dynamicUi <- reactive({
      # Initially it shows a welcome message. 
      if (input$Click.Counter==0) 
        return(
          list(
            h5("Welcome to Shiny Survey Tool!"),
            h6("by Francis Smart")
          )
        )
      
      # Once the next button has been clicked once we see each question
      # of the survey.
      if (input$Click.Counter == 1)  
        return(
          list(
            h5("PERSONAL INFORMATION"),
            textInput("name", "Name", ""),
            textInput("surname", "Surname"),
            #checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
            selectInput("institution_type", "Institution where you work / study",
                        c("",  "UEF", "EFI", "Systembolaget", "other")),
            textInput("otherinst", "If other, please specify the institution name here")
          )
        )
      
      if (input$Click.Counter == 2 )  
        return(  radioButtons("survey", "Please Select your age:",    
                              c("< 20", "21-30", "31-40",
                                "41-50", "51-60", ">65"))
        )


      if (input$Click.Counter== 3 )  
        return(
          radioButtons("radio_year_select","Year", c("1999" = "1999", "2001" = "2001"), inline=T)
          
        )
      
      if (input$Click.Counter== 3 )  
        return( Q1
        )
      
     
      
      if (input$Click.Counter>0 & input$Click.Counter<=nrow(Qlist))  
        return(
          list(
            h5(textOutput("question")),
            radioButtons("survey", "Please Select:", 
                         c("Prefer not to answer", option.list()))
          )
        )
      
      
      # Finally we see results of the survey as well as a
      # download button.
      if (input$Click.Counter>nrow(Qlist))
        return(
          list(
            h4("View aggregate results"),
            tableOutput("surveyresults"),
            h4("Thanks for taking the survey!"),
            downloadButton('downloadData', 'Download Individual Results'),
            br(),
            h6("Haven't figured out how to get rid of 'next' button yet")
          )
        )    
    })
    
    # This reactive function is concerned primarily with
    # saving the results of the survey for this individual.
    output$save.results <- renderText({
      # After each click, save the results of the radio buttons.
      if ((input$Click.Counter>0)&(input$Click.Counter>!nrow(Qlist)))
        try(results[input$Click.Counter] <<- input$survey)
      # try is used because there is a brief moment in which
      # the if condition is true but input$survey = NULL
      
      # If the user has clicked through all of the survey questions
      # then R saves the results to the survey file.
      if (input$Click.Counter==nrow(Qlist)+1) {
        if (file.exists("survey.results.Rdata")) 
          load(file="survey.results.Rdata")
        if (!file.exists("survey.results.Rdata")) 
          presults<-NULL
        presults <- presults <<- rbind(presults, results)
        rownames(presults) <- rownames(presults) <<- 
          paste("User", 1:nrow(presults))
        save(presults, file="survey.results.Rdata")
      }
      # Because there has to be a UI object to call this
      # function I set up render text that distplays the content
      # of this funciton.
      ""
    })
    
    # This function renders the table of results from the
    # survey.
    output$surveyresults <- renderTable({
      t(summary(presults))
    })
    
    # This renders the data downloader
    output$downloadData <- downloadHandler(
      filename = "IndividualData.csv",
      content = function(file) {
        write.csv(presults, file)
      }
    )
    
    # The option list is a reative list of elements that
    # updates itself when the click counter is advanced.
    option.list <- reactive({
      qlist <- Qlist[input$Click.Counter,3:ncol(Qlist)]
      # Remove items from the qlist if the option is empty.
      # Also, convert the option list to matrix. 
      as.matrix(qlist[qlist!=""])
    })
    
    # This function show the question number (Q:)
    # Followed by the question text.
    output$question <- renderText({
      paste0(
        "Q", input$Click.Counter,":", 
        Qlist[input$Click.Counter,2]
      )
    })
    
  })
