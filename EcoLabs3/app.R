  
# 

  shinyApp(
    ui = fluidPage(
      shinyjs::useShinyjs(),
      shinyjs::inlineCSS(appCSS),
      
      # Application title
      headerPanel("AHP Ecolabels"),
      
      sidebarPanel(
        # This is intentionally an empty object.
        h6(textOutput("save.results")),
        h5("Created by:"),
        tags$a("Econometrics by Simulation", 
               href="http://www.econometricsbysimulation.com"),
        h5("For details on how data is generated:"),
        tags$a("Blog Post", 
               href=paste0("http://www.econometricsbysimulation.com/",
                           "2013/19/Shiny-Survey-Tool.html")),
        h5("Github Repository:"),
        tags$a("Survey-Tool", 
               href=paste0("https://github.com/EconometricsBySimulation/",
                           "Shiny-Demos/tree/master/Survey")),
        # Display the page counter text.
        h5(textOutput("counter"))
      ),
      
      # Show a table summarizing the values entered
      mainPanel(
        shinyWidgets::sliderTextInput(inputId = "decade", 
                                      label = "Time (decade):", 
                                      choices = c(5, 4, 3, 2, 1, 0, 1, 2, 3, 4, 5), 
                                      selected = 0)
        
      )),
                  
    server = function(input, output, session) {
      observe({
        mandatoryFilled <-
          vapply(fieldsMandatory,
                 function(x) {
                   !is.null(input[[x]]) && input[[x]] != ""
                 },
                 logical(1))
        mandatoryFilled <- all(mandatoryFilled)
        
        shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
      })   
      
      formData <- reactive({
        data <- sapply(fieldsAll, function(x) input[[x]])
        data <- c(data, timestamp = epochTime())
        data <- t(data)
        data
      })
      
      # save each file with the  current time and the md5 hash of the submission data
      saveData <- function(data) {
        fileName <- sprintf("%s_%s.csv",
                            humanTime(),
                            digest::digest(data))
        
        write.csv(x = data, file = file.path(responsesDir, fileName),
                  row.names = FALSE, quote = TRUE)
      }
      
      # action to take when submit button is pressed
      observeEvent(input$submit, {
        shinyjs::disable("submit")
        shinyjs::show("submit_msg")
        shinyjs::hide("error")
        
        tryCatch({
          saveData(formData())
          shinyjs::reset("form")
          shinyjs::hide("form")
          shinyjs::show("thankyou_msg")
        },
        error = function(err) {
          shinyjs::html("error_msg", err$message)
          shinyjs::show(id = "error", anim = TRUE, animType = "fade")
        },
        finally = {
          shinyjs::enable("submit")
          shinyjs::hide("submit_msg")
        })
      })
      
      # action to take when submit button is pressed
      observeEvent(input$submit, {
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      })
      
      # hides the thank you message and goes back to the form 
      observeEvent(input$submit_another, {
        shinyjs::show("form")
        shinyjs::hide("thankyou_msg")
      }) 
      
    }
  )
  