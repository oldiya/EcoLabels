#####################################
# Title: EcoLabels questionnaire
# Date: 19/06/2018
# Author: Olalla Díaz-Yáñez
#####################################

# Setting up a shiny app

# Global section 

  fieldsMandatory <- c("name", "surname", "institution_type", "r_num_years")
  
  labelMandatory <- function(label) {
    tagList(
      label,
      span("*", class = "mandatory_star")
    )
  }
  
  appCSS <-
    ".mandatory_star { color: red; }
    #error { color: red; }"
  
  humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS") # defines a friendly time
  
  
  # Select questions to be saved 
  fieldsAll <- c("name", "surname", "institution_type")
  responsesDir <- file.path("Responses")
  epochTime <- function() {
    as.integer(Sys.time())
  }
  
  # Shiny app 
  
  shinyApp(
    ui = fluidPage(
      shinyjs::useShinyjs(),
      shinyjs::inlineCSS(appCSS),
      titlePanel("AHP Ecolabels"),
      
      div(
        id = "form",
        
        textInput("name", labelMandatory("Name"), ""),
        textInput("surname", labelMandatory("Surname")),
        #checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
        selectInput("institution_type", "Institution where you work / study",
                    c("",  "UEF", "EFI", "Systembolaget", "other")),
        textInput("otherinst", "If other, please specify the institution name here"),
        
        shinyWidgets::sliderTextInput(inputId = "decade", 
                                      label = "Time (decade):", 
                                      choices = c(5, 4, 3, 2, 1, 0, 1, 2, 3, 4, 5), 
                                      selected = 0),
        
        
        
        actionButton("submit", "Submit", class = "btn-primary"),
        
        #“Submitting…” progress message and an error message
        shinyjs::hidden(
          span(id = "submit_msg", "Submitting..."),
          div(id = "error",
              div(br(), tags$b("Error: "), span(id = "error_msg"))))
      ),
      
      # Thank you message after submission
      shinyjs::hidden(
        div(
          id = "thankyou_msg",
          h3("Thanks, your response was submitted successfully!"),
          actionLink("submit_another", "Submit another response")
        )
      ) 
      
    ),
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
