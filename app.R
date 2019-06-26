library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("bcl-data.csv", stringsAsFactors=FALSE)
# print(paste("rows: ", nrow(bcl), ", ", "cols: ", ncol(bcl)))


ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, value=c(25, 40), pre="$"),
      uiOutput("typeOutput"),
      uiOutput("subtypeOutput"),
      uiOutput("countryOuput")
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      h5(strong((textOutput("numResults")))),
      br(), br(),
      tableOutput("results")
    )
  )
)

#print(ui)

server <- function(input, output) {
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }

    observe({print(paste("subtype = ", input$subtypeInput))})
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Subtype == input$subtypeInput,
             Country == input$countryInput
      )
  })  

  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })

  output$results <- renderTable({
    filtered()
  })
  
  output$numResults <- renderText({
    numResults <- nrow(filtered())
    paste(numResults, "results found")
  })
  
  output$typeOutput <- renderUI({
    radioButtons("typeInput", "Product type",
                 sort(unique(bcl$Type)),
                 selected="SPIRITS"
    )
  })
  
  output$subtypeOutput <- renderUI({
    temp <- bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
    subtypes <- sort(unique(temp$Subtype))
    selectInput("subtypeInput", "Product subtype",
                subtypes,
                selected=ifelse(input$subtypeInput %in% subtypes, input$subtypeInput, subtypes[1])
                )
  })
  
  output$countryOuput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected="CANADA")
  })

}


shinyApp(ui=ui, server=server)  # should have no lines after this