library(shiny)

ui <- fluidPage(
  selectInput("download", "Select Data to download", choices = c("euro", "mtcars", "iris")),
  downloadButton("downloadData")
)

server <- function(input, output, session) {
  
  dataDownload <- reactive({
    switch(input$download,
           "euro" = euro,
           "mtcars" = mtcars,
           "iris" = iris)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$download, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataDownload(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)