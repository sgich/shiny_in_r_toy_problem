# kmeans-NY-Cluster-app

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)

ui <- fluidPage(
  headerPanel('New York Borough k-means clustering'),
  sidebarPanel(
    #selectInput('xcol', 'Latitude', uber_14['Lat']),
    #selectInput('ycol', 'Longitude', uber_14['Long']),
                #selected = names(uber_14)[[2]]),
    #textOutput ("Top 5"),
    #verbatimTextOutput("text"), 
    numericInput('clusters', 'Choose Cluster count', 5,
                 min = 1, max = 9)
    
  ),
  headerPanel('Preview of Top 5 Data Records'),
  mainPanel(
    tableOutput('table'),
    headerPanel('Cluster Plot'),
    plotOutput('plot1')
    
    
  )
)

server <- function(input, output) {
  
  selectedData <- reactive({
    uber_10_14[, c(2,3)]
  })
    
 
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    #par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  output$table <- renderTable(head(uber_10_14, 5), main=title)
  
  output$text <- renderText({ "top 5t" })
  
}

shinyApp(ui = ui, server = server)

