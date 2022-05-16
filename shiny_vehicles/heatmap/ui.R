library(shiny)
df = read.csv('clean-vehicles.csv',stringsAsFactors = T)
choice.make <- c('all')
for (i in unique(df$manufacturer)){choice.make <- append(choice.make,i)}
choice.type <- c('all')
for (i in unique(df$type)){choice.type <- append(choice.type,i)}
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Used car heatmap"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("make", label = "manufacturer", choices = choice.make),
      selectInput("type", label = "type", choices = choice.type)
    ),
    
    mainPanel(
      plotOutput('heatmap')
    )
  )
))
