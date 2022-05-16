library(shiny)
df = read.csv('clean-vehicles.csv',stringsAsFactors = T)
choice.make <- c('all')
for (i in unique(df$manufacturer)){choice.make <- append(choice.make,i)}
choice.type <- c('all')
for (i in unique(df$type)){choice.type <- append(choice.type,i)}

# page for used car heatmap
used_car_heatmap <- fluidPage(
  
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
)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Shiny Used Car"),
  
  navlistPanel( 
    widths=c(2,8), 
    tabPanel("Heatmap", used_car_heatmap),
    tabPanel("Component 2"), 
    tabPanel("Component 3"),
    tabPanel("Component 4"), 
    tabPanel("Component 5"),
  )
)
)
