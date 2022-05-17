library(shiny)
library(comprehenr)

# data read
df = read.csv('clean-vehicles.csv',stringsAsFactors = T)

# add choices for make
choice.manufacturer <- c('all', to_vec(for (i in unique(df$manufacturer)) i)) 

# add choices for type
choice.type <- c('all', to_vec(for (i in unique(df$type)) i)) 

# add choices for model
choice.model <- c('all', to_vec(for (i in unique(df$model)) i))

# page for used car heatmap
used_car_heatmap <- fluidPage(
  
  titlePanel("Used car heatmap"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("manufacturer", label = "manufacturer", choices = choice.manufacturer),
      selectInput("type", label = "type", choices = choice.type)
    ),
    
    mainPanel(
      tmapOutput("map")
    )
  )
)

# page for most available used car
available_used_car <- fluidPage( 
  titlePanel("Most available used car"),
  sidebarLayout(
    sidebarPanel(
      selectInput("manufacturer", label = "manufacturer", choices = choice.manufacturer),
      selectInput("model", label = "model", choices = choice.model)
    ),
    
    mainPanel(
      plotOutput('most_available')
    )
  )
)

# main page
main_page <- fluidPage(
  titlePanel("Shiny Used Car"),
  
  navlistPanel( 
    widths=c(2,8), 
    tabPanel("Heatmap", used_car_heatmap),
    tabPanel("Most available used car", available_used_car), 
    tabPanel("Component 3"),
    tabPanel("Component 4"), 
    tabPanel("Component 5"),
  )
)

# Define UI for application that draws a histogram
shinyUI(main_page)
