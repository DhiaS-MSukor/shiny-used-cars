library(shiny) 
library(comprehenr)
library(tmap)
library(tmaptools)
#library(DT)
# data read 
df = read.csv('clean-vehicles.csv',stringsAsFactors = T)

# add choices for make
choice.manufacturer <- to_vec(for (i in unique(df$manufacturer)) i)
choice.manufacturer <- choice.manufacturer[choice.manufacturer!='unknown']
choice.manufacturer <- choice.manufacturer[order(choice.manufacturer)]
choice.manufacturer <- c('all', choice.manufacturer) 

# add choices for type
choice.type <- to_vec(for (i in unique(df$type)) i)
choice.type <- choice.type[choice.type!='unknown']
choice.type <- choice.type[order(choice.type)]
choice.type <- c('all', choice.type)

# choices for states
choice.state <- to_vec(for (i in unique(df$state)) i)
choice.state <- choice.state[order(choice.state)]
choice.state <- c('all', choice.state) 

# choices for year
choice.year.min <- min(df$year)
choice.year.max <- max(df$year)

# choices for odometer
choice.odometer.min <- min(df$odometer)
choice.odometer.max <- max(df$odometer)

# page for used car heatmap
used_car_heatmap <- fluidPage(
  
  titlePanel("Used car heatmap"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("make", label = "make", choices = choice.manufacturer),
      selectInput("type", label = "type", choices = choice.type)
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Map", tmapOutput("map")),
        tabPanel("Summary", dataTableOutput("summary"))
      )
    )
  )
)

# page for most available used car
available_barchart <- fluidPage( 
  titlePanel("Most available used car"),
  sidebarLayout(
    sidebarPanel(
      selectInput("make_ab", label = "manufacturer", choices = choice.manufacturer),  
      sliderInput('top_ab', 
                  label = 'top n to display', 
                  min = 1, 
                  max = if (length(unique(df$model)) < 101) length(unique(df$model)) else 100, 
                  step = 1, 
                  value = 25),
      sliderInput("year_ab", 
                  label = "Choose year range", 
                  min = choice.year.min, 
                  max = choice.year.max, 
                  step = 1,
                  value = c(choice.year.min, choice.year.max)),
    ),
    
    mainPanel(
      plotOutput('most_available')
    )
  )
)

# page for most available used car
price_boxwhisker <- fluidPage( 
  titlePanel("Prices of typical used cars"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state_pb", label = "state", choices = choice.state),
      selectInput("type_pb", label = "type", choices = choice.type),
      sliderInput("year_pb", 
                  label = "Choose year range", 
                  min = choice.year.min, 
                  max = choice.year.max, 
                  step = 1,
                  value = c(choice.year.min, choice.year.max)),
      sliderInput("odometer_pb", 
                  label = "Choose odometer range", 
                  min = choice.odometer.min, 
                  max = choice.odometer.max, 
                  step = ceiling((choice.odometer.max - choice.odometer.min) / 100),
                  value = c(choice.odometer.min, choice.odometer.max)),
    ),
    
    mainPanel(
      plotOutput('price_boxwhisker') 
    )
  )
)

# main page
main_page <- fluidPage(
  titlePanel("Shiny Used Car"),
  
  navlistPanel( 
    widths=c(2,8), 
    tabPanel("Used car availability heatmap", used_car_heatmap),
    tabPanel("Most available used car", available_barchart), 
    tabPanel("Prices of typical used cars", price_boxwhisker), 
  )
)

# Define UI for application that draws a histogram
shinyUI(main_page)
