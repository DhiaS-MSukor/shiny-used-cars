library(shiny)
library(shinythemes)
library(htmlwidgets)
library(plotly)
library(ggplot2)
library(dplyr)
library(usmap)
library(comprehenr)
library(tidyr)

# read data
df = read.csv('clean-vehicles.csv',stringsAsFactors = T)

# function to display heatmap based on dataframe
manufacturer_type_heatmap_display <- function(df1){
  plot_usmap(data = df1, values = 'n', labels = T)+
    labs(title = 'The distribution of used-car-postings quantity in US')+
    scale_fill_gradientn(colors=rev(heat.colors(10)),na.value = 'grey',
                         guide = guide_colourbar(barwidth = 24, 
                                                 barheight = .3,
                                                 direction = 'horizontal',
                                                 title = 'number of postings (0 represented by grey colour)',
                                                 title.position = "top"))+
    theme(plot.title = element_text(size=15), 
          legend.position = "bottom",
          legend.title = element_text(size=10), 
          legend.text = element_text(size=5))
}

# function to filter by column name(s)
df_filter <- function(input, column){
  # function to group by state and count and then ungroup
  group_count_ungroup <- function(df1){
    df1 %>% group_by(state) %>% count(sort = T) %>% ungroup()
  } 
  
  # function to create new composite factor by given columns
  make_composite_factor <- function(df1, columns){ 
    df_copy = data.frame(df[, columns]) 
    df_copy <- df_copy %>% unite(col='factors', all_of(columns), remove = T)
    factors <- df_copy[,'factors']  
    as.factor(factors)
  }
     
  # if many columns to filter 
  column <- to_vec(for (c in column) if(reactiveValuesToList(input)[c] != 'all') c)
  if (length(column) > 1){
    
    df_copy = data.frame(df)
    df_copy$composite_factor = make_composite_factor(df_copy, names(column))  
    
    df_split <- split(df_copy, df_copy$composite_factor)
    df_temp <- lapply(df_split, group_count_ungroup) 
    names(df_temp) <- sort(unique(df_copy$composite_factor))
    
    get_input_by_column <- function(x) input[[x]]
    
    input_factor <- paste(c(lapply(names(column), get_input_by_column)),collapse ='_') 
    # if input factor is valid
    if (input_factor %in% (names(df_temp))){
      df_temp[[input_factor]]
    } 
    else{
      df_temp <- group_count_ungroup(df) 
      df_temp['n'] <- as.integer(NA)
      df_temp
    }
  }
  # if column to filter is unrestricted
  else if (is.null(column) ){
    group_count_ungroup(df) 
  }
  else if (input[[column]] == 'all'){
    group_count_ungroup(df) 
  }
  # when only one column to filter
  else{  
    df_split <- split(df, df[, column])
    df_temp <- lapply(df_split, group_count_ungroup) 
    names(df_temp) <- sort(unique(df[, column])) 
    
    df_temp[[input[[column]]]] 
  } 
}

# function to display dataframe based on input
manufacturer_type_heatmap_dataframe <- function(input){  
  df_temp = df_filter(input, c('manufacturer', 'type'))
  manufacturer_type_heatmap_display(df_temp) 
}

# Define server logic required to draw a heatmap
shinyServer(function(input, output) {
  output$heatmap <- renderPlot(manufacturer_type_heatmap_dataframe(input))
})
