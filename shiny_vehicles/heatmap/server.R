library(shiny)
library(shinythemes)
library(htmlwidgets)
library(plotly)
library(ggplot2)
library(dplyr)
library(usmap)

df = read.csv('clean-vehicles.csv',stringsAsFactors = T)

# function to display heatmap based on dataframe
manufacturer_type_heatmap_display <- function(df){
  plot_usmap(data = df, values = 'n', labels = T)+
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

# function to display dataframe based on input
manufacturer_type_heatmap_dataframe <- function(input){
  if (input$make=='all' & input$type=='all'){
    df.subset <- df %>% group_by(state) %>% count(sort = T) %>% ungroup()
    manufacturer_type_heatmap_display(df.subset)
  }
  else if (input$make!='all' & input$type=='all'){
    df.make <- split(df,df$manufacturer)
    li_make <- list()
    for (i in 1:length(df.make)){
      li_make[[i]] <- df.make[[i]]%>% 
        group_by(state) %>% 
        count(sort = T) %>% 
        ungroup()
    }
    names(li_make) <- sort(unique(df$manufacturer)) 
    manufacturer_type_heatmap_display(li_make[[input$make]])
  }
  else if (input$make=='all' & input$type!='all'){
    df.type <- split(df,df$type)
    li_type <- list()
    for (i in 1:length(df.type)){
      li_type[[i]] <- df.type[[i]]%>% 
        group_by(state) %>% 
        count(sort = T) %>% 
        ungroup()
    }
    names(li_type) <- sort(unique(df$type)) 
    manufacturer_type_heatmap_display(li_type[[input$type]])
    
  }
  else {
    df['make_type'] <-  paste(df$manufacturer,df$type,sep = '_')
    df$make_type <- as.factor(df$make_type)
    df.make_type <- split(df,df$make_type)
    li_make_type <- list()
    for (i in 1:length(df.make_type)){
      li_make_type[[i]] <- df.make_type[[i]]%>% 
        group_by(state) %>% 
        count(sort = T) %>% 
        ungroup()
    }
    names(li_make_type) <- sort(unique(df$make_type))
    if ((paste(input$make,input$type,sep='_')) %in% (names(li_make_type))){ 
      manufacturer_type_heatmap_display(li_make_type[[paste(input$make,input$type,sep='_')]])
    }
    else {
      df.subset <- df %>% group_by(state) %>% count(sort = T) %>% ungroup()
      df.subset['n'] <- as.integer(NA) 
      manufacturer_type_heatmap_display(df.subset)
    }
  }
}

# Define server logic required to draw a heatmap
shinyServer(function(input, output) {
  output$heatmap <- renderPlot(manufacturer_type_heatmap_dataframe(input))
})
