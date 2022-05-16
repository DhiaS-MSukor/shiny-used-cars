library(shiny)
library(shinythemes)
library(htmlwidgets)
library(plotly)
library(ggplot2)
library(dplyr)
library(usmap)

df = read.csv('clean-vehicles.csv',stringsAsFactors = T)

# Define server logic required to draw a heatmap
shinyServer(function(input, output) {
  output$heatmap <- renderPlot({
    if (input$make=='all' & input$type=='all'){
      df.subset <- df %>% group_by(state) %>% count(sort = T) %>% ungroup()
      plot_usmap(data = df.subset, values = 'n', labels = T)+
        labs(title = 'The distribution of used-car-postings quantity in US')+
        scale_fill_gradientn(colors=rev(heat.colors(10)),na.value = 'grey',
                             guide = guide_colourbar(barwidth = 24, 
                                                     barheight = .3,
                                                     direction = 'horizontal',
                                                     title = 'number of postings',
                                                     title.position = "top"))+
        theme(plot.title = element_text(size=15), 
              legend.position = "bottom",
              legend.title = element_text(size=10), 
              legend.text = element_text(size=5))
     }else if (input$make!='all' & input$type=='all'){
       df.make <- split(df,df$manufacturer)
       li_make <- list()
       for (i in 1:length(df.make)){
         li_make[[i]] <- df.make[[i]]%>% 
           group_by(state) %>% 
           count(sort = T) %>% 
           ungroup()
       }
       names(li_make) <- sort(unique(df$manufacturer))
       plot_usmap(data = li_make[[input$make]], values = 'n', labels = T)+
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
     }else if (input$make=='all' & input$type!='all'){
       df.type <- split(df,df$type)
       li_type <- list()
       for (i in 1:length(df.type)){
         li_type[[i]] <- df.type[[i]]%>% 
           group_by(state) %>% 
           count(sort = T) %>% 
           ungroup()
       }
       names(li_type) <- sort(unique(df$type))
       plot_usmap(data = li_type[[input$type]], values = 'n', labels = T)+
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
       
     }else {
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
         plot_usmap(data = li_make_type[[paste(input$make,input$type,sep='_')]], values = 'n', labels = T)+
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
       }else {
         df.subset <- df %>% group_by(state) %>% count(sort = T) %>% ungroup()
         df.subset['n'] <- as.integer(NA)
         plot_usmap(data = df.subset, values = 'n', labels = T)+
           labs(title = 'The distribution of used-car-postings quantity in US',
                tag = "There is no posting of this manufacturer and type you selected ")+
           scale_fill_gradientn(colors=rev(heat.colors(10)),na.value = 'grey')+
           theme(plot.title = element_text(size=15), 
                 plot.tag.position = 'bottom')
      }
     }
  })
})
