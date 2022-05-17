
library(htmlwidgets)
library(plotly)
library(sf)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tmap)
library(tmaptools)
library(shiny)
library(shinythemes)
df = read.csv('clean-vehicles.csv',stringsAsFactors = T)
#download.file("http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_state_20m.zip", destfile = "states.zip")
#unzip("states.zip")
us_geo<-read_sf("states/cb_2015_us_state_20m.shp")


# Define server logic required to draw a Tmap
shinyServer(function(input, output) {
  output$map <- renderTmap({
    if (input$make=='all' & input$type=='all'){
      df.subset <- df %>% 
        left_join(x = df %>% group_by(state) %>% count(sort = T) %>% ungroup(),
                  y = aggregate(df$price, by=list(type=df$state),mean),
                  by = c('state'='type')) %>% 
        `colnames<-`(c('state','posting number','mean price')) %>%  # change colname
        mutate(state = toupper(state),`mean price` = as.integer(`mean price`)) # convert state.abb to uppercase
      carmap <- left_join(us_geo, df.subset, by = c('STUSPS'='state'), key.data = "full")
      carmap$`posting number` <- as.integer(carmap$`posting number`)
      carmap <- carmap[carmap$NAME!='Alaska'&carmap$NAME!='Hawaii'&carmap$NAME!='Puerto Rico',]
      tm <- tm_shape(carmap, name = 'all make&types',unit = 'km') +
        tm_polygons(col = c('posting number','mean price'), 
                    id = 'NAME', style = "cont",
                    breaks = list(summary(carmap$`posting number`),summary(carmap$`mean price`)),
                    title = c('number of postings','average price'),
                    textNA = list('0','no posting'),
                    palette = get_brewer_pal("YlOrRd", plot = F, n = 10, 
                                             contrast = c(0.17, 0.77))) +
        tm_layout(title = 'The distribution of used-car-postings quantity in US')+
        tm_scale_bar()+
        tm_facets(as.layers = T)+
        tm_text(text = 'STUSPS', size = 0.67)
      tm
      
    }else if (input$make!='all' & input$type=='all'){
      df.make <- split(df, df$manufacturer)
      li_make <- list()
      for (i in 1:length(df.make)){
        li_make[[i]] <- df.make[[i]] %>% 
          left_join(x = df.make[[i]] %>% group_by(state) %>% count(sort = T) %>% ungroup(),
                    y = aggregate(df.make[[i]]$price, by=list(type=df.make[[i]]$state),mean),
                    by = c('state'='type')) %>% 
          `colnames<-`(c('state','posting number','mean price')) %>%  # change colname
          mutate(state = toupper(state),`mean price` = as.integer(`mean price`))    # convert state.abb to uppercase
      }
      names(li_make) <- sort(unique(df$manufacturer))
      carmap <- left_join(us_geo, li_make[[input$make]], by = c('STUSPS'='state'), key.data = "full")
      carmap$`posting number` <- as.integer(carmap$`posting number`)
      carmap <- carmap[carmap$NAME!='Alaska'&carmap$NAME!='Hawaii'&carmap$NAME!='Puerto Rico',]
      tm <- tm_shape(carmap,unit = 'km') +
        tm_polygons(col = c('posting number','mean price'), 
                    id = 'NAME', style = 'pretty',
                    title = c('number of postings','average price'),
                    textNA = list('0','no posting'),
                    palette = get_brewer_pal("YlOrRd", plot = F, n = 10, 
                                             contrast = c(0.17, 0.77))) +
        tm_layout(title = 'The distribution of used-car-postings quantity in US')+
        tm_scale_bar()+
        tm_facets(as.layers = T)+
        tm_text(text = 'STUSPS', size = 0.67)
      tm
    }else if (input$make=='all' & input$type!='all'){
      df.type <- split(df,df$type)
      li_type <- list()
      for (i in 1:length(df.type)){
        li_type[[i]] <- df.type[[i]] %>% 
          left_join(x = df.type[[i]] %>% group_by(state) %>% count(sort = T) %>% ungroup(),
                    y = aggregate(df.type[[i]]$price, by=list(type=df.type[[i]]$state),mean),
                    by = c('state'='type')) %>% 
          `colnames<-`(c('state','posting number','mean price')) %>%  # change colname
          mutate(state = toupper(state),`mean price` = as.integer(`mean price`))    # convert state.abb to uppercase
      }
      names(li_type) <- sort(unique(df$type))
      carmap <- left_join(us_geo, li_type[[input$type]], by = c('STUSPS'='state'), key.data = "full")
      carmap$`posting number` <- as.integer(carmap$`posting number`)
      carmap <- carmap[carmap$NAME!='Alaska'&carmap$NAME!='Hawaii'&carmap$NAME!='Puerto Rico',]
      tm <- tm_shape(carmap,unit = 'km') +
        tm_polygons(col = c('posting number','mean price'), 
                    id = 'NAME', style = 'pretty',
                    title = c('number of postings','average price'),
                    textNA = list('0','no posting'),
                    palette = get_brewer_pal("YlOrRd", plot = F, n = 10, 
                                             contrast = c(0.17, 0.77))) +
        tm_layout(title = 'The distribution of used-car-postings quantity in US')+
        tm_scale_bar()+
        tm_facets(as.layers = T)+
        tm_text(text = 'STUSPS', size = 0.67)
      tm
      
    }else {
      df['make_type'] <-  paste(df$manufacturer,df$type,sep = '_')
      df$make_type <- as.factor(df$make_type)
      df.make_type <- split(df,df$make_type)
      li_make_type <- list()
      for (i in 1:length(df.make_type)){
        li_make_type[[i]] <- df.make_type[[i]] %>% 
          left_join(x = df.make_type[[i]] %>% group_by(state) %>% count(sort = T) %>% ungroup(),
                    y = aggregate(df.make_type[[i]]$price, by=list(type=df.make_type[[i]]$state),mean),
                    by = c('state'='type')) %>% 
          `colnames<-`(c('state','posting number','mean price')) %>%  # change colname
          mutate(state = toupper(state),`mean price` = as.integer(`mean price`))    # convert state.abb to uppercase
      }
      names(li_make_type) <- sort(unique(df$make_type))
      
      if ((paste(input$make,input$type,sep='_')) %in% (names(li_make_type))){
        carmap <- left_join(us_geo, li_make_type[[paste(input$make,input$type,sep='_')]], by = c('STUSPS'='state'), key.data = "full")
        carmap$`posting number` <- as.integer(carmap$`posting number`)
        carmap <- carmap[carmap$NAME!='Alaska'&carmap$NAME!='Hawaii'&carmap$NAME!='Puerto Rico',]
        tm <- tm_shape(carmap,unit = 'km') +
          tm_polygons(col = c('posting number','mean price'), 
                      id = 'NAME', style = 'pretty',
                      title = c('number of postings','average price'),
                      textNA = list('0','no posting'),
                      palette = get_brewer_pal("YlOrRd", plot = F, n = 10, 
                                               contrast = c(0.17, 0.77))) +
          tm_layout(title = 'The distribution of used-car-postings quantity in US')+
          tm_scale_bar()+
          tm_facets(as.layers = T)+
          tm_text(text = 'STUSPS', size = 0.67)
        tm
        
      }else {
        df.subset <- df %>% group_by(state) %>% count() %>% ungroup() %>% 
          mutate(state = toupper(state), n = 0)
        carmap <- left_join(us_geo, df.subset, by = c('STUSPS'='state'), key.data = "full")
        carmap <- carmap[carmap$NAME!='Alaska'&carmap$NAME!='Hawaii'&carmap$NAME!='Puerto Rico',]
        tm <- tm_shape(carmap, unit = 'km')+
          tm_polygons(col = 'n', 
                      id = 'NAME', style = 'pretty',
                      title = 'number of postings',
                      textNA = '0',
                      palette = 'gray') +
          tm_layout(title = 'The distribution of used-car-postings quantity in US')+
          tm_scale_bar()+
          tm_text(text = 'STUSPS', size = 0.67)
        tm
      }
    }
  })
})