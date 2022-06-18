
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
library(RColorBrewer) 
library(hrbrthemes)
library(viridis)
library(tidyr)
library(forcats)

df = read.csv('clean-vehicles.csv',stringsAsFactors = T)
#download.file("http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_state_20m.zip", destfile = "states.zip")
#unzip("states.zip", exdir = 'states')
us_geo<-read_sf("states/cb_2015_us_state_20m.shp")


used_car_tmap <- function(input){
  renderTmap({ 
    if (input$make=='all' & input$type=='all'){
      df.subset <- df %>% 
        left_join(x = df %>% group_by(state) %>% count(sort = T) %>% ungroup(),
                  y = aggregate(df$price, by=list(type=df$state),mean),
                  by = c('state'='type')) %>% 
        `colnames<-`(c('state','posting number','mean price')) %>%  # change colname
        mutate(state = toupper(state),`mean price` = as.integer(`mean price`)) # convert state.abb to uppercase
      carmap <- left_join(us_geo, df.subset, by = c('STUSPS'='state'), key.data = "full")
      df.state <- split(df, df$state)
      pop.make1 <- to_vec(for (i in 1:51) names(sort(table(df.state[[i]]$manufacturer),decreasing=TRUE)[1]))
      pop.make2 <- to_vec(for (i in 1:51) names(sort(table(df.state[[i]]$manufacturer),decreasing=TRUE)[2]))
      pop.make3 <- to_vec(for (i in 1:51) names(sort(table(df.state[[i]]$manufacturer),decreasing=TRUE)[3]))
      df.pop.make <- data.frame(toupper(levels(df$state)),pop.make1,pop.make2,pop.make3)
      names(df.pop.make) <- c('STUSPS','No.1 popular','No.2 popular','No.3 popular')
      carmap <- left_join(carmap, df.pop.make, by = 'STUSPS')
      carmap <- carmap[carmap$NAME!='Alaska'&carmap$NAME!='Hawaii'&carmap$NAME!='Puerto Rico',]
      tm <- tm_shape(carmap, unit = 'km') +
        tm_polygons(col = c('posting number','mean price'), 
                    id = 'NAME', style = "cont",
                    breaks = list(summary(carmap$`posting number`),summary(carmap$`mean price`)),
                    title = c('number of postings','average price'),
                    textNA = list('0','no posting'),
                    popup.vars = c('posting number','mean price','No.1 popular','No.2 popular','No.3 popular'),
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
}

used_car_tmap_smry <- function(input){
  renderDataTable({ 
    if (input$make=='all' & input$type=='all'){
      df.subset <- df %>% 
        left_join(x = df %>% group_by(state) %>% count(sort = T) %>% ungroup(),
                  y = aggregate(df$price, by=list(type=df$state),mean),
                  by = c('state'='type')) %>% 
        `colnames<-`(c('state','posting number','mean price')) %>%  # change colname
        mutate(state = toupper(state),`mean price` = as.integer(`mean price`)) # convert state.abb to uppercase
      us_geo <- select(as.data.frame(us_geo),c('NAME','STUSPS'))
      smry <- left_join(df.subset,us_geo, by = c('state'='STUSPS'), key.data = "full")
      smry <- select(smry,c('NAME','posting number','mean price'))
      colnames(smry)[1] <- 'state'
      smry
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
      us_geo <- select(as.data.frame(us_geo),c('NAME','STUSPS'))
      smry <- left_join(li_make[[input$make]],us_geo, by = c('state'='STUSPS'), key.data = "full")
      smry <- select(smry,c('NAME','posting number','mean price'))
      colnames(smry)[1] <- 'state'
      smry
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
      us_geo <- select(as.data.frame(us_geo),c('NAME','STUSPS'))
      smry <- left_join(li_type[[input$type]],us_geo, by = c('state'='STUSPS'), key.data = "full")
      smry <- select(smry,c('NAME','posting number','mean price'))
      colnames(smry)[1] <- 'state'
      smry
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
      us_geo <- select(as.data.frame(us_geo),c('NAME','STUSPS'))
      if ((paste(input$make,input$type,sep='_')) %in% (names(li_make_type))){
        smry <- left_join(li_make_type[[paste(input$make,input$type,sep='_')]],us_geo, by = c('state'='STUSPS'), key.data = "full")
        smry <- select(smry,c('NAME','posting number','mean price'))
        colnames(smry)[1] <- 'state'
        smry
      }else {
        df.subset <- df %>% 
          left_join(x = df %>% group_by(state) %>% count(sort = T) %>% ungroup(),
                    y = aggregate(df$price, by=list(type=df$state),mean),
                    by = c('state'='type')) %>% 
          `colnames<-`(c('state','posting number','mean price')) %>%  # change colname
          mutate(state = toupper(state),`mean price` = as.integer(`mean price`)) # convert state.abb to uppercase
        us_geo <- select(as.data.frame(us_geo),c('NAME','STUSPS'))
        smry <- left_join(df.subset,us_geo, by = c('state'='STUSPS'), key.data = "full")
        smry <- select(smry,c('NAME','posting number','mean price'))
        colnames(smry)[1] <- 'state'
        smry$`posting number` <- 0
        smry$`mean price` <- 'no data'
        smry
      }
    }
  })
}

available_barchart <- function(input, session){
  act1 <- reactive({   
    data <- df[,c('year',"manufacturer","model")]    
    
    if (input$make_ab != 'all'){
      data <- data[data$manufacturer == input$make_ab,]  
    } 
    
    data <- data[data$year >= input$year_ab[1] & data$year <= input$year_ab[2],]
    data$make_model <- paste(data$manufacturer, data$model)
    data <- data %>% group_by(make_model) %>% count() 
    
    data <- data[order(data$n, decreasing = T),]
    data <- data[0:input$top_ab,]
    return(data)
  })   
  
  renderPlot({
    act1() %>%
      ggplot(aes(x=reorder(make_model, -n), y=n, fill=make_model)) +
      geom_bar(stat = "identity", color='black') +
      xlab("") +
      ylab("Number of postings") +
      theme(legend.position="none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }, height = 750) 
}

price_box <- function(input){
  act1 <- reactive({   
    data <- df[df$year >= input$year_pb[1] 
               & df$year <= input$year_pb[2]
               & df$odometer >= input$odometer_pb[1]
               & df$odometer <= input$odometer_pb[2]
               , c("manufacturer",'price', 'type', 'model','state')]  
    
    if (input$type_pb != 'all'){
      data <- data[data$type == input$type_pb,]
    }
    if (input$state_pb != 'all'){
      data <- data[data$state == input$state_pb,]
    } 
    
    data$make_model <- paste(data$manufacturer, data$model)
    
    grouped <- data %>% group_by(make_model) %>% count()
    grouped <- grouped[order(grouped$n, decreasing = T),]
    grouped <- grouped[0:20,]
    
    data <- merge(data,grouped, by='make_model')
    data <- data %>% mutate(make_model = fct_reorder(make_model, price))
    
    return(data)
  })   
  
  renderPlot({   
    act1() %>% 
      ggplot() +
      geom_violin(width=1.0, mapping = aes(x=make_model, y=price, color='black', fill=make_model)) +
      geom_boxplot(width=0.1, mapping = aes(x=make_model, y=price, color='black')) +  
      theme(
        legend.position="none"
      ) +
      coord_flip() + # This switch X and Y axis and allows to get the horizontal version
      xlab("") +
      ylab("Price (US$)")
  }, height = 750) 
}

type_make_trend <- function(input, method){
  act1 <- reactive({   
    data <- df[df$manufacturer %in% input$make_tr
               , c('price', 'type', 'year', 'manufacturer')]      
    
    return(data)
  })   
  
  renderPlot({   
    act1() %>% 
      ggplot(aes(x=year, y=price, group=manufacturer, color=manufacturer, fill=manufacturer)) +  
      geom_smooth(method=method, se=TRUE) +
      xlab("") +
      ylab("Price (US$)")+ 
      facet_wrap(~type, ncol=3)
  }, height = 750) 
}

type_make_line<- function(input){
  act1 <- reactive({   
    data <- df[df$manufacturer %in% input$make_tr
               , c('price', 'type', 'year', 'manufacturer')]      
    
    data$make_type_year <- paste(data$manufacturer, data$type, data$year, sep='_')
    data <- data %>% group_by(make_type_year) %>% summarise(price=median(price))
    data <- data %>% separate(make_type_year, c('manufacturer','type', 'year'), extra='drop', sep='_') 
    
    return(data)
  })   
  renderPlot({   
    act1() %>% 
      ggplot(aes(x=year, y=price, group=manufacturer, color=manufacturer, fill=manufacturer)) + 
      geom_point(shape=21, size=2) +  
      geom_line()+
      xlab("") +
      ylab("Median price (US$)")+ 
      theme( 
        panel.spacing = unit(0.1, "lines"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(size=14)
      )+ 
      facet_wrap(~type, ncol=3)
  }, height = 750) 
}

type_line <- function(input){
  act1 <- reactive({   
    data <- df[df$type %in% input$type_tl
               , c('price', 'type', 'year')]
    
    data$type_year <- paste(data$type, data$year, sep='_')
    data <- data %>% group_by(type_year) %>% summarise(price=median(price))
    data <- data %>% separate(type_year, c('type', 'year'), extra='drop', sep='_') 
    
    return(data)
  })   
  
  renderPlot({   
    act1() %>% 
      ggplot(aes(x=year, y=price, group=type, color=type, fill=type)) + 
      geom_point(shape=21, size=6) +  
      geom_line(size=1.2)+
      xlab("") +
      ylab("Median price (US$)")
  }, height = 750) 
}

type_facetline <- function(input){
  act1 <- reactive({   
    data <- df[, c('price', 'type', 'year')]
    
    data$type_year <- paste(data$type, data$year, sep='_')
    data <- data %>% group_by(type_year) %>% summarise(price=median(price))
    data <- data %>% separate(type_year, c('type', 'year'), extra='drop', sep='_')
    data$type2 <- data$type
    
    return(data)
  })   
  
  renderPlot({   
    act1() %>% 
      ggplot(aes(x=year, y=price, group=type)) +  
      geom_line( data=act1() %>% dplyr::select(-type), aes(group=type2), color="grey") +
      geom_line( aes(color=type), size=1.2)+
      xlab("") +
      ylab("Median price (US$)")+
      theme(
        legend.position="none",
        panel.spacing = unit(0.1, "lines"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(size=14)
      )+ 
      facet_wrap(~type, ncol=3)
  }, height = 750) 
}

type_bar <- function(input){
  df['type_fuel'] <- paste(df$type,df$fuel,sep = '_')
  sub_df <- aggregate(df$price, by=list(type=df$type_fuel),mean)
  sub_df[c('type', 'fuel')] <- stringr::str_split_fixed(sub_df$type, '_', 2)
  sub_df <- sub_df[sub_df$type!='unknown'&sub_df$fuel!='unknown',]
  colnames(sub_df)[2] <- 'price'
  renderPlot({
    ggplot(sub_df, aes(x = type, y = price, fill = fuel))+
      geom_bar(position = 'dodge', stat="identity", width = 0.7)+
      xlim(input$type_gb)
  })
}

user.manual <- function(input){
  renderText({
    HTML(paste(
      '','<b>User Manual</b>',
      '','Tab 1: Used car availability heatmap',
      '','INPUT: Users can input their selection of “manufacturer” and “type” of used vehicles.',
      '','OUTPUT: On the “map” tab, this reveals a heat map of the US where the colours indicate the number of postings and the average price of the used vehicles. A “summary” tab is also included to show a table that allows for ease of referencing.',
      '',
      '','Tab 2: Most available used cars',
      '','INPUT: Users can input their selection of “manufacturer”, a range of “year(s)”, and the “number” of selections that they wish to display of used vehicles.',
      '','OUTPUT: A bar graph is generated revealing all the various manufacturer’s top most widely available used cars based on the selected time period.',
      '',
      '','Tab 3: Price of top 20 most common used cars',
      '','INPUT: Users can input their selection of “state”, “type”, a “year(s)” range, and an “odometer” range.',
      '','UTPUT: A violin and box and whisker diagram is generated to show the prices of the top 20 most common used cars based on the user’s selected input.',
      '',
      '','Tab 4: Prices of cars for each type of fuel',
      '','INPUT: Users can input their selection of one or more used vehicle “types”.',
      '','OUTPUT: A grouped histogram is generated containing the breakdown for the prices of each of the five different fuel types for comparison.',
      '',
      '','Tab 5: Price trends of used cars by type',
      '','INPUT: Users can input their selection of one or more used vehicle “type”.',
      '','OUTPUT: A line graph showing the median prices of the selected used vehicle type across the years, from 1998-2020.',
      '',
      '','Tab 6: Price trends of used cars by manufacturer and type',
      '','INPUT: Users can input their selection of “manufacturer” of the used vehicle.',
      '','OUTPUT: A line graph showing the median price trends of used vehicles separated by manufacturer and type across the years from 1998-2020 is generated.', sep = "<br/>"
    ))
  })
}

about.df <- function(input){
  renderText({
    HTML(paste(
      '','<b>About the Dataset</b>',
      '','The dataset is obtained from Kaggle, where used car data is web scraped from the second-hand platform Craigslist in 2021.',
      '','The explanation of each of the data attributes are as follows:',
      '','  1. ID —> unique identification of each used vehicle',
      '  2. Region —> Region in the US where the used vehicle is listed',
      '  3. Price —> The price at each used vehicle is listed',
      '  4. Year —> The year of when each used vehicle is manufactured',
      '  5. Manufacturer —> The manufacturer of the used vehicle',
      '  6. Model —> The model of the used vehicle',
      '  7. Condition —> The condition of the used vehicle',
      '  8. Cylinders —> The number of cylinders (horsepower) of each used vehicle',
      '  9. Fuel —> The fuel type that each used vehicle requires',
      '  10. Odometer —> The distance that each used vehicle has driven',
      '  11. Transmission —> Whether the used vehicle is manual or automatic',
      '  12. Drive —> Whether the used vehicle uses 4-wheel drive, rear wheel drive, or front wheel drive',
      '  13. Size —> How large is each used vehicle',
      '  14. Type —> The classification of each used vehicle (eg. Sedan, Bus, Coupe, etc.)',
      '  15. Paint Colour —> The colour of the used vehicle',
      '  16. State —> The state of where the used vehicle is located',
      '  17. Latitude —> The latitude of where the used vehicle is located',
      '  18. Longitude —> The longitude of where the used vehicle is located',
      '  19. Posting Year —> The year of when each used vehicle is posted', sep = "<br/>"
    ))
  })
}

about.us <- function(input){
  renderText({
    HTML(paste(
      '','<b>About Us</b>',
      '','We are Group 4: The Slow and Steady 5, students of Dr Salimah’s WQD7001: Principles of Data Science class. This group project is part of the requirements of that class.',
      'Group members: ',
      '-   Chan Jie Min (S2141167)',
      '-   Dhia Syahmie Muhamad Sukor (S2147929)',
      '-   Muhammad Shakyr Bin Rosman (S2152185)',
      '-   Ruixue Zhang (S2142119)',
      '-   Huijun Liu (S2142285)',
      '','<b>References and resources</b>',
      '','  * GitHub repository: <a href="https://github.com/DhiaS-MSukor/shiny-used-cars" rel="nofollow">https://github.com/DhiaS-MSukor/shiny-used-cars</a>',
      '  * Dataset: <a href="https://www.kaggle.com/datasets/austinreese/craigslist-carstrucks-data" rel="nofollow">https://www.kaggle.com/datasets/austinreese/craigslist-carstrucks-data</a>',
      '','<b>Personal reflections of group project experience</b>',
      'There have been many learning experiences throughout this semester.',
      '','-   Chan Jie Min: Coming from a non-STEM background, this semester has been extremely challenging. However, I embrace the opportunity to up-skill myself, especially in R programming and computer science in general. In particular, I have learnt that not all problems can be solved with data science. It truly depends on various factors such as the availability of a suitable dataset, the hacking skills of the data scientist, as well as the domain knowledge in which the problem lies.',
      '','-   Dhia Syahmie Muhamad Sukor: Before beginning programming the shiny app, we had to handle a challenging dataset that was filled with many missing variables as well as inconsistent observations. This meant that we had to impute missing variables and standardise formatting. In addition to that was the large size of the raw dataset which made processing it rather resource intensive, not to mention time consuming. Thus, I had to very quickly learn of the best ways to clean the data so that the team could begin the data science process. This meant collaborating with teammates on Github to ensure that we were all on the same page, which kept us constantly engaged.',
      '','-   Muhammad Shakyr Bin Rosman: I understand that choosing an appropriate dataset is one of the fundamental aspect of ensuring that a data science project starts out right. Scouring through the internet for useful second-hand datasets that fulfils the criteria of problems that we are attempting to resolve is difficult, but I learned that there are many useful resources that are available thanks to a community that prioritises the sharing of data as well the reproducibility of data science work. I am grateful that these resources exist and understand the importance of contributing myself in the future.',
      '','-   Ruixue Zhang: I was tasked with the programming of the shiny app, alongside another teammate. We explored various different types of data analysis to find out which graphs were the most useful. This meant reiterating the process of cleaning, exploring, and finally the creation of the shiny app multiple times. I remember that just by creating a heat map of the US to depict the number of cars and sale price of the cars took more than two days.',
      '','-   Huijun Liu: This data science project has given me the opportunity to learn how to apply the data science process from start to finish. Working together in a team allows us to rely on each other whenever we fall short, for example, I have learnt many new packages and methods in R programming just from simply observing the code that is produced by my teammates. In turn, I am also able to share my expertise with the group and further the goals of the project as a whole.',
      '','As a group, we would like to sincerely thank Dr Salimah from the bottom of our hearts for putting such care and thought into her lesson plans when teaching our class. Her unique teaching style has caused some much needed stress that propelled us into ensuring that work submitted is to the best of our abilities. All the best in your retirement, Dr.', sep = "<br/>"
    ))
  })
}
# Define server logic required to draw a Tmap
shinyServer(function(input, output, session) {
  output$map <- used_car_tmap(input)
  output$summary <- used_car_tmap_smry(input)
  output$most_available <- available_barchart(input, session)
  output$price_boxwhisker <- price_box(input)  
  output$grouped_barplot <- type_bar(input) 
  output$type_make_line <- type_make_line(input)
  output$type_make_gam <- type_make_trend(input, 'gam')
  output$type_make_lm <- type_make_trend(input, 'lm')
  output$type_make_loess <- type_make_trend(input, 'loess')
  output$type_line <- type_line(input)
  output$type_facetline <- type_facetline(input)
  output$user.manual <- user.manual(input)
  output$about.df <- about.df(input)
  output$about.us <- about.us(input)
})