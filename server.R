library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)

source('functions.R', local = TRUE)
source('global.R', local = TRUE)

shinyServer(function(input, output, session) {
  
  TARGET = eventReactive(input$confirm_target, {
    input$target
  })
  
  DATA = reactive({
    withProgress(message = 'Loading data', {
      
      if(input$adjust_delta == TRUE){
        #R = readRDS('data/data_adj.rda')
        R = readRDS('C:/ShinyApps/DNOX_3/data/data_adj.rda')
      } else {
        #R = readRDS('data/data.rda')
        R = readRDS('C:/ShinyApps/DNOX_3/data/data.rda')
      }
      
      
      
    })
    
    R = R %>% filter(ST != 'SM160')
    
    R
  })
  
  output$DATE = renderUI({
    D = DATA()
    choices = sort(unique(D[, 'TrueDate']))
    selected = choices[1]
    selectInput('date','Date',choices = choices, selected = selected)
  })
  
  output$SHIFT = renderUI({
    D = DATA()
    choices = sort(unique(D[, 'Shift']))
    selected = choices[1]
    selectInput('shift','Shift',choices = choices, selected = selected)
  })
  
#  DATA_2 = eventReactive(input$run_deepdive,{
  DATA_2 = reactive({
      
    req(input$date, input$shift)
    D = DATA()
    INDEX_DATE = D[,'TrueDate'] %in% input$date
    INDEX_SHIFT = D[,'Shift'] %in% input$shift
    
    R = D[INDEX_DATE & INDEX_SHIFT, ]
    cat('----- selected data dim ------\n')
    print(dim(R))

    R
  })
  
  
  output$TIME_INTERVAL = renderUI({
    X = DATA_2()
    X$Time = X$Time - 8*60*60
    sliderInput('time_interval', NULL, min = min(X$Time), max = max(X$Time), step = 5*60,
                dragRange = TRUE,
                value = c(max(X$Time) - 60*60, max(X$Time)),
                width = 1500
                
    )
  })
  
  
  output$flight_chart = renderPlot({
    
    req(input$time_interval)
    
    time_interval = input$time_interval
    
    withProgress(message = 'Making flight chart',{
    
    DIFF = 8*60*60  
      
    X = DATA_2() %>% 
      filter(ST != 'SM26.2', Time >time_interval[1] + DIFF, Time < time_interval[2] + DIFF ) %>%
      arrange(ID, Time)
    
    X = na.omit(X)
    
    X$ST = factor(X$ST, levels = rev(LEVELS))
    P1 = ggplot(X, aes(x = ST, y = Time, group = ID, label = Time, color = State)) + 
      geom_path() + 
      #  geom_point() +
      coord_flip() +
      theme_bw() +
      scale_colour_manual(values = c( 'NOK'="red", 'OK'="black")) +
      theme(legend.position="none")
    
    X$ST = factor(X$ST, levels = LEVELS)
    
    print(head(X))    
    
    colfunc <- colorRampPalette(c("blue", "red"))

    sensitivity = 20 
    
    COLORS = colfunc(sensitivity)
    
    SEQ = seq(90,360, length.out = sensitivity)
    
    L = lapply(1:length(SEQ), function(i){
      
      cutoff = SEQ[i]
      
      TMP = X %>% arrange(ST, Time) %>% dplyr::group_by(ST) %>% do({
        data.frame(.[1:(nrow(.)-1),], Diff = Time_Vector_Diff(.$Time))
      }) %>% filter(Diff > cutoff)
      TMP2 = TMP[TMP$Time == min(TMP$Time), ]
      if(nrow(TMP2)>1) TMP2 = TMP2[1,]
      Bottle_ST = as.character(TMP2$ST)
      Bottle_ST = TMP2$ST
      
      Bottle_Timestamp = TMP2$Time
      if(nrow(TMP2)==1) R = data.frame(Bottle_ST = Bottle_ST, Bottle_Timestamp = Bottle_Timestamp, COLOR = COLORS[i])
      if(nrow(TMP2)!=1) R = data.frame(Bottle_ST = NA, Bottle_Timestamp = NA, COLOR = NA)
      
      # if(is.null(R)) R = data.frame(Bottle_ST = NA, Bottle_Timestamp = NA)
      # print(R)
      R
    })
    
    S = do.call(rbind,L)
    
    S = na.omit(S)
    
    S = S[!duplicated(S$Bottle_ST,fromLast = T),]
    
    print(S)
    
    ST_v = S$Bottle_ST
    TS_v = S$Bottle_Timestamp
    COLOR = S$COLOR
    
    print(ST_v)
    print(TS_v)
    
    P1 = P1 + annotate('text', x = ST_v, y = TS_v, label = ST_v, color = COLOR)

    
    })
    
    P1    
  })
  
  source('trend.R', local = TRUE)
  source('deepdive.R', local = TRUE)
  source('control.R', local = TRUE)
  
  
  
})
