D_Trend = reactive({

  R = DATA()
  R
})

L_Paired_Trend = reactive({
  
  withProgress(message = 'Calculating long term statistics',{
  
  cat("\nPairing calucation starts for Trend")
  
  L = list()
  
  # if('SM75' %in% D_Trend()$ST){
  # 
  # } else {
  
  # if(input$station75 == 'No'){
  #    LEVELS = LEVELS[LEVELS != 'SM75']
  # }
  # }
  
  print(LEVELS)
  
  for(i in 1:(length(LEVELS)-1)){
    
    FROM = LEVELS[i]
    TO = LEVELS[i+1]
    
    DATA = D_Trend() %>% arrange(Time) %>% filter(ST %in% c(FROM, TO))
    
    print(head(DATA))
    
    require(reshape2)
    DATA$Time = as.character(DATA$Time)
    DATA2 = dcast(DATA, ID+Segment~ST, value.var = c('Time'), fun.aggregate = function(x) x[1])
    
    if(FROM %in% names(DATA2) & TO %in% names(DATA2)){
      DATA2$From = DATA2[,FROM]
      DATA2$To = DATA2[,TO]
      

      print(head(DATA2))
      
      DATA2$From = lubridate::ymd_hms(DATA2$From)
      DATA2$To = lubridate::ymd_hms(DATA2$To)
      
      DATA3 = DATA2 %>% arrange(To)
      
      T_station = c(NA,Time_Vector_Diff(DATA3$To))
      T_piece = as.numeric(as.duration(DATA3$To - DATA3$From))
      
      #  print(head(DATA3))
      
      R = data.frame(FROM, From_Time = DATA3$From, TO, To_Time = DATA3$To, T_station, T_piece, Segment = DATA3$Segment)
      #R = na.omit(R)
      
      R = R %>% filter( T_piece < 100, T_station <100)
      
      L[[paste0(FROM,'_',TO)]] = R
      
    }
    
    
    
    
  }
  
  })
  
  
  print(paste('L length:', length(L)))
  print(names(L))
  #    print(L[['SM90_SM100']])
  L
})



L_Paired_Adj_Trend = reactive({
  withProgress(message = 'Adjusting station to station time for parallel stations:30, 80 and 140',{
  L = L_Paired_Trend()
  L$SM26.1_SM30$T_piece = L$SM26.1_SM30$T_piece/2
  L$SM76.2_SM80$T_piece = L$SM76.2_SM80$T_piece/2
  L$SM130_SM140$T_piece = L$SM130_SM140$T_piece/2
  })
  L
})

output$Trend_Table = renderTable({
  D_Paired_Summary_Trend()[1:100,]
})

D_Paired_Summary_Trend = reactive({
  
  L = L_Paired_Adj_Trend()
  
#  BREAKS = paste0(input$seg, ' min')
  
  LL = lapply(L, function(df){
    
    print(head(df))
    
    df$Seg = df$Segment
    S = df %>% group_by(Seg) %>% do({
      tmp = score_Station_2(., Target = TARGET())
      SCORES_upper = tmp[6]
      SCORES_lower = tmp[12]
      SCORES_total = tmp[14]
      Ratio =  round((SCORES_total)/(SCORES_total+TARGET()),3)
      N = nrow(.)
      #Failure = sum(.$State!='OK')
      Available = round(with(., sum(T_station <= T_piece)/N), 3)
      #cat('-------D_Paired_Summary_Trend----------\n')
      #print(head(.))

      ST = .$TO[1]
      R = data.frame(ST, SCORES_upper, SCORES_lower, SCORES_total, Ratio, N, Available)
      rownames(R) = NULL
      
      R
    })
    S  
  })
  
  D = do.call(rbind, LL)
  # write.csv(D, 'Longterm_tmp.csv')
  D
})

output$Trend = renderPlot({
  D = D_Paired_Summary_Trend()
  #D$Seg = lubridate::ymd_hms(D$Seg)
  D$ST = factor(D$ST, levels = LEVELS)
  D$Group = ifelse(D$Ratio >0, '1 - Time Loss','2 - No Time Loss')
  
  D$DATE = substr(D$Seg,1,10)
  D$SHIFT = substr(D$Seg, 14, length(D$Seg))
  
  P = ggplot(D, aes(x = Seg, y = Ratio, shape = SHIFT)) + geom_path(aes(color = Group, group =1)) + 
    theme_bw()+
    facet_wrap(~ST, scales = 'free_y') +
    geom_point(aes(color = Group), size = ggplot2::rel(4)) +
    geom_hline(yintercept = 0, linetype = 'dashed', color = 'blue') +
    ylab('Time Loss Percent') +
    xlab('Datetime') +
    scale_y_continuous(labels = scales::percent) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_color_manual(values = c('1 - Time Loss' = 'red','2 - No Time Loss' = 'blue'))
  if(input$trend_y_scale != 'Free scale')
    P = P + scale_y_continuous(labels = scales::percent, limits = c(-0.4, 0.4))

  P
})
