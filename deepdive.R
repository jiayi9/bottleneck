L_Paired = reactive({
  
  
  
  withProgress(message = 'Pairing up Piece-to-piece and station-to-station times',{
  
  cat("\nPairing calucation starts")
  
  L = list()
  
  D = DATA_2()
  
  if('SM75' %in% D$ST){
    
  } else {
    LEVELS = LEVELS[LEVELS != 'SM75']
  }
  
  for(i in 1:(length(LEVELS)-1)){
    
    FROM = LEVELS[i]
    TO = LEVELS[i+1]
    
    DATA = D %>% arrange(Time) %>% filter(ST %in% c(FROM, TO))
    
    print(head(DATA))
    
    require(reshape2)
    DATA$Time = as.character(DATA$Time)
    DATA2 = dcast(DATA, ID~ST, value.var = 'Time', fun.aggregate = function(x) x[1])
    
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
      
      R = data.frame(FROM, From_Time = DATA3$From, TO, To_Time = DATA3$To, T_station, T_piece)
      #R = na.omit(R)
      
      R = R %>% filter( T_piece < 100, T_station <100)
      
      L[[paste0(FROM,'_',TO)]] = R
    }
  }
  print(paste('L length:', length(L)))
  print(names(L))
  })
  L
})

score_Table_2 = reactive({
  req(TARGET())
  
  withProgress(message = 'Adjusting station-to-station time for parallel stations (30,80 and 140)',{
    L = L_Paired()
    L$SM26.1_SM30$T_piece = L$SM26.1_SM30$T_piece/2
    L$SM76.2_SM80$T_piece = L$SM76.2_SM80$T_piece/2
    L$SM130_SM140$T_piece = L$SM130_SM140$T_piece/2
    SCORES = lapply(L, function(x) score_Station_2(x, Target = TARGET()))
    R = data.frame(Interval = names(L), do.call(rbind,SCORES))
  })
  R
})

output$score_Table_2 = DT::renderDataTable({
  R = score_Table_2()
  rownames(R) = NULL
  
  DT::datatable(
    R,
    options = list(
      paging = FALSE,
      searching = FALSE
    )
  )
})

L_Paired_Adj = reactive({
  L = L_Paired()
  withProgress(message = 'Adjusting station-to-station time for parallel stations (30,80 and 140)',{
    
  L$SM26.1_SM30$T_piece = L$SM26.1_SM30$T_piece/2
  L$SM76.2_SM80$T_piece = L$SM76.2_SM80$T_piece/2
  L$SM130_SM140$T_piece = L$SM130_SM140$T_piece/2
  })
  L
})

L_Paired_Summary = reactive({
  
  req(TARGET())
  
  withProgress(message = 'Calculating time losses',{
    
  
    L = L_Paired_Adj()
    SCORES_upper = sapply(L, function(x) score_Station_2(x, Target = TARGET())[6])
    SCORES_lower = sapply(L, function(x) score_Station_2(x, Target = TARGET())[12])
    SCORES_total = sapply(L, function(x) score_Station_2(x, Target = TARGET())[14])
    Ratio =  round((SCORES_total)/(SCORES_total+TARGET()),6)
    N = nrow(df)
    # No State column in data
    # Failure = sapply(L, function(df) sum(df$State!='OK'))
    
    Unavailable = sapply(L, function(df){with(df,sum(T_station > T_piece)/nrow(df))})
    Available   = sapply(L, function(df){with(df,sum(T_station <= T_piece)/nrow(df))})
    Available   = round(Available, 3)
    
    R = data.frame(Sequence_No = 1:length(L),
                   Names = names(L),
                   Total_Time_Loss = SCORES_total,
                   Ratio = Ratio,
                   Available = Available
    )
    rownames(R) = NULL
  })
#  write.csv(R, 'C:/daten/tmp_file.csv', row.names = FALSE)
  R
})



output$paired_2 = renderPlot({
  
  req(TARGET())
  
  withProgress(message = 'Making time loss analysis chart',{
    
  
  L = L_Paired_Adj()
  
  
  SCORES_upper = sapply(L, function(x) score_Station_2(x, Target = TARGET())[6])
  SCORES_lower = sapply(L, function(x) score_Station_2(x, Target = TARGET())[12])
  SCORES_total = sapply(L, function(x) score_Station_2(x, Target = TARGET())[14])
  
  #  tmp = -(-SCORES_total)/(-SCORES_total+TARGET())  
  tmp = (SCORES_total)/(SCORES_total+TARGET())
  print(tmp)
  #tmp = ifelse(tmp <= 0, 0, tmp)
  SCORES_Percent = percent(tmp)
  
  Top_Cutoff = sort(SCORES_total, decreasing = TRUE)[input$max_bottle_num]
  
  
  
  
  
  Limit_CT = TARGET()
  Limit_Throughtput = TARGET()
  
  
  
  print(length(L))
  print(length(LEVELS))
  
  L2 = lapply(1:(length(L)), function(i){
    df = L[[i]]
    N = nrow(df)
    Percent_Left_Lower = with(df,sum(T_station <= Limit_CT & T_piece <= Limit_Throughtput)/N)
    Percent_Left_Upper = with(df,sum(T_station <= Limit_CT & T_piece > Limit_Throughtput)/N)
    Percent_Right_Lower = with(df,sum(T_station > Limit_CT & T_piece <= Limit_Throughtput)/N)
    Percent_Right_Upper = with(df,sum(T_station > Limit_CT & T_piece > Limit_Throughtput)/N)
    
    Percent_Upper = with(df,sum(T_station > T_piece)/N)
    Percent_Lower = with(df,sum(T_station <= T_piece)/N)
    
    
    R = round(data.frame(Percent_Left_Lower, Percent_Left_Upper, 
                         Percent_Right_Lower, Percent_Right_Upper,
                         Percent_Lower, Percent_Upper
                         
    ),3)
    R
  })
  
  
  
  LP = lapply(1:(length(L)), function(i){
    TMP = L[[i]]
    Percent_Left_Lower = percent(L2[[i]][1,1],1)
    Percent_Left_Upper = percent(L2[[i]][1,2],1)
    Percent_Right_Lower = percent(L2[[i]][1,3],1)
    Percent_Right_Upper = percent(L2[[i]][1,4],1)
    
    Percent_Lower = percent(L2[[i]][1,5])
    Percent_Upper = percent(L2[[i]][1,6])
    
    
    # TT = paste(names(L)[i],'\nTime loss(Parts available):' ,
    #            SCORES_upper[i],
    #            '\nTime loss(Parts not available):',
    #            SCORES_lower[i], 
    #            '\nTotal time loss:', 
    #            SCORES_total[i],
    #            '\nTotal piece loss:',
    #            SCORES_Percent[i]
    # )
    
    # For the presentation
    
    TT = paste(
      
      strsplit(names(L)[i],'_')[[1]][2],
               # '\nTime loss(Parts available) [s/pc]:' ,
               # round(SCORES_upper[i],1),
               # '\nTime loss(Parts not available) [s/pc]:',
               # round(SCORES_lower[i],1), 
               '\nTotal time loss: [s/pc]', 
               round(SCORES_total[i],1),
               '\nTotal piece loss:',
               SCORES_Percent[i]
    )
    
    P = ggplot(TMP, aes(x = T_piece, y = T_station)) + geom_point(alpha = 0.2) + 
      ggtitle(TT) + 
      theme_bw() + 
      geom_density2d() + 
      xlim(0,100) +
      ylim(0,100) +
      geom_vline(xintercept = Limit_CT, color = 'red', linetype = 'dashed')+
      geom_hline(yintercept = Limit_Throughtput, color = 'red', linetype = 'dashed') + 
      geom_abline(slope= 1 , intercept = 0, color = 'black', linetype = 'dashed')+
      # annotate('text', x = -Inf, y = -Inf, label = as.character(Percent_Left_Lower), vjust =-1, hjust =-1) +
      # # the following positioning changed
      # annotate('text', x = Inf, y = -Inf, label = as.character(Percent_Left_Upper), vjust =-1, hjust =1) +
      # annotate('text', x = -Inf, y = Inf, label = as.character(Percent_Right_Lower), vjust =1.5, hjust =-1) +
      # 
      # annotate('text', x = Inf, y = Inf, label = as.character(Percent_Right_Upper), vjust =1.5, hjust =1) +
      
      annotate('text', x = -Inf, y = Inf, label = as.character(Percent_Upper), vjust = 1.5, hjust = - 0.3) +
      annotate('text', x = Inf, y = -Inf, label = as.character(Percent_Lower), vjust = -1, hjust = 1) +
      
      
      
      
      xlab('Part view time [s]') +
      ylab('Station view Time [s]') +
      theme(panel.background = element_blank())
    
    Shades = data.frame(x = c(0,0,100,0,100,100), y = c(0,100,100,0,0,100), g = c('na','na','na','a','a','a'))
    #    Shades = data.frame(x = c(-10,-10,110,-10,110,110), y = c(-10,110,110,-10,-10,110), g = factor(c('na','na','na','a','a','a')))
    
    P = P + 
      geom_polygon(data= Shades, aes(x=x, y=y,  fill = g), alpha = 0.04) +
      theme(legend.position = 'none') + scale_fill_manual(values = c( "green", "blue"))
    
    P
    
    # if(SCORES_total[i] <= Top3_Cutoff & SCORES_total[i] < 0)    P = P + theme(panel.border = element_rect(color = 'red', fill = NA, size = 2))
    
    if(SCORES_total[i] >= Top_Cutoff & SCORES_total[i] > 0)    P = P + theme(panel.border = element_rect(color = 'red', fill = NA, size = 2))
    
    P
  })
  
  })
  
  do.call(grid.arrange, c(LP, ncol = 6))
})

output$Legend = renderPlot({
  Legend = data.frame(Region = c('Parts immediately avaible','Parts not immediately avaible'))
  
  P = ggplot(Legend, aes(x = Region, fill = Region)) + geom_histogram(stat='count', alpha = 0.04)+scale_fill_manual(values = c( "blue", "green"))
  g_legend <- function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  legend <- g_legend(P)
  
  grid::grid.draw(legend)
})



#######################  Bottle Neck detector  #############


FAILURE_SUMMARY = reactive({
  DATA = DATA_2()
  DATA$ST = factor(DATA$ST, levels = LEVELS)
  R = DATA %>% group_by(ST) %>% dplyr::summarise(N = n(), Failure = sum(State!='OK')) %>%
    mutate(Failure_Rate = round(Failure/N,6))
  
  R
})

output$Failure_Summary_Table = DT::renderDataTable({
  DT::datatable(
    FAILURE_SUMMARY(),
    options = list(
      paging = FALSE
    )
  )
})


L_Paired_Summary_Overview = reactive({
  D = L_Paired_Summary()
  Bottle_Neck_Ind = Bottle_Neck_Detector(D)
  
  Group = rep('Non-Bottleneck', nrow(D))
  
  if(is.null(Bottle_Neck_Ind)) return(data.frame(D, Group))
  
  Group[Bottle_Neck_Ind] = 'Bottleneck'
  
  
  D$Station = sapply(as.character(D$Names), function(x) strsplit(x,'_')[[1]][2])
  
  R = data.frame(D, Group)
  R = merge(R, FAILURE_SUMMARY(), by.x = 'Station', by.y = 'ST') %>% arrange(Sequence_No)
  R$Time_And_Failure = R$Ratio + R$Failure_Rate
  R
})

output$L_Paired_Summary_Table = DT::renderDataTable({
  # D = L_Paired_Summary()
  D = L_Paired_Summary_Overview()
  DT::datatable(D,
                rownames = FALSE,
                options = list(
                  paging = FALSE,
                  searching = FALSE
                ))
})

output$L_Paired_Summary_Overview = renderPlot({
  D = L_Paired_Summary_Overview()
  
  D2 = D[D$Ratio>0,]
  
  
  # D2$Names = as.character(D2$Names)
  # D2$Names_2 = sapply(D2$Names, function(x) strsplit(x,'_')[[1]][2])
  
  # D2$Station = factor(D2$Station, levels = D2$Station[order(D2$Ratio)])
  validate(need(length(D2$Station)>0, 'No stations have positive time loss'))
  
  D2$Station = factor(D2$Station, levels = D2$Station[order(D2$Time_And_Failure)])
  
  #write.csv(D2, 'C:/daten/temp_DNOX.csv', row.names = FALSE)
  
  # 
  # P1 = ggplot(D2, aes(x = Station, y = Ratio, fill = Group)) + geom_bar(stat='identity', width = 0.9, alpha = 0.8) + 
  #   coord_flip() +
  #   #ylim(0,XLIM) +
  #   scale_y_continuous(labels = scales::percent, limits = c(0, XLIM)) +
  #   ylab('') +    
  #   # ylab('Percent of Total Time Loss') +
  #   xlab('Station') + 
  #   ggtitle('Time Loss') +
  #   theme(legend.position = 'none')
  # 
  # P2 = ggplot(D2, aes(x = Station, y = Failure_Rate)) + geom_bar(stat='identity', width = 0.9, alpha = 0.8, fill = 'brown') + 
  #   coord_flip() +
  #   #ylim(0,XLIM) +
  #   scale_y_continuous(labels = scales::percent, limits = c(0, XLIM)) +
  #   ggtitle('Quality Loss') +
  #   ylab('') +    
  # # ylab('Percent of Failure Piece') +
  #   xlab('Station') 
  # # + 
  # #   theme(legend.position = 'none')
  # 
  # P3 = ggplot(D2, aes(x = Station, y = Time_And_Failure, fill = Group)) + theme_bw()+geom_bar(stat='identity', width = 0.9, alpha = 0.8) + 
  #   coord_flip() +
  #   #ylim(0,XLIM) +
  #   scale_y_continuous(labels = scales::percent, limits = c(0, XLIM)) +
  #   ylab('') +    
  #   # ylab('Percent of Total Time Loss & Failure Piece') +
  #   xlab('Station') + 
  #   ggtitle('Overall Loss') +
  #   theme(legend.position = 'none') 
  # 
  # grid.arrange(P1,P3,P2, ncol =3)
  
  D3 = rbind(
    data.frame(Station = D2$Station, Group = D2$Group, Value = D2$Ratio),
    data.frame(Station = D2$Station, Group = 'Quality_Loss', Value = D2$Failure_Rate)
  )
  
  XLIM = max(0.2, D3$Value)
  
  # D3$Value[D3$Value == 0] = 0.00001
 # write.csv(D3, 'C:/daten/temp_DNOX.csv', row.names = FALSE)
  
  cols = c('Bottleneck' = 'red', 'Non-Bottleneck' = 'blue', 'Quality_Loss' = 'black')
  
  P1 = ggplot(D3[D3$Group!='Quality_Loss', ], aes(x = Station, y = Value, fill = Group)) + #theme_bw() +
    geom_bar(stat='identity', width = 0.9, alpha = 0.8) + 
    coord_flip() +
    scale_fill_manual(values = cols) +
      scale_y_continuous(labels = scales::percent, limits = c(0, XLIM)) +
      ylab('') +
      xlab('Station') +
      ggtitle('Time Loss') +
      theme(legend.position = 'none')
  
  P2 = ggplot(D3, aes(x = Station, y = Value, fill = Group)) + theme_bw() +
    geom_bar(stat='identity', width = 0.9, alpha = 0.8) + 
    coord_flip() +
    scale_fill_manual(values = cols) +
    scale_y_continuous(labels = scales::percent, limits = c(0, XLIM)) +
      ylab('') +
      xlab('Station') +
      ggtitle('Overall Loss') +
      theme(legend.position = 'none')
  
  # D3$Group = factor(D3$Group, levels = c('Bottleneck','Non-Bottleneck','Quality_Loss'))
  # # cols = c(1 = 'red',2 = 'blue', 3 = 'black')
  
  P3 = ggplot(D3[D3$Group=='Quality_Loss', ], aes(x = Station, y = Value, fill = Group)) + #theme_bw() +
    geom_bar(stat='identity', width = 0.9, alpha = 0.8) + 
    coord_flip() +
    scale_fill_manual(values = cols, drop = FALSE) +
    scale_y_continuous(labels = scales::percent, limits = c(0, XLIM)) +
      ggtitle('Quality Loss') +
      ylab('') +
      xlab('Station')
  
  

  grid.arrange(P1,P2,P3, ncol =3)
})
