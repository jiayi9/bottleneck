Bottle_Neck_Detector = function(D, AVA_CUTOFF = 0.7){
  Positive_Loss_Ind = which(D$Ratio >0)
  
  print(Positive_Loss_Ind)
  
  L = list()
  
  if(length(Positive_Loss_Ind) == 0) return(NULL)
  
  if(all(diff(Positive_Loss_Ind) != 1)) return(Positive_Loss_Ind)
  
  L[[1]] = Positive_Loss_Ind[1]
  
  for(i in 2:length(Positive_Loss_Ind)){
    cat('=========\n')
    print(paste('outer round', i))
    if(Positive_Loss_Ind[i] - Positive_Loss_Ind[i-1] == 1){
      cat('----1 continuous----\n')
      print(paste('Length:',length(L)))
      print(L)
      
      print(paste('Old elements', L[[length(L)]]))
      print(paste("To concatenate", Positive_Loss_Ind[i]))
      L[[length(L)]] = c(L[[length(L)]], Positive_Loss_Ind[i])
      print(paste("Continuous result", L))
      
    } else {
      cat('-----2 non-continuous-----\n')
      print(L)
      print(Positive_Loss_Ind[i])
      L = c(L, Positive_Loss_Ind[i] )
    }
  }
  
  print(L)
  
  non_continuous_ind = unlist(lapply(L, function(x) if(length(x) ==1) x else NULL))
  
  continuous_ind = lapply(L, function(x) if(length(x) >1) x else NULL)
  continuous_ind[sapply(continuous_ind, is.null)] <- NULL
  
  tmp = list()
  for(i in 1:length(continuous_ind)){
    sub_Index = continuous_ind[[i]]
    #Bottle_Neck_Indicator = rep('Not defined', length(sub_Index))
    AVA = D$Available[sub_Index]
    
    Not_AVA_Ind = which(AVA < AVA_CUTOFF)
    if(length(Not_AVA_Ind) == 0){
      tmp[[i]] = sub_Index[length(sub_Index)]
    } else {
      
      # tmp[[i]] = c(sub_Index[Not_AVA_Ind][1]-1,sub_Index[Not_AVA_Ind][1]) 
      tmp[[i]] = c(sub_Index[Not_AVA_Ind]-1,sub_Index[Not_AVA_Ind]) 
    }
  }
  
  tmp = unlist(tmp)
  
  R = c(unlist(non_continuous_ind), tmp)
  
  R
}


score_Station_2 = function(X, INDEX = FALSE, Target = 35){
  
  # Above the diagonal line
  

  
  INDEX_upper_good = X$T_station <= Target & X$T_station < X$T_piece
  
  INDEX_upper_bad = X$T_station > Target & X$T_station < X$T_piece
  
  SUM_upper_good = sum(X$T_station[INDEX_upper_good]- Target)  
  
  SUM_upper_bad = sum(X$T_station[INDEX_upper_bad]- Target)  
  
  Size_upper_good = sum(INDEX_upper_good)
  
  Size_upper_bad = sum(INDEX_upper_bad)
  
  # Below the diagonal line
  
  INDEX_lower_good = X$T_piece <= Target & X$T_station > X$T_piece
  
  INDEX_lower_bad = X$T_piece > Target & X$T_station > X$T_piece
  
  SUM_lower_good = sum(X$T_piece[INDEX_lower_good]- Target)  
  
  SUM_lower_bad = sum(X$T_piece[INDEX_lower_bad]- Target)  
  
  Size_lower_good = sum(INDEX_lower_good)
  
  Size_lower_bad = sum(INDEX_lower_bad)
  
  # Overall
  N_upper = Size_upper_good + Size_upper_bad
  N_lower = Size_lower_good + Size_lower_bad
  N = N_upper + N_lower
  
  Score_upper = round((SUM_upper_good + SUM_upper_bad)/N, 4)
  Score_lower = round((SUM_lower_good + SUM_lower_bad)/N, 4)
  Score_Overall = round((SUM_upper_good + SUM_upper_bad + SUM_lower_good + SUM_lower_bad)/N,4)
  
  
  R = c(
    SUM_upper_good,
    Size_upper_good,
    SUM_upper_bad,
    Size_upper_bad,
    N_upper,
    Score_upper,
    
    SUM_lower_good,
    Size_lower_good,
    SUM_lower_bad,
    Size_lower_bad,
    N_lower,
    Score_lower,
    
    N,
    Score_Overall
  )
  
  names(R) = c(
    'SUM_upper_good',
    'Size_upper_good',
    'SUM_upper_bad',
    'Size_upper_bad',
    'N_upper',
    'Score_upper',
    
    'SUM_lower_good',
    'Size_lower_good',
    'SUM_lower_bad',
    'Size_lower_bad',
    'N_lower',
    'Score_lower',
    
    'N',
    'Score_Overall'
  )
  
  if(INDEX){
    R = data.frame(INDEX_upper_good, INDEX_upper_bad, INDEX_lower_good, INDEX_lower_bad)
    
  }
  R
}


Time_Diff = function(x, y){as.numeric(as.difftime(y - x))}

Time_Vector_Diff = function(x) {
  N = length(x)
  DIFF = as.numeric(as.duration(x[2:N] - x[1:(N-1)]))
  DIFF
}

score_Station = function(X, INDEX = FALSE){
  
  INDEX_area_1 = X$T_station > 35 & X$T_station < X$T_piece
  
  
  PART1 = sum(X$T_station[INDEX_area_1]-35)
  
  INDEX_area_2 = X$T_piece > 35 & X$T_station > X$T_piece
  
  
  PART2 = sum(X$T_piece[INDEX_area_2]-35)
  
  R = c(
    PART1 = PART1,
    PART1_N = sum(INDEX_area_1),
    PART2 = PART2,
    PART2_N = sum(INDEX_area_2),
    N = nrow(X),
    score = round((PART1+PART2)/nrow(X),2)
  )
  
  if(INDEX){
    R = data.frame(INDEX_area_1, INDEX_area_2, INDEX_area_3 = !INDEX_area_1&!INDEX_area_2)
    
    R$INDEX = apply(R, 1, function(x){
      if(x[1]){
        1
      } else if(x[2]){
        2
      } else{
        0
      }
    })
  }
  R
}

tieIndex = function(x, mode ="keepfirst"){
  
  N = length(x)
  
  if(N<=1) return(NULL)
  
  tmpTies = c()
  
  L = list()
  
  tieNo = 1
  
  for(i in 2:N){
    if(x[i] == x[i-1]) {
      tmpTies = union(tmpTies,c(i-1,i))
      L[[tieNo]] = tmpTies
    } else{
      tieNo = tieNo + 1
      tmpTies = c()
    }
  }
  
  if(mode == "keepfirst"){
    # remove the first element to keep it
    L = lapply(L, function(x) x[-1])
  } else if( mode == "keeplast"){
    L = lapply(L, function(x) x[-length(x)])
  } else {
    stop("it's either keepfirst or keeplast. check your code.")
  }
  
  R = do.call(c,L)
  
  return(R)
  
}



percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
