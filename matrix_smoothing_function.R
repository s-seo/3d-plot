

MatSmooth <- function(data, n){
  # smoothing matrix with moving average n
  
  data[is.na(data)] <- 0
  
  for(i in 2:(nrow(data)-1)){
    for(j in 2:(ncol(data)-1)){
      if(i > n & i < nrow(data)-n & 
         j > n & j < ncol(data)-n){
        data[i, j] <- data[seq(i-n, i+n, 1), seq(j-n, j+n, 1)] %>% mean()
      }else{
        mm <- min(i, j, nrow(data)-i, ncol(data)-j)
        data[i ,j] <- data[seq(i-mm, i+mm, 1), seq(j-mm, j+mm, 1)] %>% mean()
      } 
    }
  }
  data[data==0] <- NA
  
  data[1,] <- data[2,]
  data[,1] <- data[,2]
  data[nrow(data),] <- data[nrow(data)-1,]
  data[,ncol(data)] <- data[,ncol(data)-1]
  
  return(data)
}


