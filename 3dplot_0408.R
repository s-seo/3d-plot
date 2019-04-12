#함

library(dplyr)
library(reshape)

n <- 10000
res <- cbind(BMI = sample(seq(15,33,0.5), n, rep=T), 
             age = sample(seq(20,80,1), n, rep=T),
             pred = runif(min = -1, max =1, n) %>% round(3)) %>% 
  as.data.frame()

res2 <- res %>% 
  group_by(BMI, age) %>%
  summarise(pred = mean(pred)) %>%
  as.data.frame() %>%
  arrange(BMI, age) %>%
  mutate(BMI = as.factor(BMI),
         age = as.factor(age))

res3 <- cbind(rep(seq(15,33,0.5), each = length(seq(20,80,1))),
              rep(seq(20,80,1), by = length(seq(15,33,0.5)))) %>%
  as.data.frame() %>%
  rename(c(V1='BMI', V2='age')) %>%
  merge(., res2, by=c('BMI','age'), all=TRUE) %>%
  mutate(BMI = as.factor(BMI),
         age = as.factor(age))

res4 <- res3 %>%
  select(3) %>%
  unlist() %>%
  as.numeric() %>%
  matrix(., length(levels(res3$BMI)), length(levels(res3$age)), byrow=T) %>%
  list(BMI = seq(15,33,0.5),
       age = seq(20,80,1),
       pred =.)


library(plotly)
plot_ly(x = res4$BMI,
        y = res4$age,
        z = res4$pred,
        type = 'surface') %>%
  layout(scene = list(xaxis = list(title='BMI'),
                      yaxis = list(title='AGE'),
                      zaxis = list(title='hazard', range=c(-1,1))),
         title=paste0('PHREG Procedure: 3d plot of BMI, age and hazard'))
unloadNamespace('plotly')




if(i == 1 | j == 1 | i == nrow(data) | j == nrow(data)){
  if(which.min(c(i,j,nrow(data)-i,ncol(data)-j)) %in% c(1,3)){
    if(which.min(c(i, nrow(data)-i)) == 1){
      data[i,j] <- data[seq(i, i+1,1), seq(j-1, j+1, 1)] %>% mean()
    }else{
      data[i,j] <- data[seq(i-1, i, 1), seq(j-1, j+1, 1)] %>% mean()
    }
  }else if(which.min(c(j, ncol(data)-j)) == 1){
    data[i,j] <- data[seq(i-1, i+1, 1), seq(j, j+1, 1)] %>% mean()
  }else{
    data[i,j] <- data[seq(i-1, i+1, 1), seq(j-1, j, 1)] %>% mean()
  }
}

#moving average
whyamidoingthisfor <- function(data, m){
  data[is.na(data)] <- 0
  
  for(i in 2:(nrow(data)-1)){
    for(j in 2:(ncol(data)-1)){
      if(i > m & i < nrow(data)-m & 
         j > m & j < ncol(data)-m){
        data[i, j] <- data[seq(i-m, i+m, 1), seq(j-m, j+m, 1)] %>% mean()
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


mat <- res4$pred
mat <- whyamidoingthisfor(mat, 2)

library(plotly)
plot_ly(x = res4$BMI,
        y = res4$age,
        z = mat,
        type = 'surface') %>%
  layout(scene = list(xaxis = list(title='BMI'),
                      yaxis = list(title='AGE'),
                      zaxis = list(title='hazard', range=c(-1,1))),
         title=paste0('PHREG Procedure: 3d plot of BMI, age and hazard with SMA(2)'))
unloadNamespace('plotly')






whyamidoingthislapply <- function(m){
  mat2 <- lapply(2:(nrow(mat)-1), function(x){
    lapply(2:(ncol(mat)-1), function(y){
      if(i > m & i < nrow(mat)-m & 
         j > m & j < ncol(mat)-m){
        mat[i, j] <- mat[seq(i-m, i+m, 1), seq(j-m, j+m, 1)] %>% mean()
      }else{
        mm <- min(i, j, nrow(mat)-i, ncol(mat)-j)
        mat[i ,j] <- mat[seq(i-mm, i+mm, 1), seq(j-mm, j+mm, 1)] %>% mean()
      }
    })
  }) %>% unlist() %>% matrix(., nrow(mat), ncol(mat))
}






















