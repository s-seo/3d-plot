
library(dplyr)
library(reshape)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

res <- read.csv('C:\\Users\\N25\\Desktop\\sm\\res.csv')
res <- dat %>% rename(c(age='AGE', V3='pred'))

#통계량
res2 <- res %>%
  sample_n(., size = 100000) %>%
  group_by(BMI, AGE) %>%
  mutate(pred.mean = mean(pred),
         pred.median = median(pred),
         pred.mode = getmode(pred)) %>%
  as.data.frame()

#빈틈 없는 데이터셋 만들어 통계량 data와 통합
bmi <- seq(min(res$BMI), max(res$BMI), 0.1)
age <- seq(min(res$AGE), max(res$AGE), 1)
res3 <- cbind(rep(bmi, each=length(age)), rep(age, length(bmi))) %>% 
  as.data.frame() %>%
  rename(c(V1 = 'BMI', V2 = 'AGE')) %>%
  merge(., res2, by=c('BMI', 'AGE'), all = TRUE)
res3[is.na(res3)] <- 0

#z matrix
res4 <- lapply(1:3, function(x){
  l1 <- res3[,c(1,2,x+3)] %>%
    distinct() %>%
    arrange(BMI, AGE) %>%
    mutate(BMI = as.factor(BMI),
           age = as.factor(AGE)) %>%
    select(3) %>%
    unlist() %>%
    as.numeric() %>%
    matrix(., length(bmi), length(age), byrow=T) %>%
    list(bmi, age, .) 
  names(l1) <- c("BMI", 'age', colnames(res3)[x+3])
  return(l1)
})

#3d plot
library(plotly)
p <- lapply(1:3, function(x){
  plot_ly(x= res4[[x]][[1]],
          y= res4[[x]][[2]],
          z= res4[[x]][[3]],
          type='surface',
          showscale = FALSE) %>%
    layout(scene = list(xaxis = list(title='BMI'),
                        yaxis = list(title='AGE'),
                        zaxis = list(title='hazard')),
           title=paste0('PHREG Procedure: 3d plot of BMI, age and ',
                        c('Mean', 'Median', 'Mode')[x],
                        ' hazard'))
})

p[[2]]



coef.total <- c(0.02403, 0.00711)
BMI <- sample(seq(15,33,0.05), 1000, rep=T)
age <- sample(seq(20,80,0.1), 1000, rep=T)
hf <- cbind(BMI, age) %*% coef.total[1:2]
dat <- cbind(BMI, age, hf) %>% as.data.frame()
