

SurfSmoothPlot <- function(data, m = 2){

  # why I build this function? thought it will be convenient if there is a
  #                             function that can make 3d surface plot from a
  #                             simple dataframe with 3 columns
  #
  # expected form of the input data : dataframe with 3 features and 
  #                                   z-axis with 3rd column
  # extected form of the result : 3d plot..?
  # expected usage of the function : just for fun!
  
  pck <- c('dplyr', 'reshape')
  a <- lapply(pck, require, character.only = T)
  if(!any(unlist(a))) stop(paste0('install package ', pck[!unlist(a)]))

  # name the coloumns
  colnames(data) <- c('x', 'y', 'z')
  
  # squeezing
  dat1 <- data %>%
    group_by(x, y) %>%
    summarise(z = mean(z)) %>%   #be cautious that we use mean value
    as.data.frame() %>%
    arrange(x, y)

  # strecthing
  dist <- sapply(1:2, function(x){
    data[,x] %>%
      sort() %>%
      diff() %>%
      as.data.frame() %>%
      filter(. != 0) %>%
      min()
  })
  xax <- seq(min(data$x), max(data$x), by = dist[1])
  yax <- seq(min(data$y), max(data$y), by = dist[2])

  dat2 <- cbind(rep(xax, each = length(yax)),
                rep(yax, by = length(xax))) %>%
    as.data.frame() %>%
    rename(c(V1='x', V2='y')) %>%
    merge(., dat1, by=c('x','y'), all=TRUE) %>%
    mutate(x = as.factor(x),
           y = as.factor(y))
  
  # list containing matrix form of the 3rd column 
  dat3 <- dat2 %>%
    select(3) %>%
    unlist() %>%
    as.numeric() %>%
    matrix(., length(levels(dat2$x)), length(levels(dat2$y)), byrow=T) %>%
    list(x = seq(min(data$x), max(data$x), length.out = 100),
         y = seq(min(data$y), max(data$y), length.out = 100),
         z =.)
  
  # smoothing - simple moving average
  dat4 <- dat3$z
  dat4[is.na(dat4)] <- 0
  
  for(i in 2:(nrow(dat4)-1)){
    for(j in 2:(ncol(dat4)-1)){
      if(i > m & i < nrow(dat4)-m & 
         j > m & j < ncol(dat4)-m){
        dat4[i, j] <- dat4[seq(i-m, i+m, 1), seq(j-m, j+m, 1)] %>% mean()
      }else{
        mm <- min(i, j, nrow(dat4)-i, ncol(dat4)-j)
        dat4[i ,j] <- dat4[seq(i-mm, i+mm, 1), seq(j-mm, j+mm, 1)] %>% mean()
      } 
    }
  }
  dat4[dat4==0] <- NA
  
  dat4[1,] <- (dat4[1,] + dat4[2,])/2
  dat4[,1] <- (dat4[,1] + dat4[,2])/2
  dat4[nrow(dat4),] <- (dat4[nrow(dat4),] + dat4[nrow(dat4)-1,])/2
  dat4[,ncol(dat4)] <- (dat4[,ncol(dat4)] + dat4[,ncol(dat4)-1])/2
  
  
  # draw 3d plot with plot_ly
  if(!require(plotly, quietly = T)) stop('install package plotly')
  library(plotly)
  p1 <- plot_ly(x = dat3$x,
          y = dat3$y,
          z = dat4,
          type = 'surface',
          colors = 'Set1') %>%
    layout(scene = list(xaxis = list(title='x'),
                        yaxis = list(title='y'),
                        zaxis = list(title='z', range=c(-1,1))),
           title=paste0('surface plot with SMA(', m, ')'))
  unloadNamespace('plotly')
  
  print(p1)
}

