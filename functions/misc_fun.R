# text adjustment
my.mtext <- function(my.adj, ...) { 
  xr <- par("usr")[1:2] 
  my.at <- xr[1]+my.adj*(xr[2] - xr[1]) 
  mtext(at=my.at, ...) 
} 

# round
myround <- function(x, k=2) trimws(format(round(x, k), nsmall=k))



