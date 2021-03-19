#script for Simplex function

Simplex <- function(data){
  m = which.max(data[nrow(data),])
  end = FALSE
  while(end==FALSE)
  {
    coltmp<-array()
    i=1
    while(i < nrow(data)+1){
      if((data[i,ncol(data)]/data[i,m])>0)
      {
        coltmp[i] = data[i,ncol(data)]/data[i,m]
      }
      i=i+1
    }
    pivotligne = which.min(coltmp)
    
    data[pivotligne,] <- data[pivotligne,] / data[pivotligne,m]
    n <- 1
    while (n < ncol(data)-1)
    {
      if(n==pivotligne)
      {
        n <- n+1
      }
      else
      {
        data[n, ] <- data[n, ] - data[pivotligne, ] * data[n,m]
        n <- n+1
      }
    }
    m = which.max(data[nrow(data),])
    end = TRUE
    for (i in 1:ncol(data))
    {
      if(data[nrow(data),i]>0)
      {
        end = FALSE
      }
    }
  }
  return(data)
}
