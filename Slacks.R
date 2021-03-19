#script for Transform and Add Slacks
slacks <- function(data){
  n <- nrow(data)
  m <- ncol(data)
  
  for (i in 1:n)
  {
    if (data[i,4] < 0)
    {
      for (j in 1:(m-2))
      {
        data[i,j]=-data[i,j]
      }
      if(data[i,m-1]=='supeq')
      {
        data[i,m-1]='infeq'
      }
      else
      {
        data[i,m-1]='supeq'
      }
    }
  }
  
  V <- data.frame(data[1:n,1:(m-2)])
  Z <- data.frame(data[,m])
  I <- rbind(data.frame(diag(n-1)), 0)
  
  for (i in 1:n)
  {
    if (data[i,3]=='supeq')
    {
      I[i,] = -I[i,]
    }
  }
  
  all <- data.frame(V,I,Z)
  return(all)
}