#scrpipt for 2-Phase Simplex function

#Code Slacks for test
library("readxl")
data <- read_excel("C:/Documents/EFREI/S8/Numerical Optimization Methods/Labs/Project/Project R Code/LP_System.xlsx")

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

test <- data.frame(V,I,Z)
test
#Fin Code Slacks

Two_Phase_Simplex <- function(data){
  n=nrow(data)
  
  V <- rbind(data.frame(data[1:(nrow(data)-1),1:(ncol(data)-(nrow(data)))]),0)
  I <- data.frame(data[(ncol(data)-(nrow(data)-1)):(ncol(data)-1)])
  Z <- data.frame(data[ncol(data)])
  
  I2<-cbind(data.frame(I[0]))
  for (i in 1:(ncol(I)-1)) {
    if (I[i,i]==-1) {
      I2<-cbind(I2,data.frame(-I[i]))
    }
  }
  
  pos=nrow(I2)
  for (i in 1:ncol(I2)) {
    I2[pos,i]=-1
  }
  
  I<-cbind(I,I2)
  for(i in 1:ncol(I)){
    colnames(I)[i] <- paste("X",i)
  }
  data <- data.frame(V,I,Z)
  return(data)
}

Two_Phase_Simplex(test)
