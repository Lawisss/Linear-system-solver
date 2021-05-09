source("./Slacks.R")
source("./Simplex.R")

#script for 2-Phase Simplex function

Two_Phase_Simplex <- function(data,max){
  data <- Initialization(data)
  data <- Phase1_Part1(data)
  line_to_add <- data[nrow(data),]
  data <- Phase1_Part2(data)
  data <- Phase2(data,line_to_add)
  data <- Simplex(data)
  data <- End(data,max)
  data[nrow(data),ncol(data)]=-data[nrow(data),ncol(data)]
  return(data)
}

Initialization <- function(data){
  n <- nrow(data)
  m <- ncol(data)
  
  V <- rbind(data.frame(data[1:(n-1),1:(m-n)]),0)
  I <- data.frame(data[(m-(n-1)):(m-1)])
  Z <- data.frame(data[m])
  
  I2<-cbind(data.frame(I[0]))
  for (i in 1:ncol(I)) {
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

Phase1_Part1 <- function(data){
  n=nrow(data)
  m=ncol(data)
  for (i in (m-(n-1)):(m-1))
  {
    if(data[n,i]==-1)
    {
      for(j in (1:n))
      {
        if(data[j,i]==1)
        {
          l=j
        }
      }
      for (a in (1:m))
      {
        data[n,a]=data[n,a]+data[l,a]
      }
    }
  }
  return(data)
}

Phase1_Part2 <- function(data){
  n <- nrow(data)
  m <- ncol(data)
  
  count=0
  for(i in (1:(m-2)))
  {
    if(data[n,i]==-1)
    {
      count=count+1
    }
  }
  data[n,]=-data[n,]
  data <- Modified_Simplex(data,count)
  return(data)
}

Modified_Simplex <- function(data,stop){
  pivotcolomn = which.min(data[nrow(data),1:(ncol(data)-1)])
  count = 1
  while(count < stop + 1)
  {
    coltmp<-array()
    i=1
    while(i < nrow(data)+1){
      if((data[i,ncol(data)]/data[i,pivotcolomn])>0)
      {
        coltmp[i] = data[i,ncol(data)]/data[i,pivotcolomn]
      }
      i=i+1
    }
    pivotline = which.min(coltmp)
    
    data[pivotline,] <- data[pivotline,] / data[pivotline,pivotcolomn]
    n <- 1
    while (n < ncol(data)-1-stop)
    {
      if(n==pivotline)
      {
        n <- n+1
      }
      else
      {
        data[n, ] <- data[n, ] - data[pivotline, ] * data[n,pivotcolomn]
        n <- n+1
      }
    }
    pivotcolomn = which.min(data[nrow(data),1:(ncol(data)-1)])
    count <- count + 1
  }
  return(data)
}

Phase2 <- function(data,line_to_add){
  n <- nrow(data)
  m <- ncol(data)
  
  count=0
  for(i in (1:m))
  {
    if(data[n,i]==1)
    {
      count=count+1
    }
  }
  
  temp_data <- data[1:(n-1),1:m]
  temp_data <- rbind(temp_data,line_to_add)
  Z <- temp_data[1:n,m]

  data <- data[1:(n-1),1:(m-count-1)]
  data <- rbind(data,line_to_add[,1:(ncol(line_to_add)-count-1)])
  data <- cbind(data,Z)
  
  data[n,]=-data[n,]
  return(data)
}

End <- function(data,max){
  print(data)
  l=nrow(data)
  for (i in (1:ncol(max)))
  {
    c=i
    if((data[l,c]!=(-max[i])))
    {
      for(j in (1:(l-1)))
      {
        if(data[j,c]==1)
        {
          pivotline = j
        }
      }
      x=(max[c]-data[l,c])/data[pivotline,c]
      for(j in (1:ncol(data)))
      {
        data[l,j]=data[l,j]+x*data[pivotline,j] 
      }
    }
  }
  return(data)
}

#Function to use Simplex or 2-Phase Simplex
Simplexs <- function(data){
  max = data[nrow(data),1:(ncol(data)-2)]
  #Transform and Add Slacks
  data_transformed = slacks(data)
  
  test <- FALSE
  i=1
  while(i<nrow(data_transformed) & test == FALSE){
    if(data_transformed[nrow(data_transformed)-i,ncol(data_transformed)-i]==-1)
    {
      test = TRUE
    }
    i=i+1
  }
  
  if(test == TRUE){
    #2-Phase Simplex
    return(Two_Phase_Simplex(data_transformed,max))
  } else
  {
    #Simplex
    return(Simplex(data_transformed))
  }
}


