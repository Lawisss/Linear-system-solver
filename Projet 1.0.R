#import functions from scripts
source("C:/Documents/EFREI/S8/Numerical Optimization Methods/Labs/Project/Project R Code/Slacks.R")
source("C:/Documents/EFREI/S8/Numerical Optimization Methods/Labs/Project/Project R Code/Simplex.R")
source("C:/Documents/EFREI/S8/Numerical Optimization Methods/Labs/Project/Project R Code/2-Phase Simplex.R")
source("C:/Documents/EFREI/S8/Numerical Optimization Methods/Labs/Project/Project R Code/Split.R")
source("C:/Documents/EFREI/S8/Numerical Optimization Methods/Labs/Project/Project R Code/Cut.R")
source("C:/Documents/EFREI/S8/Numerical Optimization Methods/Labs/Project/Project R Code/IntToBin.R")
source("C:/Documents/EFREI/S8/Numerical Optimization Methods/Labs/Project/Project R Code/Binary.R")

#data import
library("readxl")

data1 <- read_excel("C:/Documents/EFREI/S8/Numerical Optimization Methods/Labs/Project/Project R Code/LP_System.xlsx")
data2 <- read_excel("C:/Documents/EFREI/S8/Numerical Optimization Methods/Labs/Project/Project R Code/LP_System2.xlsx")

main <- function(data){
  print("SOLVER")
  print("1. Full")
  print("2. Integral")
  print("3. Binary")
  
  choice <- readline(prompt="Your choice is : ")
  
  if (choice == 1){
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
    
    if(test == TRUE)
    {
      #2-Phase Simplex
      return(Two_Phase_Simplex(data_transformed))
    }
    else
    {
      #Simplex
      return(Simplex(data_transformed))
    }
  }
}

main(data1)
main(data2)

