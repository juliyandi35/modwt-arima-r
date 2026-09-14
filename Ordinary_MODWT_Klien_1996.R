#Ordinary MODWT
library(wavethresh)
library(readxl)
#Load the data
data <- read_excel("data 120.xlsx")
#Apply MODWT
#Determine the Daubechies filter dan scale levels  
filters<-"d4"
scale<-5
#Calculate the wavelet and the scale coefficients and signals
wt <- modwt(data$`Data Inflasi`, filters, n.levels = scale)
wt
