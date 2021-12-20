#Getting the packages downloaded

download.packages('tidyverse')
download.packages('dplyr')
download.packages('janitor')
download.packages('skimr')

#Loading the packages

library(tidyverse)
library(dplyr)
library(janitor)
library(skimr)

#Loading dataset
library(readxl)
automobile_data_excel_file <- read_excel("E:/data analysis/automobile_data excel file.xlsx")
View(automobile_data_excel_file)

#Assigning vector for dataset
file <- automobile_data_excel_file

#Looking at dataset
View(arrange(file,-price))
View(arrange(file,price))

##ANALYSING DATASET

#Avg price of cars

data <- file%>%group_by(make)%>%summarise(avg_price=mean(price))
arrange(data,-avg_price)

ndata <- select(data,make,avg_price)

#chart
ggplot(data=ndata, aes(x=avg_price, y=make))+geom_point()

#Cars with highest price
file1 <- filter(file, price > 35000)
View(arrange(file1,-price))

file2 <-arrange(select(file1,make,body_style,horsepower,price),-price)

file2
#chart
ggplot(data=file2, aes(x=make, y=price))+geom_col()


#Price according to body-type
data1 <- file%>%group_by(body_style)%>%summarise(avg_price = mean(price))
arrange(data1,-avg_price)
ndata1 <- select(data1,body_style,avg_price)

#chart
ggplot(data = ndata1, aes(x=body_style, y=avg_price))+geom_col()

#Price according to horsepower
data2 <- file%>%group_by(horsepower)%>%summarise(avg_price = mean(price))
View(arrange(data2,-horsepower))
ndata2 <- select(data2, horsepower, avg_price)

#chart
ggplot(data=ndata2, aes(x=horsepower, y=avg_price)) + geom_col()

#horsepower as per body-type
data3 <- file%>%group_by(body_style)%>%summarise(avg_horsepower=mean(horsepower))
View(arrange(data3,-avg_horsepower))
ndata3 <- select(data3, body_style, avg_horsepower)


#chart
ggplot(data=ndata3, aes(x=body_style, y=avg_horsepower))+geom_col()
       
