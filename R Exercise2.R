# Installing packages
install.packages("tidyverse")
library(tidyverse)

library(dplyr)
library(ggplot2)

# Extracting the data set
D0 <- read.csv("Tips.csv")
View(D0)

#Using str() and summary for data inspection
str(D0)
summary(D0)

# Showing the number of rows and columns 
num_of_row <- nrow(D0)
num_of_col <- ncol(D0)

print(num_of_row)
print(num_of_col)

#Checking for missing values 
missing_values <- is.na(D0$total_bill)
print(missing_values)

#Getting the cleaned data
D1 <- na.omit(D0)
print(D1)
View(D1)

str(D1)
summary(D1)

#Subset/Filtering dinner from time column
D2 <- D1 %>% 
  filter(time == 'Dinner')
View(D2)

#Removing the data set D2 using rm()
rm(D2)

#Data Visualization 
#Plotting a histogram for the total_bill from D1
ggplot(D1) + geom_histogram(mapping = aes(x=total_bill, 'fill'= time), bins = 30)+ 
  labs(title = "Histogram showing total bill at different times", caption = 'Data visualized by Akua')

#Histogram for females
D2 <- D1 %>% 
  filter(sex == "Female")
View (D2)

ggplot(D2) + geom_histogram(mapping = aes(x = total_bill,'fill' = sex), bins = 50)+
  labs(title = "Histogram showing females",caption = 'Data viz by Akua' )                            

# Creating a box plot for tip in D1
ggplot(D1) + geom_boxplot(mapping = aes(x = tip , 'fill'= day)) + 
  labs(title = 'Box plot for tip data', caption = "Data viz by Akua")

# Correlation 
## Finding the mean and the variance for total_bill 
D1 <- D1 %>% 
  mutate(Mean_total_bill = mean(D1$total_bill))
View(D1)

D1 <- D1 %>% 
  mutate(Variance_of_total_bill = var(D1$total_bill))
View(D1)

#Removing the first correlation columns I created
D1<- select(D1, -Mean)
D1 <- select(D1, -Variance)


#Calculating the correlation between tip and total_bill
Correlation_value <- cor(D1$total_bill,D1$tip)
View (Correlation_value)

# Drawing a line plot between total_bill and tip
plot(D1$total_bill,D1$tip,main = 'Line Plot', xlab = 'total_bill', ylab = 'tip')



