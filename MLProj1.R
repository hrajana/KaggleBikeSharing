read.csv('bikeshare.csv')
df <- read.csv('bikeshare.csv')
library(ggplot2)
library(dplyr)
library(ggthemes)
#Goal: predict the number of bikes rented during each hour covered by the test set
#Scatterplot of count vs. temp, with the gradient based on temperature
p <- ggplot(df, aes(x = temp, y = count)) + geom_point(aes(color = temp), alpha = 0.3,  size = 1)
print(p)
#Plot count versus datetime as a scatterplot with a color gradient based on temperature. You'll need to convert the datetime column into POSIXct before plotting
df$datetime <- as.POSIXct(df$datetime)
p1 <- ggplot(df, aes(x = datetime, y = count)) + geom_point(aes(color = temp), alpha = 0.5, size = 1) + scale_color_gradient(high = 'red', low = 'green')
print (p1)
#CORRELATIONS between temp and count 
corrtempcount <- cor(df$temp, df$count)
print(corrtempcount)
seasonalrentals <- ggplot(df, aes(factor(season), count)) + geom_boxplot(aes(color = factor(season)))
print(seasonalrentals)
#Make an hour column that takes the hour from datetime
hour <- format(df$datetime, "%H")
df$hour = hour
#Scatterplot of count vs. hour
countbyhour <- ggplot(subset(df, workingday == 1), aes(x = hour, y = count)) + geom_point(position=position_jitter(w=1, h=0), aes(color = temp), alpha = 0.5, size = 1) + scale_color_gradientn(colors=c('purple', 'blue','green', 'yellow', 'red'))
print (countbyhour)
#Same for nonworking days
weekendcountbyhour <- ggplot(subset(df, workingday == 0), aes(x = hour, y = count)) + geom_point(position=position_jitter(w=1, h=0), aes(color = temp), alpha = 0.5, size = 1) + scale_color_gradientn(colors=c('purple', 'blue','green', 'yellow', 'red'))
print (weekendcountbyhour)
#Building the model 
temp.model <- lm(count ~ temp, data = df)
summary(temp.model)
#Predict 
temp.test <- data.frame(temp= c(25))
predict <- predict(temp.model, temp.test)
print(predict)
#Use sapply() and as.numeric to change the hour column to a column of numeric values.
df$hour <- sapply(df$hour, as.numeric)
#Can either add up all of the columsn you want, or have all, but subtract what you don't want
model <- lm(formula = count ~ . - casual - registered, data = df, subset = -atemp)
summary(model)
predict <- predict(model, temp.test)
print(predict)

