# Open your RStudio and start setting up your R environment. 
# Open a new R script and import the turtle_sales.csv data file, which you can download from Assignment: Predicting future outcomes. 
# Import all the required libraries for the analysis and view the data. 
# Load and explore the data.
# Remove redundant columns (Ranking, Year, Genre, Publisher) by creating a subset of the data frame.
# Create a summary of the new data frame.
# Create plots to review and determine insights into the sales data set.
# Create scatterplots, histograms and boxplots to gain insights into the sales data.
# Note your observations and diagrams that could be used to provide insights into the business.

# Import the tidyverse package.
library('tidyverse')

# Change your current directory.
setwd(dir = '/Users/beatriceferreira/Downloads/LSE_DA301_assignment_files (1)')


# Import turtle sales 
sales <- read.csv("turtle_sales.csv")


# View the data frame.
head(sales)

# Convert data frame to a tibble.
as_tibble(sales)

# Use the glimpse() function.
glimpse(sales)

# View summary 
summary(sales)

# Select the columns you want to keep
df2 <- select(sales, Product, Global_Sales, EU_Sales, Platform, NA_Sales)

# Create a DataFrame of the subset 
head(df2)

# View summary 
summary(df2)

# Visualise the data 
# Check the data dimensions 
dim(df2)

# install ggplot2 
library(ggplot2)
ggplot(df2, aes(x = Product)) + geom_histogram()

# view the same plot 
hist(df2$Product)

# view the product for EU Sales 
hist(df2$EU_Sales)

# view the product for NA_Sales 
hist(df2$EU_Sales)

# view the product for Global Sales 
hist(df$Global_Sales)

# Create a boxplot
library(ggplot2)
qplot(Product, Global_Sales, data=df2, geom='boxplot')

# Create another boxplot for EU Sales 
qplot(Product, EU_Sales, data=df2, geom='boxplot')

# Create a boxplot for platform against NA_Sales 
qplot(Platform, NA_Sales, data=df2, geom='boxplot')

# Create a stacked bar chart 
qplot(NA_Sales, fill=Platform, data=df2, geom='bar')

# Create a stacked bar chart 
qplot(Platform, fill=NA_Sales, data=df2, geom='bar')

# Create another stacked bar chart showing 
qplot(Platform, fill=Product, data=df2, geom='bar')

# Create another stacked bar chart showing 
qplot(Product, fill=Platform, data=df2, geom='bar')

###############################################################################
# Continue with your R script in RStudio from Assignment activity 4: Visualise data to gather insights.
# Determine the impact on sales per product_id
# Use the group_by, apply(), and/or aggregate functions to sum the values grouped by product. (Hint: You can choose which of the three functions to use.)
# Create a summary of the new data frame.
# Create plots to review and determine insights into the data set.
# Create scatterplots, histograms and boxplots to gain insights into the sales data.
# Note your observations and diagrams that could be used to provide insights into the business.
# Determine the normality of the data set (sales data).
# Create and explore Q-Q plots for all sales data.
# Perform a Shapiro-Wilk test on all the sales data.
# Determine the Skewness and Kurtosis of all the sales data.
# Determine if there is any correlation between the sales data columns.
# Create plots to gain insights into the sales data. 
# Choose the type of plot you think best suits the data set and what you want to investigate. Explain your answer in your report.
# Compare all the sales data (columns) for any correlation(s).
# Add a trend line to the plots for ease of interpretation.
# Summarise (150–200 words) any insights you've discovered as well as anything you would like to explore further.

# Import the tidyverse package.
library('tidyverse')

# Change your current directory.
setwd(dir = '/Users/beatriceferreira/Downloads/LSE_DA301_assignment_files (1)')

# View the data frame.
head(sales)

# Select the columns you want to keep
df3 <- select(sales, Product, Global_Sales, EU_Sales, NA_Sales)

#Determine the min, max and mean values of all the sales columns 
mean(df3$EU_Sales)

mean(df3$NA_Sales)

mean(df3$Global_Sales)

min(df3$EU_Sales)

min(df3$NA_Sales)

min(df3$Global_Sales)

max(df3$EU_Sales)

max(df3$NA_Sales)

max(df3$Global_Sales)

# Create a summary of the dataframe
summary(df3)


# Install and load the dplyr package
install.packages("dplyr")
library(dplyr)

# List all objects in your environment
ls()

# Remove wages
rm(wages)

# Create data frame of EU_Sales, NA_Sales, Global_Sales and Product 
df4 = select(df3, EU_Sales, NA_Sales, Global_Sales, Product)

# View head 
head(df4)

# groupby Product 
df5 <- group_by(df4, Product)

# View the dataframe
head(df5)

library(dplyr)
df6 <- df5 %>%
  group_by(Product) %>%
  summarize(EU_Sales,
            NA_Sales,
            Global_Sales)

# View df6 
head(df6)

# Create scatterplots, boxplots and barcharts to determine the sales data 
# Create barchart with 5 bins for product to see big trends 
qplot(Product, bins=5, data=df6)

# Comparing EU_Sales with Product and add a theme (classic).
ggplot(df6, aes(x=Product, y=EU_Sales, col=Product)) + 
  geom_point() + geom_smooth() +
  theme_classic()

# Comparing NA_Sales with Product_ID and add a theme (classic)
ggplot(df6, aes(x=Product, y=NA_Sales, col=Product)) + 
  geom_point() + geom_smooth() +
  theme_classic()

# Comparing NA_Sales with Product_ID and add a theme (classic)
ggplot(df6, aes(x=Product, y=Global_Sales, col=Product)) + 
  geom_point() + geom_smooth() +
  theme_classic()

# Specify boxplot function.
boxplot(df6$NA_Sales)

# Stacked bar chart 
qplot(EU_Sales, fill=EU_Sales, data=df6, geom='bar')

#Determine the normality of the data set 
# Specify qqnorm function (draw a qqplot).
qqnorm(df6$Product)

# Specify qqline function.
qqline(df6$Product)

# Measure the normality 
# Q-Q plot:
qqnorm(df6$Product)
# Add a reference line:
qqline(df6$Product, col='red')

# Shapiro-Wilk test for Global Sales:
shapiro.test((df6$Global_Sales))

# Shapiro-Wilk test for NA_Sales: 
shapiro.test((df6$NA_Sales))

# Shapiro-Wilk test for EU_Sales: 
shapiro.test((df6$EU_Sales))

# Specify the skewness and kurtosis functions for NA_Sales 
skewness(df6$NA_Sales) 
kurtosis(df6$NA_Sales)

# Specify the skewness and kurtosis functions for EU_Sales 
skewness(df6$EU_Sales) 
kurtosis(df6$EU_Sales)

# Specify the skewness and kurtosis functions for Global_Sales 
skewness(df6$Global_Sales) 
kurtosis(df6$Global_Sales)

# Determine which graphs best suit the sales data 
# Comparing EU_Sales and NA_Sales with colour (outline only).
ggplot(df6, aes(x=EU_Sales, col=Global_Sales)) + 
  geom_bar()

# Comparing EU_Sales with NA_Sales and add a title.
ggplot(df6, aes(x=EU_Sales, fill=Product)) + 
  geom_bar(position='dodge') +
  ggtitle("EU Sales by Product_ID")

# EU_Sales per product ID with smoothing lines only.
ggplot(df6, aes(x=EU_Sales, y=Product, col=Product)) + 
  geom_smooth(se=FALSE)

# Categorical variable = Global_Sales.
ggplot(df6, aes(x=Global_Sales)) + 
  geom_bar()

################################################################################

# Week 6 

# Create a simple linear regression model.
# Determine the correlation between the sales columns.
# View the output.
# Create plots to view the linear regression.


# Create a simple linear regression model 
# Find a correlation.
cor(df6)

# Plot the relationship with base R graphics.
plot(df6$EU_Sales, cpi$NA_Sales)

plot(df6$NA_Sales, df6$Product)

plot(df6$Global_Sales, df6$EU_Sales)

plot(df6$EU_Sales, df6$Global_Sales)

plot(df6$NA_Sales, df6$Global_Sales)

plot(df6$Product, df6$Global_Sales)

# Fit a simple linear regression model.
# Create a model with the x variable as Global Sales 
model1 <- lm(Product~Global_Sales,
             data=df6)

# View the model.
model1

# View more outputs for the model - the full regression table.
summary(model1)

# Create a model with the x variable as EU Sales 
model2 <- lm(Product~EU_Sales,
             data=df6)

# View the model.
model2

# View more outputs for the model - the full regression table.
summary(model2)

# Create a model with the x variable as Global Sales 
model3 <- lm(Product~NA_Sales,
             data=df6)

# View the model.
model3
