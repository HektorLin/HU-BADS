# Practicing CACI demo



# R connection to csv & excel

# Set up directory
setwd("C:/Users/Hector Lin/Documents/HU-courses-practice/CACI")

# Read csv
ice = read.csv("PersonIceCreamImportance.csv")

#install and read xlxs, remember to install lastest JRE
#install.packages("xlsx")
#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk-11.0.1')
library("xlsx")
chocolate = read.xlsx("Data_Chocolate_allinterviews.xlsx", sheetIndex = 2)

# Descriptive stat function
summary(chocolate)
head(chocolate,10)



# Loading online data
store.df <- read.csv ("http://goo.gl/QPDdMl")

# Aggregate sales by country
p1sales.sum <- aggregate ( store.df$p1sales, 
                           by= list ( country = store.df$country ), sum )

# Plot Sales by Country with rworldmap()
#install.packages("rworldmap")
#install.packages("RColorBrewer")
library( rworldmap )
library( RColorBrewer )
p1sales.map <- joinCountryData2Map(p1sales.sum ,
                                   joinCode ="ISO2",
                                   nameJoinColumn ="country")
mapCountryData ( p1sales.map , nameColumnToPlot ="x",
                 mapTitle ="Total P1 sales by Country ",
                 colourPalette = brewer.pal(7, "Greens"),
                 catMethod ="fixedWidth", addLegend = FALSE )

