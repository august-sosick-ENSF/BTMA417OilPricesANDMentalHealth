#Author: August Sosick 30050299
#Date: 2020-11-18
#Assignment: Mini-Project
#Class: BTMA 431

library(gtrendsR)
library(tidyverse)
library(XML)
library(RCurl)
library(rvest)
library(ggplot2)
library(tinytex)

########
# NOTE:
# In order to verify the results in my RMD documents, there is a global environment provided in the zip which you can import.
#######

#H0: The frequency of negative words searched on google during a month and the average oil price are not correlated
#H1: The frequency of negative words searched on google during a month and the price of oil are correlated.


#Number of data points return from 5 years.
#Number of data points return from 5 years.
periodCount <- 261
negativeDF <- data.frame("Date" = 1:periodCount, "Depression" = "", "Guilt" = "", 
                         "Hatred" = "", "Suicide" = "","Death" = "", "Debt" = "",
                         "Angry" = "", "Shitty" = "", "Horrible" = "", 
                         "Alcohol" = "", "Drugs" = "", "Sadness" = "",
                         "Divorce" = "", "Therapy" = "")
i <- 2 #skip Date column
while (i <= length(negativeDF)) {
  results <- gtrends(keyword = colnames(negativeDF)[i], geo = "CA-AB", time = "today+5-y")
  
  #Add the data column only once
  if(i == 2){
    negativeDF$Date <- results %>% .$interest_over_time %>% .$date
  }
  negativeDF[i] <- results %>% .$interest_over_time %>% .$hits
  i <- i + 1
}

#Insure that the data column type is recognized as a Date
negativeDF$Date <- as.Date(negativeDF$Date, format = "%Y-%m-%d")
#Create a new Month and Year column based off of the numeric dates in from ex: 2020-01-01
negativeDF$Month <- months(negativeDF$Date)
negativeDF$Year <- format(negativeDF$Date, format = "%y")
#Aggregate the information by finding the average hit rate for each term across each month
#and place that information into emotionsDF
emotionsDF <- aggregate(. ~ Month + Year, negativeDF, mean)
emotionsDF <- subset(emotionsDF, select = -c(Date))


#Request the html for the webpage
#https://www.nrcan.gc.ca/our-natural-resources/energy-sources-
#distribution/clean-fossil-fuels/crude-oil/oil-pricing/18087"
urlAdd <- "https://www.nrcan.gc.ca/our-natural-resources/energy-sources-distribution/clean-fossil-fuels/crude-oil/oil-pricing/18087"

web_content <- read_html(urlAdd)

#Retrieve all the links by searching for "a" node with "href" attribtues
all_links <- web_content %>% html_nodes("a") %>% html_attr('href')
#select only links which contain "selected-crude-oil-price-daily" 
#as those links contain links to the montly data.
all_links <- all_links[which(regexpr("selected-crude-oil-price-daily", all_links)>0)]


#Create dataframe containing date and average oil prices.
oilPriceDF <- data.frame("Date"= "", "Average Oil Prices" = "")
index <- 1
#loop through all of the links we determined in the code block above
while(index < length(all_links)){
  #read the HTML page contents
  this_page_content <- read_html(all_links[index])
  #Extract the tables from the page
  tables <- html_nodes(this_page_content, "table")
  #Get the dataframe of the first table, the one pictured above,
  #and place into the oilDt variable.
  oilDt <- html_table(tables, fill = TRUE)[[1]]
  
  #Rename Column Names due to spaces
  colnames(oilDt) <- c("Date", "Exchange_Rate", "WTI_Crushing",
                       "Brent_Sullom_Vow_UK", "Mixed_Sweet_Blend_Edmonton",
                       "Synthetic_Edmonton", "Western_Canada_Select_Hardistry",
                       "Implied_Bitumen_Hardisty")
  
  #Begin to loop through each row in the table
  for(i in 1:nrow(oilDt)){
    #Do not include rows with hyphens or the final column called Average
    if(oilDt$Exchange_Rate[i] != "-" & oilDt$Date[i] != "Average"){
      #Include the current date, and the mean of the costs for each 
      #of the listed crude oils.
      #Try Catch to mismatched columns in older data sets.
      tryCatch({
        oilPriceDF[nrow(oilPriceDF)+1,] <- c(oilDt$Date[i],
                                             mean(c(as.numeric(oilDt$WTI_Crushing[i]),
                                                    as.numeric(oilDt$Brent_Sullom_Vow_UK[i]),
                                                    as.numeric(oilDt$Mixed_Sweet_Blend_Edmonton[i]), 
                                                    as.numeric(oilDt$Synthetic_Edmonton[i]),
                                                    as.numeric(oilDt$Western_Canada_Select_Hardistry[i]),
                                                    as.numeric(oilDt$Implied_Bitumen_Hardisty[i])),
                                                  na.rm = TRUE))
      },
      warning=function(err){
        #Do nothing. Warnings are cause by additional columns in older data sets. 
        #They should just be ignored and produced only NA's in table which are dealt with later.
      })
    }
  }
  index <- index + 1
}


#remove inital row which is empty.
oilPriceDF <- oilPriceDF[-c(1),]
#typeset average.oil.prices variables as numerics
oilPriceDF$Average.Oil.Prices <- as.numeric(oilPriceDF$Average.Oil.Prices)
#typeset the dates as dates and indicate proper format
oilPriceDF$Date <- as.Date(oilPriceDF$Date, format = "%Y-%m-%d")
#Create month and year table
oilPriceDF$Month <- months(oilPriceDF$Date)
oilPriceDF$Year <- format(oilPriceDF$Date, format = "%y")
#Aggregate the mean of Average Oil Prices by month and year into 
#the dataframe "aggregatedOilPrices"
aggregatedOilPrice <- aggregate(Average.Oil.Prices ~ Month + Year, oilPriceDF, mean)

#create an OilPrice column in the dataframe containing our averaged monthly hits by search 
#term
emotionsDF$OilPrice <- NA
#Search for the same month and year across the two dataframes and 
#combine where a match is determined.
for(i in 1:nrow(aggregatedOilPrice)){
  for(j in 1:nrow(emotionsDF)){
    if(aggregatedOilPrice$Month[i] == emotionsDF$Month[j] & 
       aggregatedOilPrice$Year[i] == emotionsDF$Year[j]){
      emotionsDF$OilPrice[j] <- aggregatedOilPrice$Average.Oil.Prices[i]
    } 
  }
}
#remove Na's which are where no month year matches were found
emotionsDF <- na.omit(emotionsDF)

#Plotting the information
emotionsDF$AvgNegEmotion <- NA
i<-1
while(i <= nrow(emotionsDF)){
  j <- 2
  val <- 0
  while(j <= ncol(emotionsDF)-1){
    val <- val + as.numeric(emotionsDF[i,j])
    j <- j + 1
  }
  emotionsDF$AvgNegEmotion[i] <- (val / ncol(emotionsDF)-4)
  i <- i + 1
}


g <- ggplot(emotionsDF, aes(AvgNegEmotion, OilPrice))
g + geom_jitter(width = .5, size=1) + 
  labs(y="Oil Price", 
       x="Averaged Negative Emotion Hits", 
       title="Oil Price vs. Negative Emotion")



#Regression Model
regressionModel <- lm(OilPrice ~ AvgNegEmotion, data = emotionsDF)
summary(regressionModel)

