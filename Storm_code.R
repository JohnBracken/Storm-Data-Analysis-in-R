#The ggplot2, gridExtra, and dplyr libraries will be used for this work
library(ggplot2)
library(dplyr)
library(gridExtra)

# Load the file.
storm_data <- read.csv("repdata_data_StormData.csv.bz2")

#Convert the fatalities and injuries columns to numeric values.
storm_data$FATALITIES <- as.numeric(as.character(storm_data$FATALITIES))
storm_data$INJURIES <- as.numeric(as.character(storm_data$INJURIES))

#Create a new data frame with the necessary columns needed for the analysis.
storm_economic_health_data <- select(storm_data, EVTYPE, FATALITIES, INJURIES,
                                     PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

#Replace the exponent factor levels of the property damage exponent with the appropriate
#number values, which are all powers of ten.  K,k = kilo (or 1000), M,m = millions (10^6)
#and B = billions (10^9), H,h = hundreds (100), digits 0 through 8 represent 10 to the power
#of the digit.  Blanks, question marks assign a value of zero to the row in which they are found,
#since no property damage is present for these entries.
#Plus and minus signs are assigned a value of 0, since property damage is present for these entries.
levels(storm_economic_health_data$PROPDMGEXP)
storm_economic_health_data$PROPDMGEXP <- recode(storm_economic_health_data$PROPDMGEXP, 'K' = '1000',
         'm' = '1000000', 'M' = '1000000','B' = '1000000000', '?' = '0',
        '-' = '0', '+' = '0', 'H' = '100', 'h' = '100','0' = '1','1' ='10', '2' ='100','3'='1000',
        '4' = '10000', '5'='100000','6'='1000000','7'='10000000','8'='100000000')

storm_economic_health_data$PROPDMGEXP <- as.numeric(as.character(storm_economic_health_data$PROPDMGEXP))
storm_economic_health_data$PROPDMGEXP[is.na(storm_economic_health_data$PROPDMGEXP)] <- 1


levels(storm_economic_health_data$CROPDMGEXP)
storm_economic_health_data$CROPDMGEXP <- recode(storm_economic_health_data$CROPDMGEXP, 'K' = '1000',
        'k' = '1000', 'm' = '1000000', 'M' = '1000000','B' = '1000000000', '?' = '0',
        '0' = '1', '2' ='100')

storm_economic_health_data$CROPDMGEXP <- as.numeric(as.character(storm_economic_health_data$CROPDMGEXP))
storm_economic_health_data$CROPDMGEXP[is.na(storm_economic_health_data$CROPDMGEXP)] <- 1

#Add two columns to the data that contain a dollar numeric value for property and crop damage.
storm_economic_health_data <- mutate(storm_economic_health_data, PROPDMGDOLLARS = PROPDMG*PROPDMGEXP,
                                     CROPDMGDOLLARS = CROPDMG*CROPDMGEXP)

#Aggregate the sums of the health and economic data, broken down by event type.  Arrange the data in
#descending order after aggregation.
fatalities_data <- aggregate(storm_economic_health_data$FATALITIES~storm_economic_health_data$EVTYPE, 
                             storm_economic_health_data,sum)
colnames(fatalities_data) <-c("EVTYPE", "FATALITIES")
fatalities_data <- arrange(fatalities_data,desc(FATALITIES))


injuries_data <- aggregate(storm_economic_health_data$INJURIES~storm_economic_health_data$EVTYPE, 
                             storm_economic_health_data,sum)
colnames(injuries_data) <-c("EVTYPE", "INJURIES")
injuries_data <- arrange(injuries_data,desc(INJURIES))


properties_data <- aggregate(storm_economic_health_data$PROPDMGDOLLARS~storm_economic_health_data$EVTYPE, 
                           storm_economic_health_data,sum)
colnames(properties_data) <-c("EVTYPE", "PROPDMGDOLLARS")
properties_data <- arrange(properties_data,desc(PROPDMGDOLLARS))
properties_data$PROPDMGDOLLARS <- properties_data$PROPDMGDOLLARS/1e9


crop_data <- aggregate(storm_economic_health_data$CROPDMGDOLLARS~storm_economic_health_data$EVTYPE, 
                             storm_economic_health_data,sum)
colnames(crop_data) <-c("EVTYPE", "CROPDMGDOLLARS")
crop_data <- arrange(crop_data,desc(CROPDMGDOLLARS))
crop_data$CROPDMGDOLLARS <- crop_data$CROPDMGDOLLARS/1e9




#Create two seperate bar plots for the health data (fatalities and injuries, and combine into one plot).
plot_fatalities <- ggplot(fatalities_data[1:10,], aes(reorder(EVTYPE,-FATALITIES),FATALITIES)) +
         geom_bar(stat="identity", col = "black", fill = "limegreen") + 
         labs(title = "Total Fatalities by Event",x = "Event Type",
              y= "Total Fatalities") + 
         theme(text = element_text(size=14),axis.text.x = element_text(angle = 45, hjust = 1))

plot_injuries <- ggplot(injuries_data[1:10,], aes(reorder(EVTYPE,-INJURIES),INJURIES)) +
  geom_bar(stat="identity", col = "black", fill = "#f4733b") + 
  labs(title = "Total Injuries by Event",x = "Event Type",
       y= "Total Injuries") + 
  theme(text = element_text(size=14),axis.text.x = element_text(angle = 45, hjust = 1))


#Combine the fatalities and injuries bar plots into two plots in a single
#figure health plot.
plot_health <- grid.arrange(plot_fatalities, plot_injuries, ncol= 2)



#Create two seperate bar plots for the economic data (crop and property damage) 
#and combine into one plot.
plot_property <- ggplot(properties_data[1:10,], aes(reorder(EVTYPE,-PROPDMGDOLLARS),PROPDMGDOLLARS)) +
  geom_bar(stat="identity", col = "black", fill = "#ff5477") + 
  labs(title = "Total Property Damage by Event",x = "Event Type",
       y= "Total Property Damage ($US billions)") + 
  theme(text = element_text(size=14),axis.text.x = element_text(angle = 45, hjust = 1))

plot_crops <- ggplot(crop_data[1:10,], aes(reorder(EVTYPE,-CROPDMGDOLLARS),CROPDMGDOLLARS)) +
  geom_bar(stat="identity", col = "black", fill = "#50bae8") + 
  labs(title = "Total Crop Damage by Event",x = "Event Type",
       y= "Total Crop Damage ($US billions)") + 
  theme(text = element_text(size=14),axis.text.x = element_text(angle = 45, hjust = 1))

plot_economic <- grid.arrange(plot_property, plot_crops, ncol = 2)





