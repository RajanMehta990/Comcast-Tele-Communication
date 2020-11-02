getwd()
setwd("C:/Users/rajan/Documents/Datasets")

library(ggplot2)
library(caTools)
library(dplyr)
library(lubridate)

#### Importing Data sets

comsdata <- read.csv("Comcast Telecom Complaints data.csv")

#### Finding NAs

sum(is.na(comsdata))
colSums(is.na(comsdata))


# Adding a new variable as a count

Countsdata = "1"
comsdata$count <- Countsdata
head(comsdata)

#### Trend Chart for Daily and Monthly ####

# Daily


date1 <- parse_date_time(comsdata$Date,orders = "%d!-%m!-%y!") # (Using lubridate Library )
date1
comsdata$Date <- date1
head(comsdata)


datetable <-as.data.frame( table(comsdata$Date,comsdata$count))
datetable
names(datetable)[1] <- paste("Dates")
names(datetable)[3] <- paste("Counts")

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="top" )


ggplot(data = datetable,aes(x = reorder(Dates,-Counts) ,y = Counts)) + geom_bar(stat = "identity",fill = "Red",color = "blue")  +bar_theme1 + ggtitle("Daily Number of Complaints") + geom_text(aes(label = Counts),vjust = -0.1,size = 3, color = "blue")

# Monthly

Month<- format(comsdata$Date,"%m")  # Month Done
Month <- month.abb[as.integer(Month)]
Month
comsdata$Months <- Month
head(comsdata)
monthtable <- as.data.frame(table(comsdata$Months,comsdata$count))
monthtable
names(monthtable)[1] <- paste("Months")
names(monthtable)[3] <- paste("Count")
head(monthtable)
names(monthtable)
monthtable$Var2 <- NULL


ggplot(data = monthtable,aes(x = Months ,y = Count)) + 
  geom_bar(stat = "identity",fill = "Red",color = "blue") + 
  ggtitle("Monthly Number of Complaints") + 
  geom_text(aes(label = Count),vjust = -0.2,size = 3.5, color = "blue")

#### Table With the Frequency of Complain Types ####


complaints <- as.data.frame(table(comsdata$Received.Via))
complaints

names(complaints)[1] <- paste("Received")
names(complaints)[2] <- paste("ComplaintsNo")
complaints


label = paste(complaints$Received,"-",complaints$ComplaintsNo)
label

library(plotrix)
pie3D(complaints$ComplaintsNo,labels = label ,
      explode = 0.1,
      font = 5,
      main = "Frequency of Complaint Types")


####  Which complaint types are maximum i.e., around internet, network issues, or across any other domains. ####

customercomplaints <- comsdata$Customer.Complaint

#Types <- ifelse(grepl("*speed|*internet|*cable|*Cable|*data|*Internet|*Data |*usage",customercomplaints),"Internet",ifelse(grepl("*bill|*Bill",customercomplaints),"Billing Issue", ifelse(grepl("*calls|*network|*disconnect|*connect|*Connection|*Connection",customercomplaints),"Network Issue","Other Reasons")))

#Types <- ifelse(grepl("*speed|*internet|*cable|*Cable|*data|*Internet|*Data |*usage",customercomplaints),"Internet",ifelse(grepl("*bill|*Bill",customercomplaints),"Billing Issue", ifelse(grepl("*calls|*network|*disconnect|*connect|*Connection|*Connection",customercomplaints),"Network Issue","Other Reasons")))

Types <- ifelse(grepl('*bill| *Bill',customercomplaints),"Billing",ifelse(grepl('*charge| *Charge',customercomplaints),'Charges',ifelse(grepl('email',customercomplaints),"Email",ifelse(grepl("*speed|*internet|*cable|*Cable|*data|*Internet|*Data |*usage",customercomplaints),"Internet",ifelse(grepl('*calls|*network|*disconnect|*connect|*Connection|*Connection',customercomplaints),"Network","Others")))))
Types
comsdata$Complaint_Type <- Types
head(comsdata)
complaint_types<- data.frame(table(comsdata$Complaint_Type,comsdata$count)) 
names(complaint_types)[1] <- "Complaint Types"
names(complaint_types)[3] <- "Total"
complaint_types$Var2 <- NULL
complaint_types
complaint_types[which.max(complaint_types$Total),"Complaint Types"]

###########################################################
####  Open and Close New categorical Variable creation ####
###########################################################

status <- comsdata$Status

open.closed <- ifelse(grepl('Open',status)|grepl('Pending',status),"Open","Closed")
open.closed
table(open.closed)

comsdata$Open_Closed <- open.closed
head(comsdata)


############################################
#### stack bar chart of State wise  ####
############################################

statewisecomplaints <- as.data.frame(table(comsdata$Complaint_Type,comsdata$State))
statewisecomplaints
dim(statewisecomplaints)
names(statewisecomplaints)
names(statewisecomplaints)[1] <- paste("Compalint_Type")
names(statewisecomplaints)[2] <- paste("State")
names(statewisecomplaints)[3] <- paste("Total_Complaints")
bar_theme2<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position ="bottom"  )
ggplot(statewisecomplaints,aes(x = State,y = Total_Complaints,fill = Compalint_Type )) + geom_bar(stat = "identity", legend = T ) + bar_theme2 + ggtitle("State and Complaint Types wise Chart")


###########################################
#### Which State has Maximum Complaints ####
###########################################


state_maximum_complaints <- data.frame(table(comsdata$State))
names(state_maximum_complaints)[1] <- "States"
names(state_maximum_complaints)[2] <- "Total_no_complaints"
state_maximum_complaints[which.max(state_maximum_complaints$Total_no_complaints),"States"]

################################################################
#### State wise Highest Percentage of unresolved complaints ####
################################################################

newcoms <- subset(comsdata, Open_Closed == "Open")
newcoms
head(newcoms)
names(newcoms)
unresolved_complaints <- as.data.frame(table(newcoms$Open_Closed,newcoms$State))
names(unresolved_complaints)
unresolved_complaints
unresolved_complaints$Var1 = NULL
names(unresolved_complaints)[1] <- paste("State")
names(unresolved_complaints)[2] <- paste("Counts")
bar_theme3<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                  legend.position="none")
percentage <- unresolved_complaints$Counts*100/sum(unresolved_complaints$Counts)
percentage
percentage <- round(percentage, digit = 2)
unresolved_complaints$percentage <- percentage
names(unresolved_complaints)
ggplot(data=unresolved_complaints,aes(x = State, y = percentage)) + geom_bar(stat = "identity",col = "orange",fill = "skyblue") + bar_theme3 + geom_text(aes(label = percentage),vjust = -0.2, size = 4, color = "blue") + ggtitle("State Wise Unresolved Complaints in %")
highest_percentage_state_unresolvedcomplaints <- unresolved_complaints[which.max(unresolved_complaints$percentage),"State"]
highest_percentage_state_unresolvedcomplaints # It is clearly says that "Georgia" has the highest percentage to Unresolved Complaints

############################
#### Provide the percentage of complaints resolved till date, which were received through theInternet and customer care calls. ####


newdata1 <- subset(comsdata, Open_Closed == "Closed")
newdata1
resolved <- as.data.frame(table(newdata1$Open_Closed,newdata1$Received.Via ))
resolved
resolved$Var1 = NULL
names(resolved)[1] <- paste("Received_via")
names(resolved)[2] <- paste("Counts")
percentage1 <- paste(round(resolved$Counts*100/ sum(resolved$Counts)),"%")
percentage1
resolved$percentage <- percentage1
resolved
label = paste(resolved$Received_via," - ",resolved$percentage)
pie3D(resolved$Counts,labels = label,
      explode = 0.1,
      font = 5,
      col = c("blue","red"))
