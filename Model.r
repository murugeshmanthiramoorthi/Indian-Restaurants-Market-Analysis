# Loading data
data=read.csv("Sample hotel data.csv")

# Let us now look at the variables present in tha dataset
names(data)

# Now, we will replace the missing value in the review columns with its median values
for(i in 6:11){
  data[, i]=ifelse(is.na(data[, i]), median(data[, i], na.rm=TRUE),data[, i])
}

# Now, we will scale the numerical entries to its standardized form
for(i in c(2, 6:11)){
  a=!(abs(scale(data[, i])[, 1])>3)
  data=data[a, ]
}


data[, c(2, 6:11)] = scale(data[, c(2, 6:11)])

# Let us now fit a baseline model on the data available using linear regression
fit=lm(Review_Overall_Rating~Rating_Value+Rating_Location+Rating_Sleep_Quality+Rating_Rooms+Rating_Cleanliness+Rating_Service, data=data)

# we will look at the summary of the fit to analyse it
summary(fit)

# Let us now include Reviw_Type variable in the model and analyse it
fit1=lm(Review_Overall_Rating~Rating_Value+Rating_Location+Rating_Sleep_Quality+Rating_Rooms+Rating_Cleanliness+Rating_Service+Review_Type, data=data)

summary(fit1)

# Now, we will convert the review date and month of visit variable sinto date format
data$date_of_review1=as.character(data$date_of_review)
data$date_of_review2=as.Date(data$date_of_review1, "%m/%d/%Y")
data$date_of_review2[is.na(data$date_of_review2)] =as.Date(data$date_of_review1[is.na(data$date_of_review2)], "%m-%d-%Y")

data$date_of_review2

data$Month_of_Visit1= as.character(data$Month_of_Visit)
data$Month_of_Visit1=paste("1-", data$Month_of_Visit1, sep="")
data$Month_of_Visit1=as.Date(data$Month_of_Visit1, "%d-%b-%y")

# We will include the time difference between visit and review
data$timedist=data$date_of_review2-data$Month_of_Visit1

# We are removing the data which has any na values
data=na.omit(data)

# Now we will fit a new linear regression including the new feature
fit2=lm(Review_Overall_Rating~Rating_Value+Rating_Location+Rating_Sleep_Quality+Rating_Rooms+Rating_Cleanliness+Rating_Service+Review_Type+timedist, data=data)

# Now let us examine the final model
summary(fit2)
