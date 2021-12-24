# Analysis of Hotel Prices
# NAME: Rohit Rawat
# EMAIL: rrrohit28@gmail.com  
# COLLEGE : IITB

# Combine all the 42 city data together into a single data frame using rbind function

hotels <- read.csv("AllCitiesData.csv")
View(hotels)

summary(hotels)
library(psych)
describe(hotels)
# Continuos variables: Population, RoomRent, Airport, HotelPincode, HotelCapacity
# Categorical variables: CityRank, IsMetroCity, IsTouristDestination, IsWeekend, IsNewYearEve,
#                         StarRating, FreeWifi, FreeBreakfast, HasSwimmingPool 
# Other variables: CityName, Date, HotelName, HotelAddress, HotelDescription.

attach(hotels)

# Y = RoomRent
# X1 = StarRating, X2 = CityRank, X3 = HasSwimmingPool

table(CityRank)
table(HasSwimmingPool)
library(lattice)
bwplot(StarRating)
bwplot(RoomRent,main="RoomRent with Outliers")
boxplot(RoomRent,horizontal = TRUE, outline = FALSE, main="Room Rent excluding outliers",
        xlab= "Room Rent")

# We can see that RoomRent has outliers which can skew our final results.
# We try to remove the outliers of the given dataset. The difference between step
# and first and third quantiles defines the limit for outliers.

lh <- quantile(hotels$RoomRent,0.25)
uh <- quantile(hotels$RoomRent,0.75)
step <- 1.5 * (uh-lh)

# We remove the outliers using step.
hotels1 <- hotels[which(RoomRent>=(lh-step)&RoomRent<=(uh+step)),]
View(hotels1)

# Draw Scatterplots to show relationship between independent and dependent variables.

scatterplot(RoomRent ~ StarRating,data=hotels1, main="Scatterplot StarRating vs RoomRent")

bwplot(HasSwimmingPool ~ RoomRent, data=hotels1, horizontal=TRUE, 
        xlab = "Room Rent",  ylab = "Has Swimming Pool", main = "2 = Has Swimming Pool
        1 = No Swimmng Pool")
bwplot(CityRank ~ RoomRent, data=hotels1, horizontal=TRUE, 
       xlab = "Room Rent",  ylab = "City Rank", main= "Room Rent vs City Rank using bwplot ")
scatterplot(RoomRent ~ CityRank,data=hotels1, main="Scatterplot City Rank vs Room Rent")

# Creating corrgram and variance-covariance matrix
library(corrgram)
corrgram(hotels1[,c(11,4,12,20)], order=FALSE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of Hotels")

cor(hotels1[,c(11,4,12,20)])

# We wil be checking two hypotheses.

t.test(RoomRent~HasSwimmingPool, hotels1)

t.test(RoomRent~IsWeekend, hotels1)

attach(hotels1)

#So, we will fit three models here, first seeing HasSwimmingPool
# and IsWeekend in isolation, and then 
# taking all the independent variables.

fit1<-lm(formula = RoomRent ~ HasSwimmingPool, data = hotels1)
summary(fit1)

fit2<-lm(formula = RoomRent ~ IsWeekend, data=hotels1)
summary(fit2)


fit3<-lm(formula = RoomRent ~ StarRating + CityRank + HasSwimmingPool + IsWeekend +
      IsMetroCity + IsTouristDestination + IsNewYearEve + 
     Airport + FreeBreakfast + FreeWifi + HotelCapacity, data = hotels1)
summary(fit3)

