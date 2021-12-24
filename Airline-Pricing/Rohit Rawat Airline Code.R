# Analysis of Airline Ticket Pricing
# NAME: Rohit Rawat
# EMAIL: rrrohit28@gmail.com
# COLLEGE : IITB

# Hint 1
Air <- read.csv("SixAirlinesData.csv")
View(Air)
dim(Air)

# Hint 2
summary(Air)

# Hint 3,4
# Boxploting variables independently
par(mfrow=c(1, 2))
boxplot(Air$FlightDuration, main="Flight Duration")
boxplot(Air$FractionPremiumSeats, main="FractionPremiumSeats")
par(mfrow=c(1, 3))
boxplot(Air$SeatsEconomy, main="SeatsEconomy")
boxplot(Air$SeatsPremium, main="SeatsPremium")
boxplot(Air$SeatsTotal, main="SeatsTotal")

boxplot(Air$PitchEconomy, main="PitchEconomy")
boxplot(Air$PitchPremium, main="PitchPremium")
boxplot(Air$PitchDifference, main="PitchDifference")

boxplot(Air$WidthEconomy, main="WidthEconomy")
boxplot(Air$WidthPremium, main="WidthhPremium")
boxplot(Air$WidthDifference, main="WidthDifference")

boxplot(Air$PriceEconomy, main="PriceEconomy")
boxplot(Air$PricePremium, main="PricehPremium")
boxplot(Air$PriceRelative, main="PricehRelative")

boxplot(Air$PriceEconomy, main="PriceEconomy")
boxplot(Air$PricePremium, main="PricehPremium")
boxplot(Air$PriceRelative, main="PricehRelative")
par(mfrow=c(1, 1))

#Plotting some variables against relative price

boxplot( PriceRelative ~ Airline, data=Air, horizontal=TRUE,  
        ylab="Airline", xlab="Relative Price", 
        main="Comparison of relative Prices in different Airlines", outline=FALSE)
boxplot( PriceRelative ~ Aircraft, data=Air, horizontal=TRUE,  
         ylab="Airline", xlab="Relative Price", 
         main="Comparison of relative Prices in different Aircrafts", outline=FALSE)
scatterplot(FlightDuration ~ PriceRelative , data=Air, 
            xlab="Relative Price", ylab="Flight Duration",
            main="Scatter plot of Relative Price vs Flight Duration", 
            labels=row.names(store), pch=16)
boxplot( PriceRelative ~ TravelMonth, data=Air, horizontal=TRUE,  
         ylab="Month", xlab="Relative Price", 
         main="Comparison of relative Prices in different travelling months", outline=FALSE)
boxplot( PriceRelative ~ IsInternational, data=Air, horizontal=TRUE,  
         ylab="Flight Type", xlab="Relative Price", 
         main="Comparison of relative Prices Flight Type", outline=FALSE)
scatterplot( PriceRelative ~ FractionPremiumSeats, data=Air, horizontal=TRUE,  
         ylab="Seats Ratio", xlab="Relative Price", 
         main="Comparison of relative Prices w.r.t. Seat Ratio", outline=FALSE)
boxplot( PriceRelative ~ PitchDifference, data=Air, horizontal=TRUE,  
         ylab="Pitch Difference", xlab="Relative Price", 
         main="Comparison of relative Prices w.r.t. Pitch Difference", outline=FALSE)
boxplot( PriceRelative ~ WidthDifference, data=Air, horizontal=TRUE,  
         ylab="Width Difference", xlab="Relative Price", 
         main="Comparison of relative Prices w.r.t Width Difference", outline=FALSE)
scatterplot( SeatsTotal ~ PriceRelative , data=Air, horizontal=TRUE,  
             ylab="Total Seats", xlab="Relative Price", 
             main="Comparison of relative Prices w.r.t. Seat Ratio", outline=FALSE)

# Hint 5
library(corrgram)
corrgram(store, order=FALSE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of Air variables")

cor(Air[,c(3, 6:18)])

#Hint 6,7
#Hypothesis 1: The average cost of a premium ticket is more than that of a economy ticket
#Assumptions: The data sets is normally distributed. The data points are independent of each other. 
# The vectors are numeric here.

t.test(PriceEconomy,PricePremium,data=Air)

#Hypothesis 2: Boeing planes have higher relative ticket costs than AirBus carriers.  
#Assumptions: The data sets is normally distributed. The data points are independent of each other. 
# Relative price is numeric and Aircraft is a dichotomous variable

t.test(PriceRelative~Aircraft,data=Air)

#Hypothesis 3: International flights have higher relative ticket costs than domestic planes.
#Assumptions: The data sets is normally distributed. The data points are independent of each other. 
# Relative price is numeric and Flight type is a dichotomous variable

t.test(PriceRelative~IsInternational,data=Air)


# Before fitting a linear model, we will create indicator variables for the columns which are non-numeric
# Yes we are creating a lot of new columns but they have an effect on the reltaive price.

Air$IsInternational1 <- as.numeric(Air$IsInternational=="International")
Air$IsInternational2 <- as.numeric(Air$IsInternational=="Domestic")
Air$Airline1         <- as.numeric(Air$Airline=="British")
Air$Airline2         <- as.numeric(Air$Airline=="Delta")
Air$Airline3         <- as.numeric(Air$Airline=="Jet")
Air$Airline4         <- as.numeric(Air$Airline=="Virgin")
Air$Airline5         <- as.numeric(Air$Airline=="Singapore")
Air$Airline6         <- as.numeric(Air$Airline=="AirFrance")
Air$Aircraft1        <- as.numeric(Air$Aircraft=="AirBus")
Air$Aircraft2        <- as.numeric(Air$Aircraft=="Boeing")
Air$TravelMonth1     <- as.numeric(Air$TravelMonth=="Jul")
Air$TravelMonth2     <- as.numeric(Air$TravelMonth=="Aug")
Air$TravelMonth3     <- as.numeric(Air$TravelMonth=="Sep")
Air$TravelMonth4     <- as.numeric(Air$TravelMonth=="Oct")

# Hint 8,9
# fitting the linear model
fit <- lm(PriceRelative ~ FlightDuration + PitchEconomy + PitchPremium + WidthPremium + PriceEconomy + 
            PitchDifference + WidthDifference + SeatsPremium + IsInternational1 + IsInternational2 +
            Airline1 + Airline2 + Airline3 + Airline4 + Airline5 + Airline6 +
            Aircraft1 + Aircraft2 + TravelMonth1 + TravelMonth2 + TravelMonth3 + TravelMonth4,  Air)
summary(fit)
