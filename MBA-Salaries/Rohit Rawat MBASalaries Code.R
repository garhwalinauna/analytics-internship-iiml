# Analysis of MBA1 SALARIES
# NAME: Rohit Rawat
# EMAIL: rrrohit28@gmail.com  
# COLLEGE : IITB

MBA <- read.csv("MBA Starting Salaries Data.csv")
View(MBA)

# Filter out the people who didn't fill the survey and who were not placed.
MBA1 <- MBA[which(!(MBA$salary=="0"|MBA$salary=="998"|MBA$salary=="999")),]

# summary statistics
library(psych)
describe(MBA1)

#Boxplots of each variable of placed students
library(car)
boxplot(MBA1$age, horizontal = TRUE, main = "Age(Placed)", outline= FALSE)
boxplot(MBA1$gmat_tot, horizontal = TRUE, main = "Total GMAT Score(Placed)", outline= FALSE)
boxplot(MBA1$gmat_qpc, horizontal = TRUE, main = "Quantitative GMAT percentile(Placed)", outline= FALSE)
boxplot(MBA1$gmat_vpc, horizontal = TRUE, main = "Verbal GMAT percentile(Placed)", outline= FALSE)
boxplot(MBA1$gmat_tpc, horizontal = TRUE, main = "Overall GMAT percentile(Placed)", outline= FALSE)
boxplot(MBA1$s_avg, horizontal = TRUE, main = "Spring MBA1 average(Placed)", outline=FALSE)
boxplot(MBA1$f_avg, horizontal = TRUE, main = "Fall MBA1 average(Placed)", outline= FALSE)
boxplot(MBA1$work_yrs, horizontal = TRUE, main = "Work Experience(Placed)", outline= FALSE)
hist(MBA1$salary, main = "Salary(Placed)",breaks=20)


# Gender male=1, female=2 / Placed Students
table(MBA1$sex)
# quartile ranking (1 is top, 4 is bottom) / Placed Students
table(MBA1$quarter)
# first language (1=English; 2=other) / Placed Students
table(MBA1$frstlang)
# degree of satisfaction with MBA1 program (1= low, 7 = high satisfaction) / Placed Students
table(MBA1$satis)

#boxplots of variables with each other 


#Corrogram of variables of Placed Students 
library(corrgram)
corrgram(MBA1, order=FALSE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of MBA Salaries vairables")
# Clearly we can observe that the four GMAT variables
#    are highly correlated.
# salary seems to be showing high correlation to first language,
#    Working experience, age and gender.
cor(MBA1)

# ScatterPlots and contigency tables

# Contigency tables and chi square tests of categorical variables
SexSatis<-xtabs(~ sex+satis, data=MBA1)
addmargins(SexSatis)
chisq.test(SexSatis)

SexQuarter<-xtabs(~ sex+quarter, data=MBA1)
addmargins(SexQuarter)
chisq.test(SexQuarter)

SatisQuarter<-xtabs(~ satis+quarter, data=MBA1)
addmargins(SatisQuarter)
chisq.test(SatisQuarter)

SatisFrstLang<-xtabs(~ satis+frstlang,data=MBA1)
addmargins(SatisFrstLang)
chisq.test(SatisFrstLang)

FrstLangQuarter<-xtabs(~ frstlang+quarter, data=MBA1)
addmargins(FrstLangQuarter)
chisq.test(FrstLangQuarter)

# From the above done Chi square test done on the set of categorical data columns,
# we have failed to reject to reject the Null Hypothesis in every case because
# the p-value is >0.05

# Now we check some Hypothesis using t-tests

# Hypothesis 1: The average salary of a male MBA graduate is more than a female MBA graduate.
# Sex is a dichotomous variable.

t.test(salary~sex,data=MBA1)

#Hypothesis 2: The average salary of a native English speaker is higher.
# First Language here is defined to be a dichotomous variable.

t.test(salary~frstlang,data=MBA1)

#Hypothesis 3: Gradautes with higher work eperience have higher salaries.
t.test(salary,work_yrs,data=MBA1)

#Hypothesis 4: Graduates with better GMAT percentile have higher salaries.
t.test(salary,gmat_tpc,data=MBA1)

# We reject the independence of age and sex with salary.

#Now let us start fitting some models

fit1 <- lm(salary ~  age + sex + frstlang + quarter + gmat_tpc + satis + gmat_tot + work_yrs + s_avg
           + f_avg + gmat_qpc, MBA1)
summary(fit1)

fit2 <- lm(salary ~  age + sex + frstlang + quarter +  satis + work_yrs + s_avg  + f_avg , MBA1)
summary(fit2)

fit3 <- lm(salary ~  age + sex + frstlang + quarter + work_yrs , MBA1)
summary(fit3)




#Now we look at the students who were not placed.

MBA2 <- MBA[which(MBA$salary=="0"),]
View(MBA2)

# Contigency tables and chi square tests of categorical variables
SexSatis1<-xtabs(~ sex+satis, data=MBA2)
addmargins(SexSatis1)
chisq.test(SexSatis1)

SexQuarter1<-xtabs(~ sex+quarter, data=MBA2)
addmargins(SexQuarter1)
chisq.test(SexQuarter1)

SexFrstLang1<-xtabs(~ sex+frstlang, data=MBA2)
addmargins(SexFrstLang1)
chisq.test(SexFrstLang1)

SatisQuarter1<-xtabs(~ satis+quarter, data=MBA2)
addmargins(SatisQuarter1)
chisq.test(SatisQuarter1)

SatisFrstLang1<-xtabs(~ satis+frstlang,data=MBA2)
addmargins(SatisFrstLang1)
chisq.test(SatisFrstLang1)

FrstLangQuarter1<-xtabs(~ frstlang+quarter, data=MBA2)
addmargins(FrstLangQuarter1)
chisq.test(FrstLangQuarter1)

# expect the satisfaction level and the first language, there is significant relationship
# between pairs of variables (p>0.05)


# Trying to fit a logistic model 
# First we create a column specifying whether the student got placed or not
# Placed = 1, Not placed 

# We delete the missing/unfilled/not disclosed values from the data set of salary column
MBA <- MBA[which(!(MBA$salary=="998"|MBA$salary=="999")),]
View(MBA)
# Placed = 1, Not Placed = 0.
MBA$salary <- ifelse(MBA$salary > 0, c(1), c(0))

# Now we will run our logistic analysis on salary variable

model <- glm(salary ~.,family=binomial(link='logit'),data=MBA)
summary(model)

anova(model, test="Chisq")
