#Dalton Anderson

rm(list=ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(readxl)

#preprocessing

#1.) Load in dataset
#Import data
pop_master <- read_excel("6304 Module 9 Assignment Data.xlsx")


#check data char and make them factors
names(pop_master)
colnames(pop_master)=tolower(make.names(colnames(pop_master)))
names(pop_master)
str(pop)

#check state column
unique(pop_master$state)
#so 5 states

#clean df
pop_master=select(pop_master, c(county, state, poptotal, popdensity, inmetro)) 


#2.)	Using the numerical portion of your U number as a random number seed, create a new data frame for your work.  
#The new data frame should have the following characteristics:
#a.	It should include a new variable showing population density of each county, calculated as total population divided by county area. 
county_info=pop_master %>%                   
  group_by(county) %>%                        
  summarise_at(vars(poptotal),              
               list(county_pop = sum))        
tempdf=pop_master %>%                         
  group_by(county) %>%                      
  summarise_at(vars(area),              
               list(county_area = sum)) 
county_info$county_area=temp$county_area
county_info$county_density=county_info$county_pop/county_info$county_area
#merge onto single df
pop_master = merge(pop_master,county_info)
#b. It should include only counties flagged as being rural, i.e., the inmetro variable of all included cases should be 0.
#only places outside of the city
pop_master=filter(pop_master, inmetro == 0)
#c. The state variable should be a factor variable.
#turn state into a factor
pop_master$state=as.factor(pop_master$state)
#check results
str(pop_master)
#d.	The data frame should have 100 cases in it, 
#these cases being a random sample of n=20 taken from each state.  
#You will have twenty counties from Illinois, twenty from Indiana, etc., merged into a single data frame.
tempdf = pop_master
set.seed(59657076)
sample <- tempdf
pop = sample %>% 
  group_by(state) %>% 
  sample_n(20)
rm(sample)
#check results
nrow(filter(pop,state == "IL"))
nrow(filter(pop,state == "IN"))
nrow(filter(pop,state == "MI"))
nrow(filter(pop,state == "OH"))
nrow(filter(pop,state == "WI"))

#Analysis 

#1.	Conduct a Levene test on the density variable when categorized by the state variable.  
#Report the results of this test and give a written interpretation of the results.
# Equality of variances test.       


#leveen test H0: all factor level variances are equal
#Hq: At least one factor level variance is different
leveneTest(popdensity~state, data=pop)
#p-value of .8307 I reject the null

#plot
boxplot(popdensity~state,
        main="Rural Population Density by State",
        col="red",
        data=pop)

tempdf=aggregate(popdensity~state,pop,var)
tempdf
#rm(tempdf)

max(tempdf$popdensity)/min(tempdf$popdensity)
#yes I have a probelm over 1.5, but we're working with a small sample size. We are alright for now
rm(tempdf)

#remove outliers
#tempdf = pop$popdensity[!pop$popdensity %in% boxplot.stats(pop$popdensity)$out]
#length(tempdf)- length(pop)

#2.	Conduct a One Way Analysis of Variance on the density variable using the state variable 
#as the factor.  Report and interpret the results of the F test.

#Ho all factor level means are equal
#Hq at least one factor level mean is different
pop.out=aov(popdensity~state,data=pop)
summary(pop.out)

#at least one of these factor level means are different
names(pop.out)
pop.out$coefficients

#show sample mean
tempdf=aggregate(popdensity~state,pop,mean)
tempdf

#3.	Conduct a Tukey HSD test and show the results as well as a plot of the results.
#Give a correct interpretation of these results, indicating any significant 
#(or close-to-significant) factor contrasts.

tempdf=TukeyHSD(pop.out)
tempdf
par(mar=c(5.1,8,4.1,2.1))
plot(tempdf,las=2)
par(mar=c(5.1,4.1,4.1,2.1))

#relevel example from class
#repop=pop
#repop$popdensity=relevel(repop$popdensity,"OH")
#model relevel data
#repop.out=aov(popdensity~state,data=repop)
#summary(repop.out)
#repop.out$coefficients
pop.out$coefficients
median(pop$popdensity)
#IL as base case
#interpretation
#there are some significant contrasts with  OH-IL, OH-MI, and WI-OH
#where for example OH-IL the difference in means is 42.89
#meaning one of the states higher/lower than normal sample mean
#there are some non-significant contrasts with MI-IL,WI-IL,WI-OH
#MI-IL is a contrast that with little difference becasue the means are close to each other
#keep in mind since the sample size is small for us to certify these results we increase funding and collect more data
#also, keep in mind the sample set failed the Equality of variances test earlier in our analysis





