#install packages
install.packages("dplyr")
install.packages("tidyverse")
install.packages("jmv")
install.packages("ggcorrplot")
install.packages("summarytools")
install.packages("gmodels")
install.packages("car")
install.packages('vGWAS', repos = 'http://r-forge.r-project.org')
install.packages("rcompanion")
install.packages("FSA")
install.packages("ResourceSelection")

# Load packages
library("dplyr")
library("tidyverse")
library("jmv")
library("ggcorrplot")
library("summarytools")
library("gmodels")
library("car")
library("vGWAS")
library("rcompanion")
library("FSA")
library("ResourceSelection")


# Import dataset 
ff <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv')
# ff - forestfires

# read header of ff and view as table in RStudio
head(ff)
View(ff)

#checking if there are any missing values
sum(is.na(ff))

# change the data type of the month and day columns using mutate()
ffnew <- ff%>% 
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")), 
  day = factor(day, levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun")))


# split the data into group by day and count the number of observations in each group (Fire+ Nofire).
fires_by_day<-ffnew %>% 
  group_by(day) %>%
  summarize(total_fires = n())

# split the data into group by month and count the number of observations in each group (Fire+ Nofire).
fires_by_month<-ffnew %>% 
  group_by(month) %>%
  summarize(total_fires = n())



# filter for FF yes (1)  
ff.Fire <- ffnew[ffnew$area > 0, ]
# split the data into group by day and count the number of observations in each group (Fire).
fires_by_day1<-ff.Fire %>% 
  group_by(day) %>%
  summarize(Fire = n())

# split the data into group by month and count the number of observations in each group (Fire).
fires_by_month1<-ff.Fire %>% 
  group_by(month) %>%
  summarize(Fire = n())

# filter for FF no (0)  
ff.NoFire <- ffnew[ffnew$area==0, ]

# split the data into group by day and count the number of observations in each group (NoFire).
fires_by_day0<-ff.NoFire %>% 
  group_by(day) %>%
  summarize(NoFire = n())

# split the data into group by month and count the number of observations in each group (NoFire).
fires_by_month0<-ff.NoFire %>% 
  group_by(month) %>%
  summarize(NoFire = n())

# add raw ("jan",0) and ("nov",0) to data fires_by_month1  and ("dec",0) to data fires_by_month0
jan0=c("jan",0)
dec0=c("dec",0)


fires_by_month1a <-   fires_by_month1 %>% 
  add_row(month="nov",Fire=0, .before = 10)

fires_by_month1 <- rbind(jan0, fires_by_month1a)  

fires_by_month0 <- rbind(fires_by_month0, dec0)  


# add column Fire to ff.DAY (Fire/NoFire per day)
ff.DAY=fires_by_day0
ff.DAY$Fire<- fires_by_day1$Fire

# add column Fire to ff.MONTH (Fire/NoFire per month)
ff.MONTH=fires_by_month0
ff.MONTH$Fire<- fires_by_month1$Fire

# How to Convert Character to Numeric in R
ff.MONTH$Fire <- as.numeric(ff.MONTH$Fire)
ff.MONTH$NoFire <- as.numeric(ff.MONTH$NoFire)



# Reshape ff.DAY data frame to long format for ggplot2 (DAY_FF)
DAY_FF <- data.frame(day = ff.DAY$day,                            # Reshape data frame
                      Number_of_Fires= c(ff.DAY$Fire,ff.DAY$NoFire),
                      group = c(rep("Fire", nrow(ff.DAY)),
                                rep("NoFire", nrow(ff.DAY))))

# Reshape ff.DAY data frame to long format for ggplot2 (MONTH_FF)
MONTH_FF <- data.frame(month = ff.MONTH$month,                            # Reshape data frame
                       Number_of_Fires= c(ff.MONTH$Fire,ff.MONTH$NoFire),
                       group = c(rep("Fire", nrow(ff.MONTH)),
                                 rep("NoFire", nrow(ff.MONTH))))

#Plot Fires per day and per month
# Assigning default and different outline colors to bar plot
ggplot(data=DAY_FF, aes(x=day, y=Number_of_Fires,color=group))+
  geom_bar(position="dodge", stat="identity",fill="white")+ theme_classic()

# Assigning default and different outline colors to bar plot
ggplot(data=MONTH_FF, aes(x=month, y=Number_of_Fires,color=group))+ 
  geom_bar(position="dodge", stat="identity",fill="white")+ theme_classic()
     



# find where is Fire  (Logical TRUE/FALSE)
areaBinary=ffnew[c(13)]>0

#Convert (Logical TRUE/FALSE) to String Fire/NoFire
areaBinary <- factor(areaBinary, labels = c('NoFire', 'Fire') )
#areaBinary

# add column area_binary to ffnew
ffnew$areaBinary <- areaBinary

ff1 <-ff
# set numeric as a factor
ff1$month <- as.numeric(as.factor(ff$month))
ff1$day <- as.numeric(as.factor(ff$day))
ff1$areaBinary <- as.numeric(as.factor(ffnew$areaBinary))

# Descriptive statistics using jmv package 
des.stat=descriptives(data = ff1, vars = c("X", "Y", "month", "day","FFMC", "DMC", "DC", "ISI", "RH", "temp", "rain", "area"),
  freq = TRUE,   hist = FALSE,  dens = FALSE,  bar = FALSE,  barCounts = FALSE,  box = FALSE,  sd = TRUE,  variance = TRUE,  range = TRUE,
  se = TRUE,   skew = TRUE,   kurt = TRUE,   pcEqGr = TRUE,   pcNEqGr = 4)

# write result of des.stat to file des.stat.txt in working directory
sink(file = "des.stat.txt")
des.stat
sink(file = NULL)
des.stat

#plot distribution of all dependent variables
par(mfrow=c(2,6),mar=c(4, 4.2, 2.5, 0.5))
for (variables in 1:(dim(ff1)[2]-2)){
  thisvar = ff1[,variables]
  d <- density(thisvar)
  plot(d, main = names(ff1[variables]),xlab="")
  polygon(d, col="yellow", border="red")}


# Correlation Matrix using jmv package 
corm=corrMatrix(ff1, vars = c("FFMC", "DMC", "DC", "ISI", "RH", "temp", "rain", "area"), 
          pearson = TRUE, spearman = FALSE, kendall = FALSE, sig = TRUE, flag = TRUE, ci = FALSE, ciWidth = 95, plots = FALSE,
           plotDens = FALSE, plotStats = FALSE, hypothesis = "corr")

# write result of corm to file corm.txt in working directory
sink(file = "corm.txt")
corm
sink(file = NULL)
#corm

#correlation matrix using cor fuction (simple - built in R)
cm <- cor(ffnew[, c(5,6,7,8,9,10,11,13)])

#correlation plot using cor corrplot package
ggcorrplot(cm, type="upper", lab=TRUE)



#variable manipulation 
# find weekend using factor 
isweekend <- factor(ifelse(ff$day %in% c("mon", "tue", "wed", "thu", "fri"), 0, 1))

#Convert (Logical TRUE/FALSE) to String yes/no
isweekend <- factor(isweekend, labels = c('no', 'yes') )
# isweekend

# add column isweekend to fffnew
ffnew$isweekend <- isweekend


# Correct abnornal distribution (FFMC -> FFMC3, area->logarea, ISI-> logISI, rain ->rain03 )  
# add variable FFMC3=FFMC^3, logarea=log(area+1), logISI=log(1+ffnew$ISI), rain=ffnew$rain^(1/3) 

# save logarea as value
logarea=log(1+ffnew$area)
ffnew$logarea <- logarea

# save logISI as value
logISI=log(1+ffnew$ISI)
ffnew$logISI <- logISI

# save rain03 as value
rain03=ffnew$rain^(1/3)
ffnew$rain03 <- rain03

# save FFMC3 as value
FFMC3=ffnew$FFMC^3
ffnew$FFMC3 <- FFMC3

#PLOT area
# Since the area is right-skew, we'll log(area+1) it to normalize it
par(mfrow=c(2,1),mar=c(5, 4.25, 5.5, 2))
d <- density(ffnew$area)
plot(d,main="area Density (original)",xlab="area index", col='red', lwd=3)
d <- density(ffnew$logarea)
plot(d,main="logarea Density",xlab="logarea index", col='orange', lwd=3)


# Since the FFMC is left-skew, we'll cube it to normalize it
par(mfrow=c(2,3),mar=c(5, 4.25, 5.5, 2))
d <- density(ffnew$FFMC)
plot(d,main="FFMC Density (original)",xlab="FFMC index", col='red', lwd=3)
d <- density(ffnew$ISI)
plot(d,main="ISI Density (original)",xlab="ISI index", col='red', lwd=3)
d <- density(ffnew$rain)
plot(d,main="rain Density (original)",xlab="rain index", col='red', lwd=3)
d <- density(ffnew$FFMC3)
plot(d,main="FFMC3 Density",xlab="FFMC3 index", col='orange', lwd=3)
d <- density(ffnew$logISI)
plot(d,main="logISI Density",xlab="logISI index", col='orange', lwd=3)
d <- density(ffnew$rain03)
plot(d,main="rograin Density",xlab="lograin index", col='orange', lwd=3)



##### PART 2
#### Calculate sum area per day, per month and per weekend #####

# Calculate sum area per day
DAY=ffnew %>% group_by(day) %>% summarise(sum(area))
colnames(DAY)[2] ="sumArea"


# Calculate sum area Greater Then 50ha per day
area50=(ffnew$area>50)*ffnew$area
ffnew$area50=area50
b=ffnew %>% group_by(day) %>% summarise(sum(area50))
#Rename Columns With List in R
colnames(b)[2] ="sumArea50"

#remove column
b <- b[,-1]

#add column sumArea50 to DAY
DAY['sumArea50'] <- b


# Calculate sum area Greater Then 100ha per day
c=(ffnew$area>100)*ffnew$area
ffnew$area100 <- c
d=ffnew %>% group_by(day) %>% summarise(sum(area100))
colnames(d)[2] ="sumArea100"

#remove column
d <- d[,-1]

#add column sumArea100 to DAY
DAY['sumArea100'] <- d


# Reshape DAY data frame to long format for ggplot2 (DAY_ggp)
DAY_ggp <- data.frame(day = DAY$day,                            # Reshape data frame
                       Area = c(DAY$sumArea,DAY$sumArea50, DAY$sumArea100),
                       group = c(rep("sumArea", nrow(DAY)),
                                 rep("sumArea50", nrow(DAY)),
                                 rep("sumArea100", nrow(DAY))))
#head(DAY_ggp)                                                # Head of reshaped data frame

# Assigning default and different outline colors to bar plot
ggplot(data=DAY_ggp, aes(x=day, y=Area,color=group))+
  geom_bar(position="dodge", stat="identity",fill="white")+ theme_classic()




# Calculate sum area per month
MONTH=ffnew %>% group_by(month) %>% summarise(sum(area))
colnames(MONTH)[2] ="sumArea"


# Calculate sum area Greater Then 50ha per month
#area50=(ffnew$area>50)*ffnew$area
#ffnew$area50=area50
bb=ffnew %>% group_by(month) %>% summarise(sum(area50))
colnames(bb)[2] ="sumArea50"

#remove column
bb <- bb[,-1]

#add column sumArea50 to DAY
MONTH['sumArea50'] <- bb

# Calculate sum area Greater Then 100ha per month
#c=(ffnew$area>100)*ffnew$area
#ffnew$area100 <- c
dd=ffnew %>% group_by(month) %>% summarise(sum(area100))
colnames(dd)[2] ="sumArea100"

#remove column
dd <- dd[,-1]

#add column sumArea100 to DAY
MONTH['sumArea100'] <- dd

# Reshape DAY data frame to long format for ggplot2 (DAY_ggp)
MONTH_ggp <- data.frame(month = MONTH$month,                            # Reshape data frame
                      Area = c(MONTH$sumArea,MONTH$sumArea50, MONTH$sumArea100),
                      group = c(rep("sumArea", nrow(MONTH)),
                                rep("sumArea50", nrow(MONTH)),
                                rep("sumArea100", nrow(MONTH))))
#head(MONTH_ggp)                                                # Head of reshaped data frame


# Assigning default and different outline colors to bar plot
ggplot(data=MONTH_ggp, aes(x=month, y=Area,color=group))+
  geom_bar(position="dodge", stat="identity",fill="white")+ theme_classic()



##### PART3

# Chi-Sqaure ver 
chi1_aB_d=ctable( x = ffnew$areaBinary, y = ffnew$day, chisq = TRUE, headings = TRUE)
sink(file = "chi1_aB_d.txt")
chi1_aB_d
sink(file = NULL)
chi1_aB_d

chi1_aB_m=ctable( x = ffnew$areaBinary, y = ffnew$month, chisq = TRUE, headings = TRUE) 
sink(file = "chi1_aB_m.txt")
chi1_aB_m
sink(file = NULL)
chi1_aB_m


chi1_aB_w=ctable( x = ffnew$areaBinary, y = ffnew$isweekend, chisq = TRUE, headings = TRUE) 
sink(file = "chi1_aB_w.txt")
chi1_aB_w
sink(file = NULL)
chi1_aB_w


#### Kruskal-Wallis test ####

# Check data distribution and normality assumptions using Shapiro-Wilk test
shapiro.test(ffnew$area)


# Check homogeneity of variances assumption:
# a) Levene’s test
# library("car") - Set "day" and "month" as a factor
leveneTest(area ~ day, data=ffnew)
leveneTest(area ~ month, data=ffnew)
leveneTest(area ~ isweekend, data=ffnew)

# b) Brown-Forsythe test
brown.forsythe.test(y = ffnew$area, group = ffnew$day)
brown.forsythe.test(y = ffnew$area, group = ffnew$month)
brown.forsythe.test(y = ffnew$area, group = ffnew$isweekend)

## Kruskal-Wallis test ##
kruskal.test(area ~ day, data = ffnew)
kruskal.test(area ~ month, data = ffnew)
kruskal.test(area ~ isweekend, data = ffnew)

#  Kruskal-Wallis - Effect size
#library("rcompanion")
epsilonSquared(x = ffnew$area, g = ffnew$day)
epsilonSquared(x = ffnew$area, g = ffnew$month)
epsilonSquared(x = ffnew$area, g = ffnew$isweekend)

#post-hoc test Kruskal-Wallis test (Dunn’s test)
dunnTest(area ~ month, data = ffnew, method = "bh")


# filter for FF yes (1)  
ff.yes <- ffnew[ffnew$area > 0, ]

#### Kruskal-Wallis test ####
## filter for FF yes (1)  

# Check data distribution and normality assumptions using Shapiro-Wilk test
shapiro.test(ff.yes$area)


# Check homogeneity of variances assumption:
# a) Levene’s test
# library("car") - Set "day" and "month" as a factor
leveneTest(area ~ day, data=ff.yes)
leveneTest(area ~ month, data=ff.yes)
leveneTest(area ~ isweekend, data=ff.yes)

# b) Brown-Forsythe test
brown.forsythe.test(y = ff.yes$area, group = ff.yes$day)
brown.forsythe.test(y = ff.yes$area, group = ff.yes$month)
brown.forsythe.test(y = ff.yes$area, group = ff.yes$isweekend)

## Kruskal-Wallis test ##
kruskal.test(area ~ day, data = ff.yes)
kruskal.test(area ~ month, data = ff.yes)
kruskal.test(area ~ isweekend, data = ff.yes)

#  Kruskal-Wallis - Effect size
#library("rcompanion")
epsilonSquared(x = ff.yes$area, g = ff.yes$day)
epsilonSquared(x = ff.yes$area, g = ff.yes$month)
epsilonSquared(x = ff.yes$area, g = ff.yes$isweekend)

#post-hoc test Kruskal-Wallis test (Dunn’s test)
#dunnTest(area ~ isweekend, data = ff.yes, method = "bh")


##### PART 4


### Linear regresion  ####

## all data GLM ##
LR1 <- glm(logarea~ X + Y + DC + logISI + temp + RH + wind + rain03 + 
                  month + DMC +FFMC3 + day, 
                data = ffnew, family=gaussian)

summary(LR1)

par(mfrow = c(2,2))
plot(LR1)

# calculate Hosmer and Lemeshow goodness of fit (GOF) test
hoslem.test(ffnew$logarea, fitted(LR1))

#calculate Nagelkerke pseudo R-squared = 
nagelkerke(LR1)
accuracy(list(LR1))

vif(LR1)

step(LR1, test="LRT")


## FF when fire area>0 for GLM ##

LR2 <- glm(logarea~ X + Y + DC + logISI + temp + RH + wind + rain03 + 
                 month + DMC +FFMC3 + day, 
               data = ff.yes, family=gaussian)

summary(LR2)

par(mfrow = c(2,2))
plot(LR2)

# calculate Hosmer and Lemeshow goodness of fit (GOF) test
hoslem.test(ff.yes$logarea, fitted(LR2))


#calculate Nagelkerke pseudo R-squared = 
nagelkerke(LR2)
accuracy(list(LR2))

car::vif(LR2)
step(LR2, test="LRT")

