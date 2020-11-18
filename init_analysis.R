# read libraries
source("libs.R")
source("funs.R") # custom functions for analysis

# read data
d<-data_read()

# take longitudinal cases (i.e more than one response)
d<- data_take_longitudinal_cases(d)
show_unique_id(d)
e1)

# label changes
dom$Your_Future_Security <- dom$SWB.SatPWI03
dom$MostNZPoliciesServeGrGood<-dom$SysJust04
dom$Religious <- factor(dom$Religious, labels = c("Not_Religious","Religious"))

table1::label(dom$Env.ClimateChgReal) <- "Climate Change Reality"
table1::label(dom$Env.ClimateChgCause) <- "Humans Cause Climate Change"
table1::label(dom$EnvEfficacy) <- "Human Efficacy"
table1::label(dom$Env.SacMade) <- "Sacrificed for Environment"
table1::label(dom$Env.SacWilling) <- "Willing to Sacrifice Environment"
table1::label(dom$Env.MotorwaySpend) <- "Gov Motorway Spend"
table1::label(dom$Env.PubTransSubs) <- "Gov Subsidy Public Transport"
table1::label(dom$Env.NATIVE.SPECIES) <- "Protect NZ Species"
table1::label(dom$Env.SatNZEnvironment) <- "Sat Quality NZ Environment"
table1::label(dom$Env.ClimateChgConcern) <- "Deeply Concerned About Climate"
dom$Your_Future_SecurityS = scale(dom$Your_Future_Security)
dom$K6S = scale(dom$KESSLER6)

table1(~Env.ClimateChgReal|Wave, data = dom, transpose = T)  # ALL
table1(~Env.ClimateChgCause|Wave, data = dom, transpose = T)  # ALL
table1(~EnvEfficacy|Wave, data = dom, transpose = T)  # No 15 or 18
table1(~Env.SacMade|Wave, data = dom, transpose = T)  # No 15 No 16 No 18
table1(~Env.SatNZEnvironment|Wave, data = dom, transpose = T) # all   # No 15 No 16 No 18
table1(~Your_Future_Security|Wave, data = dom, transpose = T) # all  All
table1(~KESSLER6|Wave, data = dom, transpose = T) # all  09
table1(~Env.ClimateChgConcern|Wave, data = dom, transpose = T) # Missing first 4 years
dom$Env.SatNZEnvironmentS<-scale(dom$Env.SatNZEnvironment)
dom$Your_Future_SecurityS<-scale(dom$Your_Future_Security)
dom$Env.ClimateChgConcernS<-scale(dom$Env.ClimateChgConcern)
dom$EnvEfficacyS<-scale(dom$EnvEfficacy)
dom$RelidS<-scale(dom$Relid)



table1(~Env.ClimateChgConcern | factor(yearsM), data = dom, transpose =T)
domM <- dom %>% filter(yearsM>=0)
domM$RelidS<-scale(domM$Relid)
dom$Pol.OrientS<-scale(dom$Pol.Orient)
domM$Pol.OrientS<-scale(domM$Pol.Orient)
domM$Env.ClimateChgConcernS<-scale(domM$Env.ClimateChgConcern)


# initial analysis
mt<-lmer(Env.ClimateChgConcern ~ yearsM * (Your_Future_SecurityS + K6S)+ (1|Id), data = domM)
mtt<-lmer(Env.ClimateChgConcern ~ yearsM * (RelidS)+ (1|Id), data = domM)
mttp<-lmer(Env.ClimateChgConcern ~ yearsM * (Pol.OrientS)+ (1|Id), data = domM)

na <- lmer(Env.PubTransSubs ~ yearsM * (Env.ClimateChgConcernS)+ (1|Id), data = domM)
nb <- lmer(Env.MotorwaySpend ~ yearsM * (Env.ClimateChgConcernS)+ (1|Id), data = domM)
nc <- lmer(Env.NATIVE.SPECIES ~ yearsM * (Env.ClimateChgConcernS)+ (1|Id), data = domM)

na1 <- lmer(Env.PubTransSubs ~ years * (Env.SatNZEnvironmentS)+ (1|Id), data = domM)
nr <- lmer(Env.MotorwaySpend ~ years * (Env.SatNZEnvironmentS)+ (1|Id), data = dom)
np <- lmer(Env.NATIVE.SPECIES ~ years * (Env.SatNZEnvironmentS)+ (1|Id), data = dom)

check_collinearity(na) #  correlations
check_collinearity(nb) #  correlations
check_collinearity(nc) #  correlations
check_collinearity(nr) #  correlations
check_collinearity(np) #  correlations


plot(ggpredict(na, terms =c("yearsM","Env.ClimateChgConcernS [minmax]")))
plot(ggpredict(nb, terms =c("yearsM","Env.ClimateChgConcernS [minmax]")))
plot(ggpredict(nc, terms =c("yearsM","Env.ClimateChgConcernS [minmax]")))


plot(ggpredict(na1, terms =c("years","Env.SatNZEnvironmentS [minmax]")))
plot(ggpredict(nr, terms =c("years","Env.SatNZEnvironmentS [minmax]")))
plot(ggpredict(np, terms =c("years","Env.SatNZEnvironmentS [minmax]")))


str(dom$Wave)
ml<-lmer(Env.SacMade ~ years * (Env.SatNZEnvironmentS*RelixdS)+ (Id|Wave), data = dom)
mlPP<-lmer(Env.SacMade ~ years * (Env.SatNZEnvironmentS*Pol.OrientS)+ (1|Id), data = dom)
mlk<-lmer(Env.SacMade ~ years * (Your_Future_SecurityS*RelidS)+ (1|Id), data = dom)
mlkp<-lmer(Env.SacMade ~ years * (Your_Future_SecurityS*Pol.OrientS)+ (1|Id), data = dom)

mll<-lmer(Env.SacMade ~ years * (Env.ClimateChgConcernS*RelidS)+ (1|Id), data = dom)
mlp<-lmer(Env.SacMade ~ years * (EnvEfficacyS)+ (1|Id), data = dom)
mld<-lmer(EnvEfficacy ~ years * (Env.ClimateChgConcernS)+ (1|Id), data = dom)
mld<-lmer(EnvEfficacy ~ years * (Env.ClimateChgConcernS)+ (1|Id), data = dom)
mls<-lmer(EnvEfficacy ~ years * (Env.SatNZEnvironmentS)+ (1|Id), data = dom)
mla<-lmer(Env.SacMade ~ years * (Your_Future_SecurityS)+ (1|Id), data = dom)


library(performance)
check_collinearity(mt) #  correlations
check_collinearity(mtt) #  correlations
check_collinearity(ml)  #  correlations
check_collinearity(mlk) #  correlations
check_collinearity(mll) #  correlations
check_collinearity(mlp) #  correlations
check_collinearity(mld) #  correlations



mk<-lmer(Env.ClimateChgConcern ~ yearsM *K6S + (1|Id), data = domM)
check_collinearity(mk) # high correlations

plot(ggpredict(mt, terms =c("yearsM","Your_Future_SecurityS","K6S")))
plot(ggpredict(mtt, terms =c("yearsM","RelidS")))
plot(ggpredict(mttp, terms =c("yearsM","Pol.OrientS")))

plot(ggpredict(mk, terms =c("yearsM","K6S")))


plot(ggpredict(ml, terms =c("years","Env.SatNZEnvironmentS [minmax]","RelidS"))) # USE
plot(ggpredict(mlPP, terms =c("years","Env.SatNZEnvironmentS"))) # USE
plot(ggpredict(mlPP, terms =c("years","Env.SatNZEnvironmentS [minmax]"))) # USE

plot(ggpredict(mlk, terms =c("years","Your_Future_SecurityS [minmax]","RelidS")))  # NOT FUTURE SECURITY BUT SATISFACTION WITH ENVIRONMENT
plot(ggpredict(mlkp, terms =c("years","Your_Future_SecurityS [minmax]","Pol.OrientS")))  # NOT FUTURE SECURITY BUT SATISFACTION WITH ENVIRONMENT  ## WOW

plot(ggpredict(mll, terms =c("years","Env.ClimateChgConcernS","RelidS")))
plot(ggpredict(mlp, terms =c("years","EnvEfficacyS[minmax]"))) # linear on religion
plot(ggpredict(mld, terms =c("years","Env.ClimateChgConcernS[minmax]"))) # linear on religion USE
plot(ggpredict(mld, terms =c("years","Env.ClimateChgConcernS[minmax]"))) # linear on religion # USE
plot(ggpredict(mls, terms =c("years","Env.SatNZEnvironmentS[minmax]"))) # USE ** CROSS'
#then
#
plot(ggpredict(mla, terms =c("years","Your_Future_SecurityS[minmax]"))) # USE ** CROSS


### THis is key

table1::table1(~Env.ClimateChgReal + Env.ClimateChgCause + EnvEfficacy + Env.SacMade + Env.SacWilling + Env.SatNZEnvironment + Env.ClimateChgConcern| Wave, data = dom, overall =T, transpose=T)

table1::table1(~ Env.SacWilling + Env.SacMade| Wave*Relid, data = dom, overall =F)

table1::table1(~ Env.SacWilling + Env.SacMade |Wave , data = dom, overall =F,transpose = T)

table1::table1(~Age + Edu + Employed  + Male + Pol.Orient + Relid + Urban| Wave, data = dom, overall =F, droplevels = FALSE)##topclass = "Rtable1-zebra"

table1::table1(~Age + Edu + Employed  + Male + Pol.Orient + Relid + Urban| Wave * Religious, data = dom, overall =F, droplevels = FALSE)##topclass = "Rtable1-zebra"

ldf.5$KESSLER6

# ### TRY DIFF
BGW<-  ldf.5 %>%
  filter(YearMeasured ==1)%>%
  dplyr::select(Wave,Env.ClimateChgCause,Env.ClimateChgReal,EnvEfficacy,Env.SacMade,Env.SacWilling,Id,Urban,Pol.Orient,Employed,Age,Edu,KESSLER6)%>%
  group_by(Id) %>% filter(n() > 4)%>%
  filter(n() !=0)%>%
  ungroup(Id)

# kessler 6
KESSLER6<-BGW%>%
  dplyr::select(KESSLER6,Wave,Id)%>%
  dplyr::filter(Wave !=2009) %>% 
#  dplyr::mutate(KESSLER6 =KESSLER6*6) %>% 
  dplyr::mutate(KESSLER6_CAT=cut(KESSLER6, breaks=c(-Inf, 5, 13, Inf), labels=c("low_anxiety","middle_anxiety","high_anxiety")))%>%
  dplyr::mutate(KESSLER6_Bin=cut(KESSLER6, breaks=c(-Inf, 13, Inf), labels=c("low_anxiety","high_anxiety")))

K6A<- KESSLER6%>%
  dplyr::select(Wave,KESSLER6,Id) %>% 
  spread(Wave,KESSLER6)

K63<- KESSLER6%>%
  dplyr::select(Wave,KESSLER6_CAT,Id) %>% 
  spread(Wave,KESSLER6_CAT)
head(K63)
K62<- KESSLER6%>%
  dplyr::select(Wave,KESSLER6_Bin,Id) %>% 
  spread(Wave,KESSLER6_Bin)
head(K62)
#head(KESSLER6)
K6Ac<-K6A[complete.cases(K6A), ]
K62c<-K62[complete.cases(K62), ]
K63c<-K63[complete.cases(K63), ]
head(K62c)


x_var_list<-names(K6Ac[,2:10])
x_var_list
library(lcsm)
plot_trajectories(data = K6Ac,
                  id_var = "Id",
                  var_list = x_var_list,
                  xlab = "Time", ylab = "KESSLER6",
                  connect_missing = F,
                  random_sample_frac = .15,
                  title_n = TRUE) + facet_wrap(~Id)


x_var_list<-names(K62c[,2:10])
plot_trajectories(data = K62c,
                  id_var = "Id",
                  var_list = x_var_list,
                  xlab = "Time", ylab = "KESSLER6_Bin",
                  connect_missing = F,
                  random_sample_frac = .15,
                  title_n = TRUE) + facet_wrap(~Id)

plot_trajectories(data = K63c,
                  id_var = "Id",
                  var_list = x_var_list,
                  xlab = "Time", ylab = "KESSLER6_CAT",
                  connect_missing = F,
                  random_sample_frac = .15,
                  title_n = TRUE) + facet_wrap(~Id)

plot_trajectories(data = K63,
                  id_var = "Id",
                  var_list = x_var_list,
                  xlab = "Time", ylab = "KESSLER6",
                  connect_missing = F,
                  random_sample_frac = .02,
                  title_n = TRUE) + facet_wrap(~Id)


plot_trajectories(data = K62,
                  id_var = "Id",
                  var_list = x_var_list,
                  xlab = "Time", ylab = "KESSLER6",
                  connect_missing = F,
                  random_sample_frac = .02,
                  title_n = TRUE) + facet_wrap(~Id)

## pol orient
## 
## 
Pol.Orient<-BGW%>%
  dplyr::select(Pol.Orient,Wave,Id)%>%
  spread(Wave,Pol.Orient)
head(Pol.Orient)
Pol.Orient<-Pol.Orient[complete.cases(Pol.Orient), ]
x_var_list<-names(Pol.Orient[,2:11])
x_var_list
library(lcsm)
plot_trajectories(data = Pol.Orient,
                  id_var = "Id",
                  var_list = x_var_list,
                  xlab = "Time", ylab = "Pol.Orient",
                  connect_missing = F,
                  random_sample_frac = .1,
                  title_n = TRUE) + facet_wrap(~Id)

#### AGE
Age<-BGW%>%
  dplyr::select(Age,Wave,Id)%>%
  spread(Wave,Age)
head(Age)
Age<-Age[complete.cases(Age), ]
x_var_list<-names(Age[,2:11])
x_var_list
library(lcsm)

## REALITY CHECK
plot_trajectories(data = Age,
                  id_var = "Id",
                  var_list = x_var_list,
                  xlab = "Time", ylab = "Urban",
                  connect_missing = F,
                  random_sample_frac = .1,
                  title_n = TRUE) + facet_wrap(~Id)

#### Edu
Edu<-BGW%>%
  dplyr::select(Edu,Wave,Id)%>%
  dplyr::filter(Wave !=2010 & Wave!=2011) %>% 
  dplyr::mutate(Education = as.numeric(Edu))%>%
  dplyr::select(-Edu) %>% 
  spread(Wave,Education)

#Edu<-Edu[complete.cases(Edu), ]
head(Edu)
x_var_list<-names(Edu[,2:8])
x_var_list
library(lcsm)

## REALITY CHECK
plot_trajectories(data = Edu,
                  id_var = "Id",
                  var_list = x_var_list,
                  xlab = "Time", ylab = "Education",
                  connect_missing = F,
                  random_sample_frac = .05,
                  title_n = TRUE) + facet_wrap(~Id)



####
#### urban
Urban<-BGW%>%
  dplyr::select(Urban,Wave,Id)%>%
  spread(Wave,Urban)
head(Urban)
Urban<-Urban[complete.cases(Urban), ]
x_var_list<-names(Urban[,2:11])
x_var_list
library(lcsm)
plot_trajectories(data = Urban,
                  id_var = "Id",
                  var_list = x_var_list,
                  xlab = "Time", ylab = "Urban",
                  connect_missing = F,
                  random_sample_frac = .1,
                  title_n = TRUE) + facet_wrap(~Id)


###
ClimateChgReal<-BGW%>%
  dplyr::select(Env.ClimateChgReal,Wave,Id)%>%
  spread(Wave,Env.ClimateChgReal)
head(ClimateChgReal)
#ClimateChgReal<-ClimateChgReal[complete.cases(ClimateChgReal), ]
x_var_list<-names(ClimateChgReal[,2:11])
x_var_list
library(lcsm)
plot_trajectories(data = ClimateChgReal,
                  id_var = "Id",
                  var_list = x_var_list,
                  xlab = "Time", ylab = "ClimateChgReal",
                  connect_missing = F,
                  random_sample_frac = .015,
                  title_n = TRUE) + facet_wrap(~Id)



Env.ClimateChgCause<-BGW%>%
  dplyr::select(Env.ClimateChgCause,Wave,Id)%>%
  spread(Wave,Env.ClimateChgCause)
head(Env.ClimateChgCause)
#Env.ClimateChgCause<-Env.ClimateChgCause[complete.cases(Env.ClimateChgCause), ]
x_var_list<-names(Env.ClimateChgCause[,2:11])
x_var_list
library(lcsm)
plot_trajectories(data = Env.ClimateChgCause,
                  id_var = "Id",
                  var_list = x_var_list,
                  xlab = "Time", ylab = "Env.ClimateChgCause",
                  connect_missing = F,
                  random_sample_frac = .015,
                  title_n = TRUE) + facet_wrap(~Id)


#### 
EnvEfficacy<-BGW%>%
  dplyr::select(EnvEfficacy,Wave,Id)%>%
  dplyr::filter(Wave !=2018 &  Wave != 2015)%>%
  spread(Wave,EnvEfficacy)

# 
# 
head(EnvEfficacy)
#EnvEfficacy<-EnvEfficacy[complete.cases(EnvEfficacy), ]
EnvEfficacy

x_var_list<-names(EnvEfficacy[,2:9])
x_var_list
library(lcsm)
plot_trajectories(data = EnvEfficacy,
                  id_var = "Id",
                  var_list = x_var_list,
                  xlab = "Time", ylab = "EnvEfficacy",
                  connect_missing = F,
                  random_sample_frac = .01,
                  title_n = TRUE) + facet_wrap(~Id)

### 
Env.SacMade<-BGW%>%
  dplyr::select(Env.SacMade,Wave,Id)%>%
  dplyr::filter(Wave !=2018 & Wave != 2016 & Wave != 2015)%>%
  spread(Wave,Env.SacMade)

# 
# 
head(Env.SacMade)
#Env.SacMade<-Env.SacMade[complete.cases(Env.SacMade), ]
Env.SacMade

x_var_list<-names(Env.SacMade[,2:8])
x_var_list
library(lcsm)
plot_trajectories(data = Env.SacMade,
                  id_var = "Id",
                  var_list = x_var_list,
                  xlab = "Time", ylab = "Env.SacMade",
                  connect_missing = F,
                  random_sample_frac = .01,
                  title_n = TRUE) + facet_wrap(~Id)

Env.SacWilling

### 
Env.SacWilling<-BGW%>%
  dplyr::select(Env.SacMade,Wave,Id)%>%
  dplyr::filter(Wave !=2018 & Wave != 2016 & Wave != 2015)%>%
  spread(Wave,Env.SacMade)
Env.SacWilling
# 
# 
head(Env.SacWilling)
#Env.SacMade<-Env.SacMade[complete.cases(Env.SacMade), ]
Env.SacWilling

x_var_list<-names(Env.SacWilling[,2:8])
x_var_list
library(lcsm)
plot_trajectories(data = Env.SacWilling,
                  id_var = "Id",
                  var_list = x_var_list,
                  xlab = "Time", ylab = "Env.SacWilling",
                  connect_missing = F,
                  random_sample_frac = .01,
                  title_n = TRUE) + facet_wrap(~Id)




### DOM 
dom$Male <- factor(ldf.a$Male, labels = c("Female","Male"))
table1::label(dom$Env.ClimateChgReal) <- "Climate Change Reality"
table1::label(dom$Env.ClimateChgCause) <- "Humans Cause Climate Change"
table1::label(dom$EnvEfficacy) <- "Human Efficacy"
table1::label(dom$Env.SacMade) <- "Sacrificed for Environment"
table1::label(dom$Env.SacWilling) <- "Willing to Sacrifice Environment"
dom$Religious <- factor(dom$Male, labels = c("Not Religious","Religious"))
library("tidyLPA")
library("tidyverse")
# library("dplyr")
# 
# time9dat<-ldf.5%>%
#   dplyr::filter(Wave==2017 & YearMeasured==1)
# 
# summary(time9dat$Env.SacMade)
# nrow(time9dat)
# head(time9dat$Env.SacMade)
# 
# library(dplyr)
# time9dat$ENVEFFICACY
# dom9 <-time9dat%>%
#   dplyr::select(Env.ClimateChgCause,Env.ClimateChgReal,EnvEfficacy,Env.SacWilling,Env.SacMade)%>%
#   tidyLPA::single_imputation() %>%
#   tidyLPA::estimate_profiles(2:4)
# 
# tidyLPA::get_fit(dom9) # 2 class is best
# 
# dom9 <-time9dat%>%
#   dplyr::select(Env.ClimateChgCause,Env.ClimateChgReal,EnvEfficacy,Env.SacWilling,Env.SacMade)%>%
#   tidyLPA::single_imputation() %>%
#   tidyLPA::estimate_profiles(2)
# 

# Graph
tidyLPA::plot_profiles(dom9, add_line = T)

shortR<- time9dat
gotn<- tidyLPA::get_data(dom9)
shortR$Class<-gotn$Class

shortR$Class<-gotn$Class
table(shortR$Class)
round(mean(shortR$Class == 1),3) #  0.73
round(mean(shortR$Class == 2),3) #  0.22


library(Amelia)
head(ldf.5)

library(prophet)
library(readr)
library(lubridate)
today()
dom%>%
  tally(n = TSCORE)


df <- dom %>% 
  dplyr::select(TSCORE,Id,Wave,years,Relid,Religious)%>%
  dplyr::mutate(ds = make_date(year = 2009, month = 6, day = 30)+ TSCORE)
ggplot(df) + aes(x = format(ds, "%Y")) + 
  geom_bar(fill = "purple")
ggplot(df) + aes(x = format(ds, "%Y-%m")) + 
  geom_bar(fill = "purple")
head(df)
ggplot(df) + aes(x = format(ds, "%Y-%m"),group=Wave) + facet_grid(~ "%Y")+
  geom_bar(fill=1)

library(ggpmisc)
## USE
df %>% 
  count(week = floor_date(ds, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_col(colour="purple")  + scale_x_date(date_labels = "%Y") +ylab("count") + xlab("year") + ylab("Count of Responses")+ ggtitle("New Zealand Attitudes and Values Study Responses: 2009-2018") + theme_blank()




#NZAVSHISTOGRAM

## THIS IS GOOD SEE DOCUMENTATION 
## 
library(timelineS)
Event<-c("Christchurch EarthQuake","JB Birthday") 
Event_Dates <- as.Date(c("2010-02-22","2016-09-16"))
m<-tibble(Event,Event_Dates)
timelineS(m, main = "NZAVS Dates") 


life_country

rhead(df)
str(df)
ggplot(data = df, aes(x = ds, y = y)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Total daily precipitation in Boulder, Colorado",
       subtitle = "Fall 2013",
       x = "Date", y = "Daily Precipitation (Inches)")

# not run
# m<-prophet(df)
# future <- make_future_dataframe(m, periods = 12)
# forecast <- predict(m, future)
# plot(m, forecast)
# 
# 
# 

table(dom$KESSLER6)
ldf.5$EthnicCats
amdat.1  <-dom%>%
  filter(YearMeasured ==1)%>%
  group_by(Id) %>% filter(n() > 1)%>%
  filter(n() !=0)%>%
  ungroup(Id)%>%
  dplyr::select(Env.ClimateChgCause,Env.ClimateChgReal,EnvEfficacy,Env.SacWilling,Env.SacMade,Age,
                Id,years,Wave,Urban,Pol.Orient,Male,Relid,NZdep,Edu,Employed,EthnicCats,GenCohort,Religious_Group,Believe.Spirit,Believe.God,Religion.Church,Env.SatNZEnvironment,Env.ClimateChgConcern,NZdep,KESSLER6,Your_Future_Security,Env.NATIVE.SPECIES,Env.PubTransSubs,Env.MotorwaySpend)%>%
  mutate(Wave =factor(Wave),
         Male = factor(Male),
         #  Religious = factor(Religious),
         Urban =  factor(Urban),
         EthnicCats = factor(EthnicCats),
         Employed = factor(Employed),
         Edu=as.numeric(Edu),
         Relid =as.integer(Relid),
         Pol.Orient=as.integer(Pol.Orient),
         Edu=as.integer(Edu),
         KESSLER6 = as.integer(KESSLER6))

table(amdat.1$KESSLER6)
amdat.1<-data.frame(amdat.1)
table(tally(group_by(amdat.1, Id), sort = TRUE))
count(tally(group_by(amdat.1, Id), sort = TRUE, name="waves"),waves)

library(furniture)
furniture::table1(amdat.1,
       output ="latex2",
       Age,  EthnicCats, Edu, Male, NZdep,Pol.Orient, Relid, Urban,
       splitby=~Wave,
       caption ="Demographic Indicators",
       na.rm = FALSE)


library(Amelia)
set.seed(1234)
am.out.dom <- amelia(amdat.1, 
                     m=10,  
                     idvars = c("Wave","GenCohort","Religious_Group"),
                     ords = c("Male",
                              "KESSLER6",
                              "Urban",
                              "Employed",
                              "Believe.God",
                              "Believe.Spirit",
                              "EthnicCats",
                              "Relid",
                              "Pol.Orient",
                              "Edu"),
                     logs = "Religion.Church",
                     ts = "years",
                     polytime = 2,
                 cs = "Id")
#saveRDS(am.out.dom,"am.out.dom")
#am.out.dom<-readRDS("am.out.dom")
head(am.out.dom)
tscsPlot(am.out.dom,ts = "years", cs="Id", var = "Env.SacWilling")

compare.density(am.out.dom, var = "Env.ClimateChgConcern")
compare.density(am.out.dom, var = "Env.ClimateChgCause")

compare.density(am.out.dom, var = "Env.PubTransSubs")
compare.density(am.out.dom, var = "Env.MotorwaySpend")
compare.density(am.out.dom, var = "Env.PubTransSubs")


#am.out<-am.out.dom
domALL <- transform.amelia(am.out.dom,
                           Id = factor(Id),
                           Age.C.decade = scale(Age, center=TRUE, scale =FALSE)/10,
                           Male = factor(Male),
                           Pol.Orient.C = scale(Pol.Orient, center =T, scale=F),
                           Relid.C= scale(Relid,  center = T, scale= F),
                           Pol.Orient.S = scale(Pol.Orient, center =T, scale=T),
                           Relid.S= scale(Relid,  center = T, scale= T),
                           Edu.S = scale(Edu),
                           years.f = factor(years),
                           NZdepS = scale(NZdep),
                           KESSLER6S = scale(KESSLER6),
                           Your_Future_SecurityS = scale(Your_Future_Security),
                           Env.SatNZEnvironmentS = scale(Env.SatNZEnvironment),
                           EnvEfficacyS = scale(EnvEfficacy),
                           Env.ClimateChgConcernS = scale(Env.ClimateChgConcern))
#saveRDS(domALL,"domALL")
domALL<- readRDS("domALL")
library(lme4)
#library(merTools)
library(sjPlot)

## GET LATEX EQUATION FOR REPORTING

forlatex <-lmer(Env.ClimateChgConcern ~  years * (Relid.S + Pol.Orient.S + Edu.S)  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  + (1|Id), data=domALL$imputations$imp1)

library(equatiomatic)
eq<-extract_eq(forlatex,wrap =TRUE)
eq
# Models
m <- 10
models <- NULL
for(i in 1:m) {
  models[[i]] <- lmer(Env.ClimateChgConcern ~  years * (Relid.S + Pol.Orient.S + Edu.S)  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  + (1|Id), data=domALL$imputations[[i]])
}

#devtools::install_github("easystats/parameters") Get development version
library(parameters)
library(texreg)
mdomCONCERN <- lapply(models, model_parameters)
parameters::pool_parameters(mdomCONCERN)%>%tibble()%>%
  dplyr::select(Parameter, Coefficient, SE, CI_low, CI_high)%>%
  mutate_if(is.numeric, ~round(., 3))%>%
 # mutate_all(funs(str_replace(., "b_", "")))%>%
  # mutate_all(funs(str_replace(., "_", " X ")))%>%
  ggpubr::ggtexttable(rows = NULL,
                      theme = ttheme("mOrange")) 


mCONCERN<-pool_parameters(mdomCONCERN)
mCONCERN
# COEFF PLOT
plot(mCONCERN) + ggtitle("Coefficient Plot: Deeply Concerned About Climate") # pick your  own title

## EXPECTATION PLOTS
## 
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("years [4:9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = TRUE, color="viridis") + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
CONCERN_TIME <-plots[[10]] + theme_clean() 
CONCERN_TIME # 12 X 800
# 
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("EthnicCats","years [4,6.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
CONCERN_EthnicCats_T <-plots[[10]]   + theme_clean() 
CONCERN_EthnicCats_T # 12 X 800


## edu
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("Edu.S","years [4,6.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
CONCERN_Edu.S <-plots[[10]]   + theme_clean() 
CONCERN_Edu.S # 12 X 800

# political orientation

m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("Pol.Orient.S","years [4,6.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = TRUE, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
CONCERN_Pol.Orient.S <-plots[[10]]   + theme_clean() 
CONCERN_Pol.Orient.S # 12 X 800

# RELIGION
## EXPECTATION PLOTS


# RELIGION
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("Relid.S [minmax]","years [4,6.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
CONCERN_RELIDS_T <-plots[[10]] + theme_clean() 
CONCERN_RELIDS_T # 12 X 800
# 
# CONCERN_T_RELIDS + CONCERN_RELIDS_T + plot_annotation(title="Religious New Zealanders Less Sensitive to Climate Concern: 2013-2018")
# 
# CONCERN_T_RELIDS_CONCERN_RELIDS_T # 2000 x1000

## 
## 
## 


### CLIMATE CHANGE IS REAL 

# Models
m <- 10
models <- NULL
for(i in 1:m) {
  models[[i]] <- lmer(Env.ClimateChgReal~  years * (Relid.S + Pol.Orient.S + Edu.S)  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  + (1|Id), data=domALL$imputations[[i]])
}

#devtools::install_github("easystats/parameters") Get development version
library(parameters)
library(texreg)
mdomREAL <- lapply(models, model_parameters)
parameters::pool_parameters(mdomREAL)%>%tibble()%>%
  dplyr::select(Parameter, Coefficient, SE, CI_low, CI_high)%>%
  mutate_if(is.numeric, ~round(., 3))%>%
  # mutate_all(funs(str_replace(., "b_", "")))%>%
  # mutate_all(funs(str_replace(., "_", " X ")))%>%
  ggpubr::ggtexttable(rows = NULL,
                      theme = ttheme("mOrange")) 


mREAL<-pool_parameters(mdomREAL)
mREAL
# COEFF PLOT
plot(mREAL) + ggtitle("Coefficient Plot: Climate Change is Real") # pick your  own title

## EXPECTATION PLOTS
## 
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("years [0:9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = TRUE, color="viridis")  + scale_y_continuous(limits=c(5,7))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
REAL_TIME <-plots[[10]]  + theme_clean() 
REAL_TIME # 12 X 800
# 
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("EthnicCats","years [0,4.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(5,7))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
REAL_EthnicCats_T <-plots[[10]]   + theme_clean() 
REAL_EthnicCats_T # 12 X 800


## edu
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("Edu.S","years [0,4.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
REAL_Edu.S <-plots[[10]]   + theme_clean() 
REAL_Edu.S # 12 X 800

# political orientation

m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("Pol.Orient.S","years [0,4.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = TRUE, color="viridis")  + scale_y_continuous(limits=c(5,7))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
REAL_Pol.Orient.S <-plots[[10]]  + theme_clean() 
REAL_Pol.Orient.S # 12 X 800

# RELIGION
## EXPECTATION PLOTS


# RELIGION
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("Relid.S [minmax]","years [0,4.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(5,7))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
REAL_RELIDS_T <-plots[[10]] + theme_clean() 
REAL_RELIDS_T # 12 X 800
# 
# CONCERN_T_RELIDS + CONCERN_RELIDS_T + plot_annotation(title="Religious New Zealanders Less Sensitive to Climate Concern: 2013-2018")
# 
# CONCERN_T_RELIDS_CONCERN_RELIDS_T # 2000 x1000

####
####
####
####
####

#### CLIMATE IS HUMAN CAUSED
#### 
#### 
#### 


# Models
m <- 10
models <- NULL
for(i in 1:m) {
  models[[i]] <- lmer(Env.ClimateChgCause ~  years * (Relid.S + Pol.Orient.S + Edu.S)  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  + (1|Id), data=domALL$imputations[[i]])
}

#devtools::install_github("easystats/parameters") Get development version
library(parameters)
library(texreg)
mdomHUMANCAUSED <- lapply(models, model_parameters)
parameters::pool_parameters(mdomHUMANCAUSED )%>%tibble()%>%
  dplyr::select(Parameter, Coefficient, SE, CI_low, CI_high)%>%
  mutate_if(is.numeric, ~round(., 3))%>%
  # mutate_all(funs(str_replace(., "b_", "")))%>%
  # mutate_all(funs(str_replace(., "_", " X ")))%>%
  ggpubr::ggtexttable(rows = NULL,
                      theme = ttheme("mOrange")) 


mHUMANCAUSED <-pool_parameters(mdomHUMANCAUSED )
mHUMANCAUSED 
# COEFF PLOT
plot(mHUMANCAUSED) + theme_clean() # pick your  own title

## EXPECTATION PLOTS
## 
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("years [4:9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = TRUE, color="viridis") + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
HUMANCAUSED_TIME <-plots[[10]] + theme_clean() 
HUMANCAUSED_TIME # 12 X 800
# 
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("EthnicCats","years [0,4.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 

library(ggthemes)
HUMANCAUSED_EthnicCats_T <-plots[[10]]   + theme_clean() 
HUMANCAUSED_EthnicCats_T # 12 X 800
## edu
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("Edu.S","years [0,4.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
HUMANCAUSED_Edu.S <-plots[[10]]   + theme_clean() 
HUMANCAUSED_Edu.S # 12 X 800

# political orientation
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("Pol.Orient.S","years [0,4.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = TRUE, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 

HUMANCAUSED_Pol.Orient.S <-plots[[10]]  + theme_clean() 
HUMANCAUSED_Pol.Orient.S # 12 X 800

# RELIGION

m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("Relid.S [minmax]","years [0,4.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")   + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
HUMANCAUSED_RELIDS_T <-plots[[10]] + theme_clean() 
HUMANCAUSED_RELIDS_T # 12 X 800
# 
# CONCERN_T_RELIDS + CONCERN_RELIDS_T + plot_annotation(title="Religious New Zealanders Less Sensitive to Climate Concern: 2013-2018")
# 
# CONCERN_T_RELIDS_CONCERN_RELIDS_T # 2000 x1000


####
####
#### EFFICACY
#### 
#### 
#### 

dom$EnvEfficacy
# Models
m <- 10
models <- NULL
for(i in 1:m) {
  models[[i]] <- lmer(EnvEfficacy ~  years * (Relid.S + Pol.Orient.S + Edu.S)  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  + (1|Id), data=domALL$imputations[[i]])
}

#devtools::install_github("easystats/parameters") Get development version
library(parameters)
mdomEFFICACY <- lapply(models, model_parameters)
parameters::pool_parameters(mdomEFFICACY)%>%tibble()%>%
  dplyr::select(Parameter, Coefficient, SE, CI_low, CI_high)%>%
  mutate_if(is.numeric, ~round(., 3))%>%
  # mutate_all(funs(str_replace(., "b_", "")))%>%
  # mutate_all(funs(str_replace(., "_", " X ")))%>%
  ggpubr::ggtexttable(rows = NULL,
                      theme = ttheme("mOrange")) 


mEFFICACY<-pool_parameters(mdomEFFICACY)
mEFFICACY
# COEFF PLOT
plot(mEFFICACY) + ggtitle("Coefficient Plot: Environmental Human Efficacy") # pick your  own title

## EXPECTATION PLOTS
## 
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("years [0:9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = TRUE, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
EFFICACY_TIME <-plots[[10]]  + theme_clean() 
EFFICACY_TIME # 12 X 800
# 
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("EthnicCats","years [0,4.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
EFFICACY_EthnicCats_T <-plots[[10]]   + theme_clean() 
EFFICACY_EthnicCats_T # 12 X 800


## edu
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("Edu.S","years [0,4.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
EFFICACY_Edu.S <-plots[[10]]   + theme_clean() 
EFFICACY_Edu.S # 12 X 800

# political orientation

m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("Pol.Orient.S","years [0,4.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = TRUE, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
EFFICACY_Pol.Orient.S <-plots[[10]] + theme_clean() 
EFFICACY_Pol.Orient.S # 12 X 800

# RELIGION
## EXPECTATION PLOTS


# RELIGION
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("Relid.S [minmax]","years [0,4.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")   + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
EFFICACY_RELIDS_T <-plots[[10]] + theme_clean() 
EFFICACY_RELIDS_T # 12 X 800
# 
# EFFICACY_T_RELIDS + EFFICACY_RELIDS_T + plot_annotation(title="Religious New Zealanders Less Sensitive to Climate Concern: 2013-2018")
# 
# CONCERN_T_RELIDS_CONCERN_RELIDS_T # 2000 x1000

##
## SACRIFICE WILLING
##
# 
# # Models
# m <- 10
# models <- NULL
# for(i in 1:m) {
#   models[[i]] <- lmer(Env.SacWilling ~  years * (Relid.S + Pol.Orient.S + Edu.S  + Your_Future_SecurityS)  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  + (1|Id), data=domALL$imputations[[i]])
# }
# 
# #devtools::install_github("easystats/parameters") Get development version
# library(parameters)
# library(texreg)
# mdomSACRIFICEWILLING <- lapply(models, model_parameters)
# parameters::pool_parameters(mdomSACRIFICEWILLING)%>%tibble()%>%
#   dplyr::select(Parameter, Coefficient, SE, CI_low, CI_high)%>%
#   mutate_if(is.numeric, ~round(., 3))%>%
#   # mutate_all(funs(str_replace(., "b_", "")))%>%
#   # mutate_all(funs(str_replace(., "_", " X ")))%>%
#   ggpubr::ggtexttable(rows = NULL,
#                       theme = ttheme("mOrange")) 
# 
# 
# mSACRIFICEWILLING<-pool_parameters(mdomSACRIFICEWILLING)
# mSACRIFICEWILLING
# # COEFF PLOT
# plot(mSACRIFICEWILLING) + ggtitle("Environmental Sacrifice Willing") # pick your  own title
# 
# ## EXPECTATION PLOTS
# ## 
# m<-10
# out<-NULL
# for(i in 1:m) {
#   out[[i]] <-  ggpredict(models[[i]], terms =c("years [0:9]"))
# }
# m<-10
# plots<-NULL
# for(i in 1:m) {
#   plots[[i]] <- plot(out[[i]], facets = TRUE, color="viridis")  + scale_y_continuous(limits=c(4,6))
# }
# # Inspect all  # No differences 
# plots[[1]]
# plots[[2]]
# plots[[3]]
# plots[[4]]
# plots[[5]]
# plots[[6]]
# plots[[7]]
# plots[[8]]
# plots[[9]]
# plots[[10]] 
# 
# 
# library(ggthemes)
# SACRIFICEWILLING_TIME <-plots[[10]]   +   ggtitle("Human Efficacy: (2013-2018)") + theme_clean() 
# SACRIFICEWILLING_TIME # 12 X 800
# # 
# m<-10
# out<-NULL
# for(i in 1:m) {
#   out[[i]] <-  ggpredict(models[[i]], terms =c("EthnicCats","years [0,4.5,9]"))
# }
# m<-10
# plots<-NULL
# for(i in 1:m) {
#   plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
# }
# # Inspect all  # No differences 
# plots[[1]]
# plots[[2]]
# plots[[3]]
# plots[[4]]
# plots[[5]]
# plots[[6]]
# plots[[7]]
# plots[[8]]
# plots[[9]]
# plots[[10]] 
# 
# 
# library(ggthemes)
# SACRIFICEWILLING_EthnicCats_T <-plots[[10]]   + theme_clean() 
# SACRIFICEWILLING_EthnicCats_T # 12 X 800
# 
# 
# ## edu
# m<-10
# out<-NULL
# for(i in 1:m) {
#   out[[i]] <-  ggpredict(models[[i]], terms =c("Edu.S","years [0,4.5,9]"))
# }
# m<-10
# plots<-NULL
# for(i in 1:m) {
#   plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
# }
# # Inspect all  # No differences 
# plots[[1]]
# plots[[2]]
# plots[[3]]
# plots[[4]]
# plots[[5]]
# plots[[6]]
# plots[[7]]
# plots[[8]]
# plots[[9]]
# plots[[10]] 
# 
# 
# library(ggthemes)
# SACRIFICEWILLING_Edu.S <-plots[[10]]   + theme_clean() 
# SACRIFICEWILLING_Edu.S # 12 X 800
# 
# # political orientation
# 
# m<-10
# out<-NULL
# for(i in 1:m) {
#   out[[i]] <-  ggpredict(models[[i]], terms =c("Pol.Orient.S","years [0,4.5,9]"))
# }
# m<-10
# plots<-NULL
# for(i in 1:m) {
#   plots[[i]] <- plot(out[[i]], facets = TRUE, color="viridis")  + scale_y_continuous(limits=c(4,6))
# }
# # Inspect all  # No differences 
# plots[[1]]
# plots[[2]]
# plots[[3]]
# plots[[4]]
# plots[[5]]
# plots[[6]]
# plots[[7]]
# plots[[8]]
# plots[[9]]
# plots[[10]] 
# 
# 
# library(ggthemes)
# SACRIFICEWILLING_Pol.Orient.S <-plots[[10]]    + theme_clean() 
# SACRIFICEWILLING_Pol.Orient.S # 12 X 800
# 
# # RELIGION
# ## EXPECTATION PLOTS
# 
# 
# # RELIGION
# m<-10
# out<-NULL
# for(i in 1:m) {
#   out[[i]] <-  ggpredict(models[[i]], terms =c("Relid.S [minmax]","years [0,4.5,9]"))
# }
# m<-10
# plots<-NULL
# for(i in 1:m) {
#   plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
# }
# # Inspect all  # No differences 
# plots[[1]]
# plots[[2]]
# plots[[3]]
# plots[[4]]
# plots[[5]]
# plots[[6]]
# plots[[7]]
# plots[[8]]
# plots[[9]]
# plots[[10]] 
# 
# 
# library(ggthemes)
# SACRIFICEWILLING_RELIDS_T <-plots[[10]] + theme_clean() 
# SACRIFICEWILLING_RELIDS_T # 12 X 800
# 

####
####
#### Have made a sacrifice 
####
####

dev.off()
# Models
m <- 10
models <- NULL
for(i in 1:m) {
  models[[i]] <- lmer(Env.SacMade ~  years * (Relid.S + Pol.Orient.S + Edu.S)  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  + (1|Id), data=domALL$imputations[[i]])
}

#devtools::install_github("easystats/parameters") Get development version

mdomSACRIFICEMADE <- lapply(models, model_parameters)
parameters::pool_parameters(mdomSACRIFICEMADE)%>%tibble()%>%
  dplyr::select(Parameter, Coefficient, SE, CI_low, CI_high)%>%
  mutate_if(is.numeric, ~round(., 3))%>%
  # mutate_all(funs(str_replace(., "b_", "")))%>%
  # mutate_all(funs(str_replace(., "_", " X ")))%>%
  ggpubr::ggtexttable(rows = NULL,
                      theme = ttheme("mOrange")) 


mSACRIFICEMADE<-pool_parameters(mdomSACRIFICEMADE)
mSACRIFICEMADE
# COEFF PLOT
plot(mSACRIFICEMADE) + ggtitle("Coefficient Plot: Sacrifice Made for the Environment") # pick your  own title

## EXPECTATION PLOTS
## 
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("years [0,4.5,9]]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = TRUE, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
SACRIFICEMADE_TIME <-plots[[10]] + theme_clean() 
SACRIFICEMADE_TIME # 12 X 800
# 
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("EthnicCats","years [0,4.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
SACRIFICEMADE_EthnicCats_T <-plots[[10]]   + theme_clean() 
SACRIFICEMADE_EthnicCats_T # 12 X 800


## edu
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("Edu.S","years [0,4.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
SACRIFICEMADE_Edu.S <-plots[[10]]   + theme_clean() 
SACRIFICEMADE_Edu.S # 12 X 800

# political orientation

m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("Pol.Orient.S","years [0,4.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = TRUE, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
SACRIFICEMADE_Pol.Orient.S <-plots[[10]] + theme_clean() 
SACRIFICEMADE_Pol.Orient.S # 12 X 800

# RELIGION
## EXPECTATION PLOTS


# RELIGION
m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("Relid.S [minmax]","years [0,4.5,9]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")   + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]] 


library(ggthemes)
SACRIFICEMADE_RELIDS_T <-plots[[10]] + theme_clean() 
SACRIFICEMADE_RELIDS_T # 12 X 800



#### EXTRA ANALYSIS
#### 
# Models
m <- 10
models <- NULL
for(i in 1:m) {
  models[[i]] <-lmer(Env.SacMade ~ years * (Env.SatNZEnvironmentS*Relid.S) + Pol.Orient.S + Edu.S  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  +  (1|Id), data=domALL$imputations[[i]])
}
#devtools::install_github("easystats/parameters") Get development version
library(parameters)
library(texreg)
mdomSACRIFICEMADE
mdomSACRIFICEMADE <- lapply(models, model_parameters)
parameters::pool_parameters(mdomSACRIFICEMADE)%>%tibble()%>%
  dplyr::select(Parameter, Coefficient, SE, CI_low, CI_high)%>%
  mutate_if(is.numeric, ~round(., 3))%>%
  # mutate_all(funs(str_replace(., "b_", "")))%>%
  # mutate_all(funs(str_replace(., "_", " X ")))%>%
  ggpubr::ggtexttable(rows = NULL,
                      theme = ttheme("mOrange")) 


mSACRIFICEMADE<-pool_parameters(models, component = "all")
mSACRIFICEMADE
# COEFF PLOT
plot(mSACRIFICEMADE) + ggtitle("") # pick your  own title



## EXPECTATION PLOTS
## 

m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("years [0,4.5,9]","Env.SatNZEnvironmentS [meansd]","Relid.S [minmax]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
p

library(ggthemes)
X_SACRIFICEMADE_Env.SatNZEnvironmentS_Relid.S <-plots[[10]]   + theme_clean() 
X_SACRIFICEMADE_Env.SatNZEnvironmentS_Relid.S# 12 X 800



### 2
# Models
m <- 10
models <- NULL
for(i in 1:m) {
  models[[i]] <-lmer(Env.SacMade ~ years * (Env.SatNZEnvironmentS*Pol.Orient.S)+ Relid.S + Edu.S  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  +  (1|Id), data=domALL$imputations[[i]])
}
#devtools::install_github("easystats/parameters") Get development version
library(parameters)
library(texreg)
mdomSACRIFICEMADE <- lapply(models, model_parameters)
parameters::pool_parameters(mdomSACRIFICEMADE)%>%tibble()%>%
  dplyr::select(Parameter, Coefficient, SE, CI_low, CI_high)%>%
  mutate_if(is.numeric, ~round(., 3))%>%
  # mutate_all(funs(str_replace(., "b_", "")))%>%
  # mutate_all(funs(str_replace(., "_", " X ")))%>%
  ggpubr::ggtexttable(rows = NULL,
                      theme = ttheme("mOrange")) 


mSACRIFICEMADE<-pool_parameters(mdomSACRIFICEMADE)
mSACRIFICEMADE
# COEFF PLOT
plot(mSACRIFICEMADE) + ggtitle("") # pick your  own title

## EXPECTATION PLOTS
## 

m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("years [0,4.5,9]","Env.SatNZEnvironmentS [meansd]","Pol.Orient.S [minmax]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[10]] 


library(ggthemes)
X_SACRIFICEMADE_Env.SatNZEnvironmentS_Pol.Orient.S <-plots[[10]]   + theme_clean() 
X_SACRIFICEMADE_Env.SatNZEnvironmentS_Pol.Orient.S# 12 X 800


#### 3
# Models
m <- 10
models <- NULL
for(i in 1:m) {
  models[[i]] <-lmer(Env.SacMade ~ years * (Your_Future_SecurityS*Relid.S) + Pol.Orient.S + Edu.S  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  +  (1|Id), data=domALL$imputations[[i]])
}
#devtools::install_github("easystats/parameters") Get development version
library(parameters)
library(texreg)
mdomSACRIFICEMADE <- lapply(models, model_parameters)
parameters::pool_parameters(mdomSACRIFICEMADE)%>%tibble()%>%
  dplyr::select(Parameter, Coefficient, SE, CI_low, CI_high)%>%
  mutate_if(is.numeric, ~round(., 3))%>%
  # mutate_all(funs(str_replace(., "b_", "")))%>%
  # mutate_all(funs(str_replace(., "_", " X ")))%>%
  ggpubr::ggtexttable(rows = NULL,
                      theme = ttheme("mOrange")) 


mSACRIFICEMADE<-pool_parameters(mdomSACRIFICEMADE)
mSACRIFICEMADE
# COEFF PLOT
plot(mSACRIFICEMADE) + ggtitle("") # pick your  own title

## EXPECTATION PLOTS
## 

m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("years","Your_Future_SecurityS [meansd]","Relid.S [minmax]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[10]] 


library(ggthemes)
X_SACRIFICEMADE_Your_Future_SecurityS_Relid.S <-plots[[10]]   + theme_clean() 
X_SACRIFICEMADE_Your_Future_SecurityS_Relid.S# 12 X 800  # USE



### 4
# Models
m <- 10
models <- NULL
for(i in 1:m) {
  models[[i]] <-lmer(Env.SacMade ~ years * (Your_Future_SecurityS*Pol.Orient.S) + Relid.S + Edu.S  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  +  (1|Id), data=domALL$imputations[[i]])
}
#devtools::install_github("easystats/parameters") Get development version
library(parameters)
library(texreg)
mdomSACRIFICEMADE <- lapply(models, model_parameters)
parameters::pool_parameters(mdomSACRIFICEMADE)%>%tibble()%>%
  dplyr::select(Parameter, Coefficient, SE, CI_low, CI_high)%>%
  mutate_if(is.numeric, ~round(., 3))%>%
  # mutate_all(funs(str_replace(., "b_", "")))%>%
  # mutate_all(funs(str_replace(., "_", " X ")))%>%
  ggpubr::ggtexttable(rows = NULL,
                      theme = ttheme("mOrange")) 


mSACRIFICEMADE<-pool_parameters(mdomSACRIFICEMADE)
mSACRIFICEMADE
# COEFF PLOT
plot(mSACRIFICEMADE) + ggtitle("") # pick your  own title

## EXPECTATION PLOTS
## 

m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("years","Your_Future_SecurityS [meansd]","Pol.Orient.S [minmax]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[10]] 


library(ggthemes)
X_SACRIFICEMADE_Your_Future_SecurityS_Pol.Orient.S <-plots[[10]]   + theme_clean() 
X_SACRIFICEMADE_Your_Future_SecurityS_Pol.Orient.S# 12 X 800


####  5 PUB TRANS SUB


m <- 10
models <- NULL
for(i in 1:m) {
  models[[i]] <-lmer(Env.PubTransSubs ~ years * (Env.ClimateChgConcernS)+ Pol.Orient.S + Relid.S + Edu.S  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  + (1|Id), data=domALL$imputations[[i]])
}
#devtools::install_github("easystats/parameters") Get development version
library(parameters)
library(texreg)
mdomPubTransSubs <- lapply(models, model_parameters)
parameters::pool_parameters(mdomPubTransSubs)%>%tibble()%>%
  dplyr::select(Parameter, Coefficient, SE, CI_low, CI_high)%>%
  mutate_if(is.numeric, ~round(., 3))%>%
  # mutate_all(funs(str_replace(., "b_", "")))%>%
  # mutate_all(funs(str_replace(., "_", " X ")))%>%
  ggpubr::ggtexttable(rows = NULL,
                      theme = ttheme("mOrange")) 


mPubTransSubs<-pool_parameters(mdomPubTransSubs)
mPubTransSubs
# COEFF PLOT
plot(mPubTransSubs)# + ggtitle("") # pick your  own title

## EXPECTATION PLOTS
## 

m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("years [4,6.5,9]","Env.ClimateChgConcernS [meansd]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(4,6))
}

library(ggthemes)
XX_PLOT_PubTransSubsEnv.ClimateChgConcernS <-plots[[10]]   + theme_clean() 
XX_PLOT_PubTransSubsEnv.ClimateChgConcernS# 12 X 800


####   Env.MotorwaySpend
m <- 10
models <- NULL
for(i in 1:m) {
  models[[i]] <-lmer(Env.MotorwaySpend ~ years * (Env.ClimateChgConcernS) + Pol.Orient.S + Relid.S + Edu.S  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  +  (1|Id), data=domALL$imputations[[i]])
}
#devtools::install_github("easystats/parameters") Get development version
library(parameters)
library(texreg)
mdomEnv.MotorwaySpend <- lapply(models, model_parameters)
parameters::pool_parameters(mdomEnv.MotorwaySpend)%>%tibble()%>%
  dplyr::select(Parameter, Coefficient, SE, CI_low, CI_high)%>%
  mutate_if(is.numeric, ~round(., 3))%>%
  # mutate_all(funs(str_replace(., "b_", "")))%>%
  # mutate_all(funs(str_replace(., "_", " X ")))%>%
  ggpubr::ggtexttable(rows = NULL,
                      theme = ttheme("mOrange")) 


mEnv.MotorwaySpend<-pool_parameters(mdomEnv.MotorwaySpend)
mEnv.MotorwaySpend
# COEFF PLOT
plot(mEnv.MotorwaySpend)# + ggtitle("") # pick your  own title

## 
## Env.MotorwaySpend

m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("years [4,6.5,9]","Env.ClimateChgConcernS [meansd]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis") + scale_y_continuous(limits=c(3,6))
}
# Inspect all  # No differences 
plots[[10]] 


library(ggthemes)
XX_PLOT_Env.MotorwaySpend.ENVCLIMATECONCERN <-plots[[10]]   + theme_clean() 
XX_PLOT_Env.MotorwaySpend.ENVCLIMATECONCERN# 12 X 800


yy
####   Env.NATIVE.SPECIES
m <- 10
models <- NULL
for(i in 1:m) {
  models[[i]] <-lmer(Env.NATIVE.SPECIES ~ years * (Env.ClimateChgConcernS)+ Pol.Orient.S + Relid.S + Edu.S  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  +  (1|Id), data=domALL$imputations[[i]])
}
#devtools::install_github("easystats/parameters") Get development version
library(parameters)
library(texreg)
mdomEnv.NATIVE.SPECIES <- lapply(models, model_parameters)
parameters::pool_parameters(mdomEnv.MotorwaySpend)%>%tibble()%>%
  dplyr::select(Parameter, Coefficient, SE, CI_low, CI_high)%>%
  mutate_if(is.numeric, ~round(., 3))%>%
  # mutate_all(funs(str_replace(., "b_", "")))%>%
  # mutate_all(funs(str_replace(., "_", " X ")))%>%
  ggpubr::ggtexttable(rows = NULL,
                      theme = ttheme("mOrange")) 


mEnv.NATIVE.SPECIES<-pool_parameters(mdomEnv.NATIVE.SPECIES)
mEnv.NATIVE.SPECIES
# COEFF PLOT
plot(mEnv.NATIVE.SPECIES)# + ggtitle("") # pick your  own title

## 
## Env.NATIVE.SPECIES

m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("years [4,6.5,9]","Env.ClimateChgConcernS [meansd]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis")  + scale_y_continuous(limits=c(5,7))
}
# Inspect all  # No differences 
plots[[10]] 


library(ggthemes)
XX_PLOT_Env.NATIVE.SPECIES.ENVCONCERN <-plots[[10]]   + theme_clean() 
XX_PLOT_Env.NATIVE.SPECIES.ENVCONCERN# 12 X 800





####  5 PUB TRANS SUB

m <- 10
models <- NULL
for(i in 1:m) {
  models[[i]] <-lmer(Env.PubTransSubs ~ years * (Env.SatNZEnvironmentS)+ Pol.Orient.S + Relid.S + Edu.S  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  +  (1|Id), data=domALL$imputations[[i]])
}

## EXPECTATION PLOTS
## 

m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("years [0,4.5,9]","Env.SatNZEnvironmentS [meansd]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis") + scale_y_continuous(limits=c(4,6))
}
# Inspect all  # No differences 
plots[[10]] 


library(ggthemes)
XY_PLOT_PubTransSubsSATENVIRON <-plots[[10]]   + theme_clean() 
XY_PLOT_PubTransSubsSATENVIRON# 12 X 800 # USE


####   Env.MotorwaySpend
m <- 10
models <- NULL
for(i in 1:m) {
  models[[i]] <-lmer(Env.MotorwaySpend ~ years * (Env.SatNZEnvironmentS)+ Pol.Orient.S + Relid.S + Edu.S  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  +  (1|Id), data=domALL$imputations[[i]])
}
#devtools::install_github("easystats/parameters") Get development version
library(parameters)
library(texreg)
mdomEnv.MotorwaySpend <- lapply(models, model_parameters)
parameters::pool_parameters(mdomEnv.MotorwaySpend)%>%tibble()%>%
  dplyr::select(Parameter, Coefficient, SE, CI_low, CI_high)%>%
  mutate_if(is.numeric, ~round(., 3))%>%
  # mutate_all(funs(str_replace(., "b_", "")))%>%
  # mutate_all(funs(str_replace(., "_", " X ")))%>%
  ggpubr::ggtexttable(rows = NULL,
                      theme = ttheme("mOrange")) 


mEnv.MotorwaySpend<-pool_parameters(mdomEnv.MotorwaySpend)
mEnv.MotorwaySpend
# COEFF PLOT
plot(mEnv.MotorwaySpend)# + ggtitle("") # pick your  own title

## 
## Env.MotorwaySpend

m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("years [0,4.5,9]","Env.SatNZEnvironmentS [meansd]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis") + scale_y_continuous(limits=c(3,6))
}
# Inspect all  # No differences 
plots[[10]] 


library(ggthemes)
XY_PLOT_Env.MotorwaySpend.SATENVIRON <-plots[[10]]   + theme_clean() 
XY_PLOT_Env.MotorwaySpend.SATENVIRON# 12 X 800

####   Env.NATIVE.SPECIES
m <- 10
models <- NULL
for(i in 1:m) {
  models[[i]] <-lmer(Env.NATIVE.SPECIES ~ years * (Env.SatNZEnvironmentS)+ Pol.Orient.S + Relid.S + Edu.S  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  +  (1|Id), data=domALL$imputations[[i]])
}
#devtools::install_github("easystats/parameters") Get development version
library(parameters)
library(texreg)
mdomEnv.NATIVE.SPECIES <- lapply(models, model_parameters)
parameters::pool_parameters(mdomEnv.MotorwaySpend)%>%tibble()%>%
  dplyr::select(Parameter, Coefficient, SE, CI_low, CI_high)%>%
  mutate_if(is.numeric, ~round(., 3))%>%
  # mutate_all(funs(str_replace(., "b_", "")))%>%
  # mutate_all(funs(str_replace(., "_", " X ")))%>%
  ggpubr::ggtexttable(rows = NULL,
                      theme = ttheme("mOrange")) 


mEnv.NATIVE.SPECIES<-pool_parameters(mdomEnv.NATIVE.SPECIES)
mEnv.NATIVE.SPECIES
# COEFF PLOT
plot(mEnv.NATIVE.SPECIES)# + ggtitle("") # pick your  own title

## 
## Env.NATIVE.SPECIES

m<-10
out<-NULL
for(i in 1:m) {
  out[[i]] <-  ggpredict(models[[i]], terms =c("years [0,4.5,9]","Env.SatNZEnvironmentS [meansd]"))
}
m<-10
plots<-NULL
for(i in 1:m) {
  plots[[i]] <- plot(out[[i]], facets = T, color="viridis") + scale_y_continuous(limits=c(5,7))
}
XY_PLOT_Env.NATIVE.SPECIES.SATENVIRON <-plots[[10]]   + theme_clean() 
XY_PLOT_Env.NATIVE.SPECIES.SATENVIRON# 12 X 800


# 
# library(brms)
# #library(brmstools)
# library(rstan)
# library(ggeffects)
# library(sjPlot)
# library(wesanderson)
# 
# conflicts()
# ##ask to run models on multiple cores
# rstan_options(auto_write=TRUE)
# options(mc.cores=parallel::detectCores ())
# #library(lazerhawk)  # goodtables
# head(amelia.list)
# dev.off()
# ## run model
# lm.0<- lmer(Env.ClimateChgConcern ~  years * (Relid.S + Pol.Orient.S + Edu.S + KESSLER6S)  + Age.C.decade  + EthnicCats  + Male + NZdepS + Urban  + (1|Id),  data = dat.dom) # 
# 
# plot_model(lm.0)
# tab_model(lm.0)
# show_pals()
# min(dat.dom$Relid.S)
# max(dat.dom$Relid.S)
# max(dat.dom$Pol.Orient.S)
# min(dat.dom$Edu.S)
# max(dat.dom$Edu.S)
# pr1 <- ggpredict(lm.0, c("years"))
# plot0<-plot(pr1, facet =T) + scale_y_continuous(limits = c(3,7))  + geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5)
# 
# plot0
# pr1a <- ggpredict(lm.0, c("Relid.S [-.72:1.93,by=.01]","years [0,4.5,9]"))
# pr1b <- ggpredict(lm.0, c("Pol.Orient.S [-1.94:2.47,by=.01]","years [0,4.5,9]"))
# pr1c <- ggpredict(lm.0, c("Edu.S [-1.84:1.72,by=.01]","years [0,4.5,9]"), type = "fe")
# pr1d <- ggpredict(lm.0, c("EthnicCats","years [0,4.5,9]"), type = "fe")
# pr1e <- ggpredict(lm.0, c("KESSLER6S","years [0,4.5,9]"), type = "fe")


p1<-plot(pr1a, facet =T,    
       # ci.style = "errorbar", 
    #  ci.style = "dot", 
     dot.size=1.5,
     colors="viridis") +
  scale_y_continuous(limits = c(3.75,6)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5)+theme_clean()

p1
p2<- plot(pr1b,  facet =T,
  #   ci.style = "errorbar", 
     # ci.style = "dot", 
     dot.size=1.5,
     colors="viridis") +
  scale_y_continuous(limits = c(3.75,6)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +theme_clean()

p2
p3<- plot(pr1c, facet =T,
   #  ci.style = "errorbar", 
    # ci.style = "dot", 
     dot.size=1.5,
     colors="viridis") +
  scale_y_continuous(limits = c(3.75,6))  +  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5)+theme_clean()
p4<- plot(pr1d, facet =T,
          #  ci.style = "errorbar", 
          # ci.style = "dot", 
          dot.size=1.5,
          colors="viridis") +
  scale_y_continuous(limits = c(3.75,6))  +  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5)+theme_clean()
p5<- plot(pr1e, facet =T,
          #  ci.style = "errorbar", 
          # ci.style = "dot", 
          dot.size=1.5,
          colors="viridis") +
  scale_y_continuous(limits = c(3.75,6))  +  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5)+theme_clean()
p5

(p1/p2/p3/p4/p5) +  plot_annotation(title = 'Predicted values of "Deeply Concerned About Climate"')




pr1 <- ggpredict(lm.0, c("years"))
pr1a <- ggpredict(lm.0, c("Relid.S [-2,2]","years [0,4.5,9]"))
pr1b <- ggpredict(lm.0, c("Pol.Orient.S [-2,2]","years [0,4.5,9]"))
plot0<-plot(pr1, facet =T) + scale_y_continuous(limits = c(3,7))
plot1<-plot(pr1a, facet =T) + scale_y_continuous(limits = c(3,7))
plot2<- plot(pr1b, facet =T) + scale_y_continuous(limits = c(3,7))

library(patchwork)
(plot0/(plot1 |plot2))  + plot_annotation(tag_levels = 'a',
                                          title = 'T',
                                          subtitle = '',
                                          caption = '',)



lm.0A<- lmer(Env.SatNZEnvironment ~ 
              years * (Relid.S + Pol.Orient.S) + Age.C.decade + Edu + EthnicCats 
            + Male + Urban  + (1|Id) +(1|years.f), 
            data = dat.dom) # 

plot_model(lm.0A)
tab_model(lm.0A)
pr1a <- ggpredict(lm.0A, c("Relid.S [-2,2]","years [0,4.5,9]"))
pr1b <- ggpredict(lm.0A, c("Pol.Orient.S [-2,2]","years [0,4.5,9]"))
plot1<-plot(pr1a, facet =T) + scale_y_continuous(limits = c(3,7))
plot2<- plot(pr1b, facet =T) + scale_y_continuous(limits = c(3,7))

library(patchwork)
{plot1} + 
  {plot2}



lm.1<- lmer(Env.SacMade ~ 
              years * (Relid.S + Pol.Orient.S) + Age.C.decade + Edu + EthnicCats 
               + Male + Urban  + (1|Id) +(1|years.f), 
            data = dat.dom) # 

plot_model(lm.1)
tab_model(lm.1)
pr1 <- ggpredict(lm.1, c("Relid.S [-2,2]","years [0,4.5,9]"))
pr1 <- ggpredict(lm.1, c("Pol.Orient.S [-2,2]","years [0,4.5,9]"))
plot(pr1, facet =T) + scale_y_continuous(limits = c(4.5,6))



lm.2<- lmer(Env.ClimateChgCause ~ 
              years * (Relid.S + Pol.Orient.S) + Age.C.decade + Edu + EthnicCats 
            + Male + Urban  + (1|Id) +(1|years.f), 
            data = dat.dom) # 
plot_model(lm.2)
pr2 <- ggpredict(lm.2, c("Relid.S [-2:2,by=.05]","years [0,4.5,9]"))
pr2 <- ggpredict(lm.2, c("Pol.Orient.S [-2,2]","years [0,4.5,9]"))
plot(pr2, facet =T,ci.style = "errorbar", colors="bw") +scale_y_continuous(limits = c(3,7)) 
 

tab_model(lm.1)

lm.3<- lmer(EnvEfficacy ~ 
              years * (Relid.S + Pol.Orient.S) + Age.C.decade + Edu + EthnicCats 
            + Male + Urban  + (1|Id) +(1|years.f), 
            data = dat.dom) # 
plot_model(lm.3)
plot_model(lm.3)
pr3 <- ggpredict(lm.3, c("Relid.S [-2,2]","years [0,4.5,9]"))
pr3 <- ggpredict(lm.3, c("Pol.Orient.S [-2,2]","years [0,4.5,9]"))
plot(pr3, facet =T)

lm.4<- lmer(Env.SacWilling ~ 
              years * (Relid.S + Pol.Orient.S) + Age.C.decade + Edu + EthnicCats 
            + Male + Urban  + (1|Id) +(1|years.f), 
            data = dat.dom) # 
plot_model(lm.4)
pr4 <- ggpredict(lm.4, c("Relid.S [-2,2]","years [0,4.5,9]"))
pr4 <- ggpredict(lm.4, c("Pol.Orient.S [-2,2]","years [0,4.5,9]"))
plot(pr4, facet =T)


lm.5<- lmer(Env.SacWilling ~ 
              years * (Relid.S + Pol.Orient.S) + Age.C.decade + Edu + EthnicCats 
            + Male + Urban  + (1|Id) +(1|years.f), 
            data = dat.dom) # 
plot_model(lm.5)
pr5 <- ggpredict(lm.5, c("Relid.S [-2,2]","years [0,4.5,9]"))
pr5 <- ggpredict(lm.5, c("Pol.Orient.S [-2,2]","years [0,4.5,9]"))
plot(pr5, facet =T)



####

summary(lm.1)
plot_model(lm.1)
tab_model(lm.1)
summary(dat.dom$Relid.S)

plot(pr1, facet =T)
pr1 <- ggpredict(lm.1, c("Relid.S [-2,2]","years [0,4.5,9]"))
pr1 <- ggpredict(lm.1, c("Pol.Orient.S [-2,2]","years [0,4.5,9]"))
pr1 <- ggpredict(lm.1, c("EthnicCats","years [0,4.5,9]"))

plot(pr1, facet =T)


plot(pr1, facet =T, use.theme=F) + 
  theme_linedraw() + theme(plot.title = element_text(size=09)) +
  scale_x_continuous(labels = c("-2sd", "-1sd", "mean", "+1sd","+2sd"), 
                     breaks = c(-2,-1,0,1,2))+
  labs(
    #  labeller=labeller(years = year.labs),
    x = "Religious Identification (Standard Deviation Units: -2SD to +2SD)",
    y = "Climate Change is Real: 1-7",
    title = "Predicted values of Beliefs that Climate Change is Real by Religious ID in New Zealand 2009 -2017 (years =  0 - 8, N = 31,464)",
    colour = get_legend_title(pr1)
  )



pr1.1 <- ggpredict(lm.1, "years")

plot(pr1.1, facet =F, 
     # color ="paired", 
     use.theme=F) +
  #scale_color_grey() + 
  #theme_minimal()+
  theme_linedraw()+  theme(plot.title = element_text(size=10)) +
  # theme_black() + 
  # theme_grey() +
  #scale_y_continuous(limits=c(1,7)) + 
  # scale_x_continuous(limits=c(-2,2)) +
  scale_x_continuous(labels = c("2009", "2010", "2011","2012", "2013","2014", "2015","2016", "2017"), breaks = c(-0,1,2,3,4,5,6,7,8))+
  labs(
    labeller=labeller(years = year.labs),
    x = "years: 2009 - 2017",
    y = "Climate Change is Real: 1-7",
    title = "Predicted values of Beliefs that Climate Change is Real in New Zealand 2009-2017 (N = 31,464)",
    colour = get_legend_title(pr5)
  )


lm.2<- lmer(Env.ClimateChgCause ~ 
              years * (Relid.S + Pol.Orient.S) + Age.C.decade + Edu +
              Employed + Male + Urban  + (1|Id) +(1|years.f), 
            data = dat.dom) # 
plot_model(lm.2)
tab_model(lm.2)


pr2 <- ggpredict(lm.2, c("Relid.S [-2,2]","years"))

plot(pr2, facet =T, color ="paired", use.theme=F) +
  #scale_color_grey() + 
  #theme_minimal()+
  theme_linedraw()+theme(plot.title = element_text(size=09)) +
  # theme_black() + 
  # theme_grey() +
  #scale_y_continuous(limits=c(1,7)) + 
  # scale_x_continuous(limits=c(-2,2)) +
  scale_x_continuous(labels = c("-2sd", "-1sd", "mean", "+1sd","+2sd"), 
                     breaks = c(-2,-1,0,1,2))+
  labs(
    #  labeller=labeller(years = year.labs),
    x = "Religious Identification (Standard Deviation Units: -1SD to +3SD)",
    y = "Anthropogenic Climate Change Beliefs: 1-7",
    title = "Predicted values of Anthropogenic Climate Change Beliefs by Religious ID in New Zealand 2009 -2017 (years =  0 - 8, N = 31,464)",
    colour = get_legend_title(pr2)
  )



pr2.1 <- ggpredict(lm.2, "years")

plot(pr2.1, facet =F, 
     # color ="paired", 
     use.theme=F) +
  #scale_color_grey() + 
  #theme_minimal()+
  theme_linedraw()+ theme(plot.title = element_text(size=10)) +
  # theme_black() + 
  # theme_grey() +
  #scale_y_continuous(limits=c(1,7)) + 
  # scale_x_continuous(limits=c(-2,2)) +
  scale_x_continuous(labels = c("2009", "2010", "2011","2012", "2013","2014", "2015","2016", "2017"), breaks = c(-0,1,2,3,4,5,6,7,8))+
  labs(
    x = "years: 2009 - 2017",
    y = "Climate Change is Real: 1-7",
    title = "Predicted values of Beliefs that Climate Change is Real in New Zealand 2009-2017 (N = 31,464)",
    colour = get_legend_title(pr2.1)
  )




lm.3<- lmer(EnvEfficacy ~ 
              years * (Relid.S + Pol.Orient.S) + Age.C.decade + Edu +
              Employed + Male + Urban  + (1|Id) +(1|years.f), 
            data = dat.dom) # 
plot_model(lm.3)
tab_model(lm.3)



pr3 <- ggpredict(lm.3, c("Relid.S [-2,2]","years"))


tab_model(lm.3)

plot(pr3, facet =T,  use.theme=F) +
  #scale_color_grey() + 
  #theme_minimal()+
  theme_linedraw()+ theme(plot.title = element_text(size=09)) +
  # theme_black() + 
  # theme_grey() +
  #scale_y_continuous(limits=c(1,7)) + 
  # scale_x_continuous(limits=c(-2,2)) +
  scale_x_continuous(labels = c("-2sd", "-1sd", "mean", "+1sd","+2sd"), 
                     breaks = c(-2,-1,0,1,2))+
  labs(
    labeller=labeller(years = year.labs),
    x = "Religious Identification (Standard Deviation Units: -1SD to +3SD)",
    y = "Personal Environmental Efficacy Beliefs: 1-7",
    title = "Predicted values of Personal Environmental Efficacy Beliefs by Religious ID in New Zealand 2009 -2017 (years =  0 - 8, N = 31,464)",
    colour = get_legend_title(pr3)
  )


pr3.1 <- ggpredict(lm.3, "years")

plot(pr3.1, facet =F, 
     # color ="paired", 
     use.theme=F) +
  #scale_color_grey() + 
  #theme_minimal()+
  theme_linedraw()+
  # theme_black() + 
  # theme_grey() +
  scale_y_continuous(limits=c(1,7)) + 
  # scale_x_continuous(limits=c(-2,2)) +
  scale_x_continuous(labels = c("2009", "2010", "2011","2012", "2013","2014", "2015","2016", "2017"), breaks = c(-0,1,2,3,4,5,6,7,8))+
  labs(
    labeller=labeller(years = year.labs),
    x = "years: 2009 - 2017",
    y = "Personal Environmental Efficacy Beliefs: 1-7",
    title = "Predicted values of Personal Environmental Efficacy Beliefsin New Zealand 2009-2017 (N = 31,464)",
    colour = get_legend_title(pr3.1)
  )


lm.4<- lmer(Env.SacWilling ~ 
              years * (Relid.S + Pol.Orient.S) + Age.C.decade + Edu +
              Employed + Male + Urban  + (1|Id) +(1|years.f), 
            data = dat.dom) # 

pr4 <- ggpredict(lm.4, c("Relid.S [-2,2]","years"))

plot(pr4, facet =T, use.theme=F) +
  #scale_color_grey() + 
  #theme_minimal()+
  theme_linedraw()+  theme(plot.title = element_text(size=09)) +
  # theme_black() + 
  # theme_grey() +
  #scale_y_continuous(limits=c(1,7)) + 
  # scale_x_continuous(limits=c(-2,2)) +
  scale_x_continuous(labels = c("-2sd", "-1sd", "mean", "+1sd","+2sd"), 
                     breaks = c(-2,-1,0,1,2))+
  labs(
    labeller=labeller(years = year.labs),
    x = "Religious Identification (Standard Deviation Units: -1SD to +3SD)",
    y = "Willingness to Sacrifice for the Environment: 1-7",
    title = "Predicted values of Willingness to Sacrifice for the Environment by Religious ID in New Zealand 2009 -2017 (years =  0 - 8, N = 31,464)",
    colour = get_legend_title(pr4)
  )


%

pr4.1 <- ggpredict(lm.4, "years")

plot(pr4.1, facet =F,  use.theme=F) +
  #scale_color_grey() + 
  #theme_minimal()+
  theme_linedraw()+ theme(plot.title = element_text(size=10)) +
  scale_x_continuous(labels = c("2009", "2010", "2011","2012", "2013","2014", "2015","2016", "2017"), breaks = c(-0,1,2,3,4,5,6,7,8))+
  labs(
    x = "years: 2009 - 2017",
    y = "Willingness to Sacrifice for the Environment: 1-7",
    title = "Predicted values of Willingness to Sacrifice for the Environment in New Zealand 2009 -2017 (years =  0 - 8, N = 31,464)",
    colour = get_legend_title(pr4.1))



lm.5<- lmer(Env.SacMade ~ 
              years * (Relid.S + Pol.Orient.S) + Age.C.decade + Edu +
              Employed + Male + Urban  + (1|Id) +(1|years.f), 
            data = dat.dom) # 


pr5 <- ggpredict(lm.5,c("Relid.S [-2,2]","years"))


plot(pr5, facet =T,use.theme=F) +
  #scale_color_grey() + 
  #theme_minimal()+
  theme_linedraw()+ theme(plot.title = element_text(size=09)) +
  scale_y_continuous(limits=c(1,7)) + 
  scale_x_continuous(labels = c("-2sd", "-1sd", "mean", "+1sd","+2sd"), 
                     breaks = c(-2,-1,0,1,2))+
  labs(
    x = "Religious Identification (Standard Deviation Units: -1SD to +3SD)",
    y = "Sacrifice Made for the Environment: 1-7",
    title = "Predicted values of Sacrifices Made the Environment by Religious ID in New Zealand 2009 -2017 (years =  0 - 8, N = 31,464)",
    colour = get_legend_title(pr5))



pr5.1 <- ggpredict(lm.5, "years")

plot(pr5.1, facet =F,use.theme=F) + theme_linedraw() + theme(plot.title = element_text(size=10)) +
  scale_y_continuous(limits=c(1,7)) + 
  scale_x_continuous(labels = c("2009", "2010", "2011","2012", "2013","2014", "2015","2016", "2017"), breaks = c(-0,1,2,3,4,5,6,7,8)) +
  labs( x = "years: 2009 - 2017",
        y = "Sacrifice Made for the Environment: 1-7",
        title = "Predicted values of Sacrifices Made the Environment in New Zealand 2009 -2017(N = 31,464)", colour = get_legend_title(pr5.1))



### HURRAY
dat.dom1<-dat.dom
dat.dom1$years<-as_label(dat.dom1$years+2009)

# Patriotism 
# I feel a great pride in the land that is our New Zealand.
# Although at times I may not agree with the government, my commitment to New Zealand always remains strong.
# Nationalism
# Generally, the more influence NZ has on other nations, the better off they are.
# Foreign nations have done some very fine things but they are still not as good as New Zealand.

lm.6<- lmer(NATIONAL ~ 
              years * (Relid.C + Pol.Orient.C) + Age.C.decade + Edu +
              Employed + Male + Urban  + (1|Id) +(1|years.f), 
            data = dat.dom) # 

plot_model(lm.6)
tab_model(lm.6)
pr6 <- ggpredict(lm.6, c("years"))
get_title(pr6)

plot(pr6, facet =F,use.theme=F) + theme_linedraw() + theme(plot.title = element_text(size=10)) +
  scale_x_continuous(labels = c("2009", "2010", "2011","2012", "2013","2014", "2015","2016", "2017"), breaks = c(-0,1,2,3,4,5,6,7,8)) +
  scale_y_continuous(limits=c(1,7)) + 
  labs( x = "years: 2009 - 2017",
        y = "Nationalism Ratings: 1-7",
        title = "Predicted values of Nationalism in New Zealand 2009 -2017, N = 31,464)", colour = get_legend_title(pr5.1))



lm.7<- lmer(PATRIOT ~ 
              years * (Relid.C + Pol.Orient.C) + Age.C.decade + Edu +
              Employed + Male + Urban  + (1|Id) +(1|years.f), 
            data = dat.dom)# 

plot_model(lm.7)
tab_model(lm.7)
pr7 <- ggpredict(lm.7, c("years"))
plot(pr7, facet =F,use.theme=F) + theme_linedraw() + theme(plot.title = element_text(size=10)) +
  scale_x_continuous(labels = c("2009", "2010", "2011","2012", "2013","2014", "2015","2016", "2017"), breaks = c(-0,1,2,3,4,5,6,7,8)) +
  scale_y_continuous(limits=c(1,7)) + 
  labs( x = "years: 2009 - 2017",
        y = "Patriotism Ratings: 1-7",
        title = "Predicted values of Patriotism in New Zealand 2009 -2017, N = 31,464)", colour = get_legend_title(pr7))



hist(dat.dom$Relid.S)


# Including religious instruction in Christianity as part of the school curriculum.

lm.8<- lmer(Issue.ReligiousEd ~ 
              years * (Relid.S + Pol.Orient.S) + Age.C.decade + Edu +
              Employed + Male + Urban  + (1|Id) +(1|years.f), 
            data = dat.dom)# 

plot_model(lm.8)
tab_model(lm.8)

pr8 <- ggpredict(lm.8, c("Relid.S [-2,2]","years"))


plot(pr8, facet =T, color ="paired", use.theme=F) +
  #scale_color_grey() + 
  #theme_minimal()+
  theme_linedraw()+
  # theme_black() + 
  # theme_grey() +
  #scale_y_continuous(limits=c(1,7)) + 
  # scale_x_continuous(limits=c(-2,2)) +
  scale_x_continuous(labels = c("-2sd", "-1sd", "mean", "+1sd","+2sd"), 
                     breaks = c(-2,-1,0,1,2))+
  labs(
    labeller=labeller(years = year.labs),
    x = "Religious Identification (Standard Deviation Units: -1SD to +3SD)",
    y = "Attitude to Religious Education: 1-7",
    title = "Predicted values of Attitude  to Religious Education by Religious ID in New Zealand 2009 -2017 (years =  0 - 8, N = 31,464)",
    colour = get_legend_title(pr8)
  )



pr5.1 <- ggpredict(lm.5, "years")

plot(pr5.1, facet =F,use.theme=F) + theme_linedraw() + theme(plot.title = element_text(size=10)) +
  scale_x_continuous(labels = c("2009", "2010", "2011","2012", "2013","2014", "2015","2016", "2017"), breaks = c(-0,1,2,3,4,5,6,7,8)) +
  labs( x = "years: 2009 - 2017",
        y = "Sacrifice Made for the Environment: 1-7",
        title = "Predicted values of Sacrifice Made the Environment in New Zealand 2009 -2017 (years =  0 - 8, N = 31,464)", colour = get_legend_title(pr5.1))



#scale_color_grey() + 
#theme_minimal()+
theme_linedraw()+
  # theme_black() + 
  # theme_grey() +
  #scale_y_continuous(limits=c(1,7)) + 
  # scale_x_continuous(limits=c(-2,2)) +
  scale_x_continuous(labels = c("-1sd", "mean", "+1sd","+2sd","+3sd"), breaks = c(-1,0,1,2,3))+
  labs(
    labeller=labeller(years = year.labs),
    x = "Religious Identification (Standard Deviation Units: -1SD to +3SD)",
    y = "Sacrifice Made for the Environment: 1-7",
    title = "Predicted values of Sacrifice Made the Environment by Religious ID in New Zealand 2009 -2017 (years =  0 - 8, N = 31,464)",
    colour = get_legend_title(pr5)
  )




