#religion-environment-rwa-wave-5.R

# Religion enviornment 

# church-use R
# set digits = 3
options(scipen=999)

dat$Env.RoutineWilling

#libraries and functions
source(here::here("scripts", "libs.R"))
source(here::here("scripts", "funs.R"))

# read data ( only work for jb's computer )
dat <- read_my_data()

dat1 <- dat %>%
  filter(YearMeasured == 1) 

# table1::table1( ~ Env.Possum.Control | Wave, data = dat1)

#OUTOCMES

# # By taking personal action I believe I can make a positive difference to environmental problems.
# Env.Eff01,
# Env.Eff01.T09,
# Env.Eff01.T12,
# 
# #Env.Eff02.T05  I feel I can make a difference to the state of the environment.
# Env.Eff02,
# Env.Eff02.T09,
# Env.Eff02.T12,
# 
# # Climate change is real.
# Env.ClimateChgReal,
# Env.ClimateChgReal.T09,
# Env.ClimateChgReal.T10,
# Env.ClimateChgReal.T11,
# Env.ClimateChgReal.T12,
# 
# # Climate change is caused by humans.
# Env.ClimateChgCause,
# Env.ClimateChgCause.T09,
# Env.ClimateChgCause.T10,
# Env.ClimateChgCause.T11,
# Env.ClimateChgCause.T12,
# 
# # I am deeply concerned about climate change.
# Env.ClimateChgConcern,
# Env.ClimateChgConcern.T09,
# Env.ClimateChgConcern.T10,
# Env.ClimateChgConcern.T11
# Env.ClimateChgConcern.T12,
# 
# 
# # Satisfaction with NZ environment
# Env.SatNZEnvironment,
# Env.SatNZEnvironment.T09,
# Env.SatNZEnvironment.T10,
# Env.SatNZEnvironment.T11,
# Env.SatNZEnvironment.T12,
# 
# 
# # Protecting New Zealand’s native species should be a national priority.
# Env.Native.Species,
# Env.Native.Species.T09,
# Env.Native.Species.T11,
# 
# 
# # Do you support the use of 1080 poison for possum control in New Zealand?
# Env.Possum.Control,
# Env.Possum.Control.T09,
# Env.Possum.Control.T11,

# table for participant N
dt <- dat %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0)) %>%
  dplyr::mutate(Religious = as.numeric(Religious) - 1) %>%
  # dplyr::rename(Prayer = Religion.Prayer2) |>
  # dplyr::rename(Scripture = Religion.Scripture2) |>
  dplyr::rename(Env.Native.Species = Env.NATIVE.SPECIES) |>
  dplyr::filter((Wave == 2013  &
                   YearMeasured  == 1 & Religious == 1) |
                  (Wave == 2014  &  Religious == 1 & YearMeasured == 1) |
                  (Wave == 2015 & YearMeasured  != -1) |
                  (Wave == 2016 & YearMeasured  != -1) | 
                  (Wave == 2017 & YearMeasured  != -1)|
                  (Wave == 2018 & YearMeasured  != -1))  %>% # Eligibility criteria
  # dplyr::filter(Id != 9630) %>% # problematic for income
  group_by(Id) %>%
  dplyr::mutate(org2 = ifelse(Wave == 2014 &
                                YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold2 = mean(org2, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold2 > 0) %>% #
  dplyr::mutate(org1 =  ifelse(Wave == 2013 &
                                 YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold1 = mean(org1, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold1 > 0) %>%
  ungroup() %>%
  droplevels() %>%
  arrange(Id, Wave)

length(unique(dt$Id)) #6879

# increasing rate
dt%>%
  group_by(Wave) %>%
  summarise(mean = mean(Env.ClimateChgCause, na.rm = TRUE),
            sd = sd(Env.ClimateChgCause, na.rm = TRUE))

## select vars
dfee <- dt %>%
  select(
    Id,
    years,
    YearMeasured,
    Wave,
    Partner,
    EthCat,
    Age,
    Male,
    NZSEI13,
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    Edu,
    SDO,
    RWA,
    NZdep,
    Employed,
    # HomeOwner,
    Pol.Orient,
    Urban,
    Household.INC,
    Parent,
    Relid,
    Religion.Church,
    Religious,
    Christian_nfd,
    # Believe.Spirit,
    #  Believe.God,
    #  Spiritual.Identification,
  #  SWB.SoC01,
    # EmotionRegulation1,
    # EmotionRegulation2,
    # EmotionRegulation3,
    #Bodysat,
    # VENGEFUL.RUMIN,
    # retired,
    # semiretired,
    BornNZ,
    KESSLER6sum,
    HLTH.Fatigue,
    #  Rumination,
    Smoker,
    ChildrenNum,
    NWI,
    BELONG,
    SUPPORT,
    CharityDonate,
    #  GRATITUDE,
    # Volunteers,
    Hours.Work,
    HLTH.SleepHours,
    HLTH.Disability,
    Hours.Exercise,
    HoursCharity,
    #  LIFEMEANING,
    LIFESAT,
    SFHEALTH,
    SELF.CONTROL,
    SFHEALTH,
    SELF.ESTEEM,
    Respect.Self,
    #  GenCohort,
    #  Respect.Self,
   # Alcohol.Frequency,
  #  Alcohol.Intensity,
    HLTH.BMI,
    # GenCohort,
    # Euro,
    # partnerlost_job, rare
    #lost_job,
    #began_relationship,
    # SexualSatisfaction,
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living,
    #Env.SacWilling,
    #Env.SacMade,
    #  PERFECTIONISM,
    # PermeabilityIndividual,
    #  ImpermeabilityGroup
    # Emp.JobSecure,
    Env.CarbonRegs,
    Env.MotorwaySpend,
    Env.PubTransSubs,
    Env.Native.Species,
    Env.SacNorms,
    Env.SacMade,
    Env.SacWilling,
    Env.RoutineMade,
    Env.RoutineWilling,
    Env.ClimateChgReal,
    Env.ClimateChgCause,
    Env.ClimateChgConcern,
    Env.SatNZEnvironment,
    Env.Native.Species,
    Env.Possum.Control,
    Env.Eff01.ActionBelief,
    Env.Eff02.ActionFeeling
  ) %>%
  dplyr::mutate(across(!c(Id), ~ as.numeric(.x))) %>% # make factors numeric for easy of processing
 # dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  arrange(Id, Wave) %>%
  dplyr::mutate(
    Volunteers = if_else(HoursCharity == 1, 1, 0),
    # Depressed = (as.numeric(
    #   cut(
    #     KESSLER6sum,
    #     breaks = c(-Inf, 13, Inf),
    #     labels = c("0", "1"),
    #     right = FALSE
    #   )
    # ) - 1),
    # EthCat = factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    Church = if_else(Religion.Church > 8, 8, Religion.Church),
    income_log = log(Household.INC + 1),
  ) |> 
  group_by(Id) |> 
  mutate(wave = Wave - min(Wave)) |> 
  ungroup() |> 
  dplyr::select(-c(
    years,
    Religion.Church,
   # HoursCharity,
    # EthCat,
    Religious,
    # Respect.Self_lead2,
    Household.INC,
    #  org2018,
    #  not_euro,
    #  not_euro_lead2,
    # hold18,
    #   Euro,
    # Emp.WorkLifeBalance,
  #  YearMeasured,
    #HLTH.Disability_lead1,
    # org2019,
    # hold19,
    # retired,
    # semiretired,
  )
  ) %>%
  droplevels() |>
  arrange(Id) %>%
  data.frame() 


# d1 <- dfee%>%
#   filter(Wave == 1) %>%
#   select(Church, Id)
# 
# d2<- dfee%>%
#   filter(Wave == 2) %>%
#   select(Church, Id)
# 
# ma <- left_join(d1, d2, by = "Id")
# head(ma)
# 


N<- length(unique(dfee$Id))
min(dfee$NZSEI13, na.rm = TRUE)
table1::table1( ~ Church | Wave , data = dfee)



### impute amelia 
library(Amelia)
# match k6
match("NZSEI13",names(dfee)) 
nrow(dfee)

bds <- matrix(c(8, 10, 90), nrow = 1, ncol = 3) # bounds for Kessler6
nrow(dfee)
imputed_m <- amelia(
  set.seed = 0,
  dfee,
  #dataset to impute
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  # ords = "Ys",    # used for published analysis , must use numeric because values have been jittered
  # leads="Ys",
  noms = c("EthCat"),
  idvars = c("Wave", "YearMeasured"),
  logs = c("Hours.Exercise","CharityDonate","ChildrenNum", "HoursCharity"),
  #polytime = 2, # Allow polynomial?
  intercs = F,
  # too many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(dfe),
  parallel = "multicore"
)

# save
saveRDS(imputed_m, here::here("mods","rel_env_rwa_wave_5"))
##  create mids object 
library(miceadds)

acmice <- miceadds::datlist2mids( imputed_m$imputations )
a_mice<- mice::complete(acmice, action = 'long', include = TRUE)
skim(a_mice)

out_mice <- a_mice |>
  dplyr::mutate(across(
    c(Church
    ),
    ~ lead(.x, n = 1),
    .names = "{col}_lead1"
  )) %>% # make leads
  dplyr::mutate(across(
    c(
      RWA
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) |> # make leads
  dplyr::mutate(across(
    c(
      Church,
      Env.ClimateChgReal,
      Env.ClimateChgCause,
      Env.ClimateChgConcern,
      Env.SatNZEnvironment,
      Env.Native.Species,
      Env.Possum.Control,
      Env.Eff01.ActionBelief,
      Env.Eff02.ActionFeeling,
    ),
    ~ lead(.x, n = 3),
    .names = "{col}_lead3"
  )) |>
  dplyr::mutate(across(
    c(
      Env.ClimateChgReal,
      Env.ClimateChgCause,
      Env.ClimateChgConcern,
      Env.SacNorms,
      Env.SatNZEnvironment,
      Env.SacMade,
      Env.SacWilling#,
      # Env.RoutineMade, There??
      # Env.RoutineWilling There?
    ),
    ~ lead(.x, n = 4),
    .names = "{col}_lead4"
  )) |>
  dplyr::mutate(across(
    c(
      Env.ClimateChgReal,
      Env.ClimateChgCause,
      Env.ClimateChgConcern,
      Env.SatNZEnvironment,
      Env.MotorwaySpend,
      Env.PubTransSubs
    ),
    ~ lead(.x, n = 5),
    .names = "{col}_lead5"
  )) |> filter(Wave == 1) |>
  mutate(across(where(is.double), as.numeric)) %>%
  droplevels() %>%
  arrange(Id) %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) |>
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1)) |>
 # dplyr::mutate(id = as.factor(rep(1:N, 11))) |> # needed for g-comp
  dplyr::group_by(Id) |>
  mutate(Env.Eff = mean(c(Env.Eff01.ActionBelief,Env.Eff02.ActionFeeling), na.rm = TRUE)) |>
  mutate(Env.Eff_lead3 = mean(c(Env.Eff01.ActionBelief_lead3,Env.Eff02.ActionFeeling_lead3), na.rm = TRUE)) |>
  # mutate(PWI = mean(
  #   c(
  #     Your.Future.Security,
  #     Your.Personal.Relationships,
  #     Your.Health,
  #     Standard.Living
  #   ),
  #   na.rm = TRUE
  # )) |>
  ungroup() |>
  droplevels() |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z))


# # Get data into shape
aml <- out_mice %>% mutate_if(is.matrix, as.vector)
aml <- mice::as.mids(aml)
amf<- mice::complete(aml, "long", inc = FALSE)

table(amf$Church == amf$Church_lead3)

# head(aml$loggedEvents, 100)



# out_mice%>%
#   select(id,Id) %>%
#   tail()
# tali(out_mice)
# tail(amf)



# mice data prep ----------------------------------------------------------


dfe <- dfee %>% #
  dplyr::select(-c(wave, YearMeasured)) |> 
  dplyr::mutate(across(
    c(Church
    ),
    ~ lead(.x, n = 1),
    .names = "{col}_lead1"
  )) %>% # make leads
  dplyr::mutate(across(
    c(
      RWA
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) |> # make leads
  dplyr::mutate(across(
    c(
      Church,
      Env.ClimateChgReal,
      Env.ClimateChgCause,
      Env.ClimateChgConcern,
      Env.SatNZEnvironment,
      Env.Native.Species,
      Env.Possum.Control,
      Env.Eff01.ActionBelief,
      Env.Eff02.ActionFeeling,
    ),
    ~ lead(.x, n = 3),
    .names = "{col}_lead3"
  )) |>
  dplyr::mutate(across(
    c(
      Env.ClimateChgReal,
      Env.ClimateChgCause,
      Env.ClimateChgConcern,
      Env.SacNorms,
      Env.SatNZEnvironment,
      Env.SacMade,
      Env.SacWilling#,
      # Env.RoutineMade, There?? 
      # Env.RoutineWilling There? 
    ),
    ~ lead(.x, n = 4),
    .names = "{col}_lead4"
  )) |> 
  dplyr::mutate(across(
    c(
      Env.ClimateChgReal,
      Env.ClimateChgCause,
      Env.ClimateChgConcern,
      Env.SatNZEnvironment,
      Env.MotorwaySpend,
      Env.PubTransSubs
    ),
    ~ lead(.x, n = 5),
    .names = "{col}_lead5"
  )) |> filter(Wave == 2013) |> 
  mutate(across(where(is.double), as.numeric)) %>%
  droplevels() %>%
  arrange(Id)


# Filtering retirement -- consistency and positivity assumptions
# number of ids
N <- length(unique(dfe$Id))
N  # 4703

# inspect data
skim(amf)%>%
  arrange(n_missing)


# mice model  -------------------------------------------------------------
library(mice)
mice_c <- dfee %>%
  dplyr::select(-c( Wave, Id))  # won't otherwise run

library(naniar)

naniar::gg_miss_var(mice_c)
vis_miss(mice_c,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(mice_c)

# impute
env_rel_rwa_mice_5 <- mice::mice(mice_c,  seed = 0, m = 10)

# save
saveh(env_rel_rwa_mice_5, "env_rel_rwa_mice_5")

# read
c_mice <- readh("env_rel_rwa_mice_5")
c_mice <-env_rel_rwa_mice_5
# checks
outlist2 <-
  row.names(c_mice)[c_mice$outflux < 0.5]
length(outlist2)

# checks
head(c_mice$loggedEvents, 100)

# data warangling
# we create two completed data sets -- the one without the missing data will be useful for
# determing causal contrasts -- which we'll describe below.

mf <- mice::complete(c_mice, "long", inc = F)
ml <- mice::complete(c_mice, "long", inc = TRUE)

# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful

N<- length(ml$Church)/11
# create variables in z score
ml <- ml %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) |> 
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1)) |> 
  dplyr::mutate(id = as.factor(rep(1:N, 11))) |> # needed for g-comp
  dplyr::group_by(id) |> 
  mutate(Env.Eff = mean(c(Env.Eff01.ActionBelief,Env.Eff02.ActionFeeling), na.rm = TRUE)) |> 
  mutate(Env.Eff_lead3 = mean(c(Env.Eff01.ActionBelief_lead3,Env.Eff02.ActionFeeling_lead3), na.rm = TRUE)) |> 
  # mutate(PWI = mean(
  #   c(
  #     Your.Future.Security,
  #     Your.Personal.Relationships,
  #     Your.Health,
  #     Standard.Living
  #   ),
  #   na.rm = TRUE
  # )) |>
  ungroup() |> 
  droplevels() |> 
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z))


# for models wihout looping (not advised)

mf <- mf %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) |> 
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1)) |> 
  dplyr::mutate(id = as.factor(rep(1:N, 10))) |> # needed for g-comp
  dplyr::group_by(id) |> 
  mutate(Env.Eff = mean(c(Env.Eff01.ActionBelief,Env.Eff02.ActionFeeling), na.rm = TRUE)) |> 
  mutate(Env.Eff_lead3 = mean(c(Env.Eff01.ActionBelief_lead3,Env.Eff02.ActionFeeling_lead3), na.rm = TRUE)) |> 
  # mutate(PWI = mean(
  #   c(
  #     Your.Future.Security,
  #     Your.Personal.Relationships,
  #     Your.Health,
  #     Standard.Living
  #   ),
  #   na.rm = TRUE
  # )) |>
  ungroup() |> 
  droplevels() |> 
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z))

# Get data into shape
mf <- mf %>% mutate_if(is.matrix, as.vector)
ml <- ml %>% mutate_if(is.matrix, as.vector)

ml <- mice::as.mids(ml)

 # saveh(ml, "churchl")
 # saveh(mf, "churchf")
skimr::skim(mf)

skimr::skim(amf)


###### READ THIS DATA IN   #########
# ml <- readh("churchl")
# mf <- readh("churchf")


# model equations ---------------------------------------------------------


cvars = c("Env.CarbonRegs_z","Env.MotorwaySpend_z","Env.PubTransSubs_z", "Env.Native.Species_z","Env.SacNorms_z","Env.SacMade_z","Env.SacWilling_z","Env.RoutineMade_z","Env.RoutineWilling_z", "Env.Eff_z", "Env.ClimateChgReal_z", "Env.ClimateChgCause_z","Env.ClimateChgConcern_z","Env.SatNZEnvironment_z","Env.Native.Species_z", "Env.Possum.Control_z", "AGREEABLENESS_z","CONSCIENTIOUSNESS_z","EXTRAVERSION_z","HONESTY_HUMILITY_z","NEUROTICISM_z","OPENNESS_z","Age_z","BornNZ_z","BELONG_z","CharityDonate_log_z","ChildrenNum_z","Church_z","Edu_z","Employed_z","EthCat", "Hours.Exercise_log_z","Hours.Work_z","HLTH.BMI_z", "HLTH.Disability_z", "HLTH.Fatigue_z", "HLTH.SleepHours_z", "KESSLER6sum_z", "LIFESAT_z", "Male_z","NZdep_z", "NWI_z","NZSEI13_z","Parent_z","Partner_z","Pol.Orient_z", "Relid_z", "Respect.Self_z", "RWA_z",  "SDO_z", "SELF.CONTROL_z", "SELF.ESTEEM_z", "SFHEALTH_z","Smoker_z", "Standard.Living_z", "SUPPORT_z","Urban_z", "Volunteers_z", "Your.Health_z", "Your.Future.Security_z", "Your.Personal.Relationships_z")


# General set up ----------------------------------------------------------

ylim <- c(-.3,.3)
df <-  aml
is.mids(aml)
m = 10



#nrow(df$data)
# functions ---------------------------------------------------------------

# see "funs.R"

## Also use
round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# set up -------------------------------------------------------------


############################################################################
## CHURCH
###########

# CHURCH ------------------------------------------------------------------

# CHURCH here
## CHURCH set up ---------------------------------------------------------------
#How many times did you pray in the last week?
X = "Church_lead1"
xlab = "Church frequency (monthly)"
min= 0
max = 8
# baseline
r = 0
# focal contrast
f = 4
c = c(r,f)
# range of estimates
x =  min:max
p = 5


# CHURCH EFFICACY  -------------------------------------------------------------
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.Eff_lead3_z ~  bs(Church_lead1) +",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Environmental Efficacy + 3"
ylab = "Environmental Efficacy (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()
## contrasts
summary(pool(out_m))

out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# CHURCH EFFICACY BELIEVE  -------------------------------------------------------------
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.Eff01.ActionBelief_lead3_z~  bs(Church_lead1) +",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Environmental Efficacy Action Belief + 3"
ylab = "Environmental Efficacy (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = aml, m = m,  X = X, x = c, r= r)
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# CHURCH EFFICACY BELIEVE  -------------------------------------------------------------
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.Eff02.ActionFeeling_lead3_z ~  bs(Church_lead1) +",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Environmental Efficacy Action Feeling + 3"
ylab = "Environmental Efficacy (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# Climate change real ---------------------------------------------------------------------
# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.ClimateChgReal_lead3_z ~ bs(Church_lead1)+ ",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Climate Change is Real + 3"
ylab = "Climate Change is Real (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)

# evalues
round( EValue::evalues.OLS( 0.004, se = 0.018, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)



# climage change real + 4 -------------------------------------------------
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.ClimateChgReal_lead4_z ~ bs(Church_lead1)+ ",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Climate Change is Real + 3"
ylab = "Climate Change is Real (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)

# evalues
round( EValue::evalues.OLS( 0.004, se = 0.018, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)



# Climate change real ---------------------------------------------------------------------
# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.ClimateChgReal_lead4_z ~ bs(Church_lead1)+ ",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Climate Change is Real + 4"
ylab = "Climate Change is Real (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)

# evalues
round( EValue::evalues.OLS( 0.004, se = 0.018, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)



# climate change caused by humans -----------------------------------------
# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.ClimateChgCause_lead3_z ~ bs(Church_lead1)+ ",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Climate Change is Human Caused + 3"
ylab = "Climate Change is Human Caused (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# climate change caused by humans 4 -----------------------------------------
# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.ClimateChgCause_lead4_z ~ bs(Church_lead1)+ ",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Climate Change is Human Caused + 4"
ylab = "Climate Change is Human Caused (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# climate cause 5 ---------------------------------------------------------



out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.ClimateChgCause_lead5_z ~ bs(Church_lead1)+ ",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Climate Change is Human Caused + 5"
ylab = "Climate Change is Human Caused (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# Climate change concern --------------------------------------------------

# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.ClimateChgConcern_lead3_z ~ bs(Church_lead1) +",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Climate Change Concern + 3"
ylab = "Climate Change Concern (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = aml, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)

# evalues
round( EValue::evalues.OLS( -0.098, se = 0.049, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# climate concern 4 -------------------------------------------------------

# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.ClimateChgConcern_lead4_z ~ bs(Church_lead1)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Climate Change Concern: 4 years from baseline"
ylab = "Climate Change Concern (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# climate concern 5 -------------------------------------------------------

# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.ClimateChgConcern_lead5_z ~ bs(Church_lead1)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Climate Change Concern: 5 years from baseline"
ylab = "Climate Change Concern (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = c(-.3,.3), main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)




# Env.SatNZEnvironment  ---------------------------------------------------

# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.SatNZEnvironment_lead3_z ~ bs(Church_lead1)+",
                                paste(cvars,
                                      collapse = "+")))) )
}


# labels
main = "Satisfaction with NZ Environment + 3"
ylab = "Satisfaction with NZ Environment (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# possum --------------------------------------------------------------

out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.Native.Species_lead3_z ~ bs(Church_lead1)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Protect Native Species + 3"
ylab = "Protect Native Species (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)



# Env.SatNZEnvironment  ---------------------------------------------------

# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.SatNZEnvironment_lead4_z ~ bs(Church_lead1)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Satisfaction with NZ Environment + 4"
ylab = "Satisfaction with NZ Environment (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# Env.SatNZEnvironment 5 ---------------------------------------------------

# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.SatNZEnvironment_lead5_z ~ bs(Church_lead1)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Satisfaction with NZ Environment + 5"
ylab = "Satisfaction with NZ Environment (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# Native species --------------------------------------------------------------

out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.Native.Species_lead3_z ~ bs(Church_lead1)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Protect Native Species + 3"
ylab = "Protect Native Species (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# possum ------------------------------------------------------------------

# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.Possum.Control_lead3_z ~ bs(Church_lead1)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Possum Control 1080: 3 years from baseline"
ylab = "Possum Control 1080 (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)



# Env.SacNorms, -----------------------------------------------------
# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.SacNorms_lead4_z ~ bs(Church_lead1)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Env Sacrifice Norms + 4"
ylab = "Env Sacrifice Norms + 4"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# sacrifice made ----------------------------------------------------------
# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.SacMade_lead4_z ~ bs(Church_lead1)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Env Sacrifice Made + 4"
ylab = "Env Sacrifice Made + 4"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)




# sacrifice willing -------------------------------------------------------
# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.SacWilling_lead4_z ~ bs(Church_lead1)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Env Sacrifice Willing + 4"
ylab = "Env Sacrifice Willing + 4"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# motorway spend ----------------------------------------------------------
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.MotorwaySpend_lead5_z ~ bs(Church_lead1)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Motorway Spending + 5"
ylab = "Motorway Spending (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# public transport --------------------------------------------------------
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.PubTransSubs_lead5_z ~ bs(Church_lead1)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Public Transport Subsidy + 5"
ylab = "Public Transport Subsidy (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = ml, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = ml, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


#############################################################################


# RWA ---------------------------------------------------------------------

X = "RWA_lead2_z"
xlab = "RWA (SD)"
min= -2
max = 2
# baseline
r = 0
# focal contrast
f = 2
c = c(r,f)
p = 5 #position from bottom (for graphs)
# range of estimates
x =  min:max


# EFFICACY  -------------------------------------------------------------
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.Eff_lead3_z ~  (RWA_lead2_z) +",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Environmental Efficacy"
ylab = "Environmental Efficacy (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# Climate change real ---------------------------------------------------------------------
# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.ClimateChgReal_lead3_z ~ (RWA_lead2_z)+ ",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Climate Change is Real"
ylab = "Climate Change is Real (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = c(-.4,.3), main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, main, xlab, ylim=c(-.3,.3), ylab = ylab, min = min, p)

# evalues
round( EValue::evalues.OLS( , se = 0.018, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)



# climate change caused by humans -----------------------------------------
# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.ClimateChgCause_lead3_z ~ (RWA_lead2_z)+ ",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Climate Change is Human Caused"
ylab = "Climate Change is Human Caused (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# Climate change concern --------------------------------------------------

# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.ClimateChgConcern_lead3_z ~ (RWA_lead2_z) +",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Climate Change Concern"
ylab = "Climate Change Concern (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)

# evalues
round( EValue::evalues.OLS( -0.098, se = 0.049, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# Env.SatNZEnvironment  ---------------------------------------------------

# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.SatNZEnvironment_lead3_z ~ (RWA_lead2_z)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Satisfaction with NZ Environment"
ylab = "Satisfaction with NZ Environment (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# possum --------------------------------------------------------------

out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.Native.Species_lead3_z ~ (RWA_lead2_z)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Protect Native Species"
ylab = "Protect Native Species (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# 1080 possum -------------------------------------------------------------
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.Possum.Control_lead3_z ~  (RWA_lead2_z) +",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Use 1080 Possum Control"
ylab = "Use 1080 Possum Control (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)




# sacrifice made ----------------------------------------------------------
# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.SacMade_lead4_z ~ (RWA_lead2_z)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Env Sacrifice Made + 4"
ylab = "Env Sacrifice Made + 4"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)




# sacrifice willing -------------------------------------------------------
# prepare
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.SacWilling_lead4_z ~ (RWA_lead2_z)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Env Sacrifice Willing + 4"
ylab = "Env Sacrifice Willing + 4"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = c(-.3,.3), main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# motorway spend ----------------------------------------------------------
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.MotorwaySpend_lead5_z ~ (RWA_lead2_z)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Motorway Spending + 5"
ylab = "Motorway Spending (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# public transport --------------------------------------------------------
out_f = function(formula) {
 with(ml,glm(as.formula(paste("Env.PubTransSubs_lead5_z ~ (RWA_lead2_z)+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "Public Transport Subsidy + 5"
ylab = "Public Transport Subsidy (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, p=p)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

#############################################################################
#############################################################################
#############################################################################

# SCRIPTURE CAUSAL MEDIATION --------------------------------------------------------

cma_sc1 <- cmest(data = amf,
                 model = "gformula",
                 outcome = "Env.Eff_lead3_z",
                 exposure = "Church_lead1",
                 mediator = "RWA_lead2_z",
                 basec = cvars,
                 EMint = F,
                 mreg = list("linear"),
                 yreg = "linear",
                 astar = 0,
                 basecval = NULL,
                 a = 4,
                 mval = list(0),
                 estimation = "imputation",
                 inference = "boot",
                 nboot = 10, 
                 full = FALSE)

library(ggplot2)

title = "Env Efficacy + 3"
sc1 <- ggcmest(cma_sc1, errorbar.colour = c( "blue")) + #coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8)) + labs(title = title)
sc1

summary(cma_sc1, full = FALSE)
cmsens(object = cma_sc1, sens = "uc")

cma_sc2 <- cmest(data = amf,
                 model = "gformula",
                 outcome = "Env.ClimateChgReal_lead5_z",
                 exposure = "Church_lead1",
                 mediator = "RWA_lead2_z",
                 basec = cvars,
                 EMint = TRUE,
                 mreg = list("linear"),
                 yreg = "linear",
                 astar = 0,
                 basecval = NULL,
                 a = 4,
                 mval = list(0),
                 estimation = "imputation",
                 inference = "boot",
                 nboot = 10, 
                 full = FALSE)

title = "Climate Change Real + 5"
sc2 <- ggcmest(cma_sc2, errorbar.colour = c( "blue")) + #coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8)) + labs(title = title)
sc2

summary(cma_sc2, full = FALSE)
cmsens(object = cma_sc2, sens = "uc")


cma_sc3 <- cmest(data = amf,
                 model = "gformula",
                 outcome = "Env.ClimateChgCause_lead3_z",
                 exposure = "Church_lead1",
                 mediator = "RWA_lead2_z",
                 basec = cvars,
                 EMint = TRUE,
                 mreg = list("linear"),
                 yreg = "linear",
                 astar = 0,
                 basecval = NULL,
                 a = 4,
                 mval = list(0),
                 estimation = "imputation",
                 inference = "boot",
                 nboot = 20, 
                 full = FALSE)

title = "Climate Change is Caused by Humans + 3"
sc3 <- ggcmest(cma_sc3, errorbar.colour = c( "blue")) + #coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8)) + labs(title = title)
sc3

summary(cma_sc3, full = FALSE)
cmsens(object = cma_sc3, sens = "uc")

cma_sc4 <- cmest(data = amf,
                 model = "gformula",
                 outcome = "Env.ClimateChgConcern_lead3_z",
                 exposure = "Church_lead1",
                 mediator = "RWA_lead2_z",
                 basec = cvars,
                 EMint = TRUE,
                 mreg = list("linear"),
                 yreg = "linear",
                 astar = 0,
                 basecval = NULL,
                 a = 4,
                 mval = list(0),
                 estimation = "imputation",
                 inference = "boot",
                 nboot = 10, 
                 full = FALSE)

title = "Climate Change Concern + 3"
sc4 <- ggcmest(cma_sc4, errorbar.colour = c( "blue")) + #coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8)) + labs(title = title)
sc4

summary(cma_sc4, full = FALSE)
cmsens(object = cma_sc4, sens = "uc")


cma_sc5 <- cmest(data = amf,
                 model = "gformula",
                 outcome = "Env.SatNZEnvironment_lead3_z",
                 exposure = "Church_lead1",
                 mediator = "RWA_lead2_z",
                 basec = cvars,
                 EMint = TRUE,
                 mreg = list("linear"),
                 yreg = "linear",
                 astar = 0,
                 basecval = NULL,
                 a = 4,
                 mval = list(0),
                 estimation = "imputation",
                 inference = "boot",
                 nboot = 10, 
                 full = FALSE)

title = "Satisfied with New Zealand Environment + 3"
sc5 <- ggcmest(cma_sc5, errorbar.colour = c( "blue")) + #coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8)) + labs(title = title)
sc5

summary(cma_sc5, full = FALSE)
cmsens(object = cma_sc5, sens = "uc")


cma_sc6 <- cmest(data = amf,
                 model = "gformula",
                 outcome = "Env.Eff01.ActionBelief_lead3_z",
                 exposure = "Church_lead1",
                 mediator = "RWA_lead2_z",
                 basec = cvars,
                 EMint = TRUE,
                 mreg = list("linear"),
                 yreg = "linear",
                 astar = 0,
                 basecval = NULL,
                 a = 4,
                 mval = list(0),
                 estimation = "imputation",
                 inference = "boot",
                 nboot = 10, 
                 full = FALSE)

title = "Efficacy Belief"
sc6 <- ggcmest(cma_sc6, errorbar.colour = c( "blue")) + #coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8)) + labs(title = title)
sc6

summary(cma_sc6, full = FALSE)
cmsens(object = cma_sc6, sens = "uc")


cma_sc7 <- cmest(data = amf,
                 model = "gformula",
                 outcome = "Env.Eff02.ActionFeeling_lead3_z",
                 exposure = "Church_lead1",
                 mediator = "RWA_lead2_z",
                 basec = cvars,
                 EMint = TRUE,
                 mreg = list("linear"),
                 yreg = "linear",
                 astar = 0,
                 basecval = NULL,
                 a = 4,
                 mval = list(0),
                 estimation = "imputation",
                 inference = "boot",
                 nboot = 10, 
                 full = FALSE)

title = "Efficacy Feeling + 3"
sc7 <- ggcmest(cma_sc7, errorbar.colour = c( "blue")) + #coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8)) + labs(title = title)
sc7

summary(cma_sc7, full = FALSE)
cmsens(object = cma_sc7, sens = "uc")


cma_sc8 <- cmest(data = amf,
                 model = "gformula",
                 outcome = "Env.Native.Species_lead3_z",
                 exposure = "Church_lead1",
                 mediator = "RWA_lead2_z",
                 basec = cvars,
                 EMint = TRUE,
                 mreg = list("linear"),
                 yreg = "linear",
                 astar = 0,
                 basecval = NULL,
                 a = 4,
                 mval = list(0),
                 estimation = "imputation",
                 inference = "boot",
                 nboot = 10, 
                 full = FALSE)

title = "Protect Native Species + 3"
sc8 <- ggcmest(cma_sc8, errorbar.colour = c( "blue")) + #coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8)) + labs(title = title)
sc8

summary(sc8, full = FALSE)
cmsens(object = sc8, sens = "uc")




cma_s9 <- cmest(data = amf,
                 model = "gformula",
                 outcome = "Env.Possum.Control_lead3_z",
                 exposure = "Church_lead1",
                 mediator = "RWA_lead2_z",
                 basec = cvars,
                 EMint = TRUE,
                 mreg = list("linear"),
                 yreg = "linear",
                 astar = 0,
                 basecval = NULL,
                 a = 4,
                 mval = list(0),
                 estimation = "imputation",
                 inference = "boot",
                 nboot = 10, 
                 full = FALSE)

title = "Possum Control 1080 + 4"
s9 <- ggcmest(cma_s9, errorbar.colour = c( "blue")) + #coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8)) + labs(title = title)
s9

summary(cma_s9, full = FALSE)
cmsens(object = cma_s9, sens = "uc")



cma_s10 <- cmest(data = amf,
                model = "gformula",
                outcome = "Env.SacNorms_lead4_z",
                exposure = "Church_lead1",
                mediator = "RWA_lead2_z",
                basec = cvars,
                EMint = TRUE,
                mreg = list("linear"),
                yreg = "linear",
                astar = 0,
                basecval = NULL,
                a = 4,
                mval = list(0),
                estimation = "imputation",
                inference = "boot",
                nboot = 10, 
                full = FALSE)

title = "Perceived acrificial Norms + 4"
s10 <- ggcmest(cma_s10, errorbar.colour = c( "blue")) + #coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8)) + labs(title = title)
s10

summary(cma_s10, full = FALSE)
cmsens(object = cma_s10, sens = "uc")



cma_s11 <- cmest(data = amf,
                 model = "gformula",
                 outcome = "Env.SacWilling_lead4_z",
                 exposure = "Church_lead1",
                 mediator = "RWA_lead2_z",
                 basec = cvars,
                 EMint = TRUE,
                 mreg = list("linear"),
                 yreg = "linear",
                 astar = 0,
                 basecval = NULL,
                 a = 4,
                 mval = list(0),
                 estimation = "imputation",
                 inference = "boot",
                 nboot = 100, 
                 full = FALSE)

title = "Sacrifice Willing + 4"
s11 <- ggcmest(cma_s11, errorbar.colour = c( "blue")) + #coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8)) + labs(title = title)
s11

summary(cma_s11, full = FALSE)
cmsens(object = cma_s11, sens = "uc")


cma_s12 <- cmest(data = amf,
                 model = "gformula",
                 outcome = "Env.SacMade_lead4_z",
                 exposure = "Church_lead1",
                 mediator = "RWA_lead2_z",
                 basec = cvars,
                 EMint = TRUE,
                 mreg = list("linear"),
                 yreg = "linear",
                 astar = 0,
                 basecval = NULL,
                 a = 4,
                 mval = list(0),
                 estimation = "imputation",
                 inference = "boot",
                 nboot = 100, 
                 full = FALSE)

title = "Sacrifice Made + 4"
s12<- ggcmest(cma_s12, errorbar.colour = c( "blue")) + #coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8)) + labs(title = title)
s12

summary(cma_s12, full = FALSE)
cmsens(object = cma_s12, sens = "uc")

cma_s13 <- cmest(data = amf,
                 model = "gformula",
                 outcome = "Env.MotorwaySpend_lead5_z",
                 exposure = "Church_lead1",
                 mediator = "RWA_lead2_z",
                 basec = cvars,
                 EMint = TRUE,
                 mreg = list("linear"),
                 yreg = "linear",
                 astar = 0,
                 basecval = NULL,
                 a = 4,
                 mval = list(0),
                 estimation = "imputation",
                 inference = "boot",
                 nboot = 100, 
                 full = FALSE)

title = "Public Spending Motorway + 5"
s13<- ggcmest(cma_s13, errorbar.colour = c( "blue")) + #coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8)) + labs(title = title)
s13

summary(cma_s13, full = FALSE)
cmsens(object = cma_s13, sens = "uc")


cma_s14 <- cmest(data = amf,
                 model = "gformula",
                 outcome = "Env.PubTransSubs_lead5_z",
                 exposure = "Church_lead1",
                 mediator = "RWA_lead2_z",
                 basec = cvars,
                 EMint = TRUE,
                 mreg = list("linear"),
                 yreg = "linear",
                 astar = 0,
                 basecval = NULL,
                 a = 4,
                 mval = list(0),
                 estimation = "imputation",
                 inference = "boot",
                 nboot = 10, 
                 full = FALSE)

title = "Government Spending Public Transport + 5"
s14<- ggcmest(cma_s14, errorbar.colour = c( "blue")) + #coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8)) + labs(title = title)
s14

summary(cma_s14, full = FALSE)
cmsens(object = cma_s14, sens = "uc")



