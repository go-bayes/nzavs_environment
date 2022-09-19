# Religion environment  INDIVIDUAL OPTIMISM 

# Religion enviornment 

# church-use R
# set digits = 3
options(scipen=999)

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
tab_in <- dat %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0)) %>%
  dplyr::mutate(Religious = as.numeric(Religious)-1) %>%
  dplyr::rename(Prayer = Religion.Prayer2,
                Scripture = Religion.Scripture2,
                Env.Native.Species = Env.NATIVE.SPECIES) |> 
  dplyr::filter((Wave == 2016  & YearMeasured  == 1) |
                  (Wave == 2017  &
                     YearMeasured  == 1) |
                  (Wave == 2018 & YearMeasured  != -1)|
                  (Wave == 2019 & YearMeasured  != -1)|
                  (Wave == 2020 & YearMeasured  != -1))  %>% # Eligibility criteria
  # dplyr::filter(Id != 9630) %>% # problematic for income
  group_by(Id) %>%
  dplyr::mutate(org1 =  ifelse(Wave == 2016 &
                                 YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold1 = mean(org1, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold1 > 0) %>%
  # dplyr::mutate(org2 = ifelse(Wave == 2017&
  #                               YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  #   dplyr::mutate(hold2 = mean(org2, na.rm = TRUE)) %>%  # Hack0
  # dplyr::filter(hold2 > 0) %>% #
  ungroup() %>%
  droplevels() %>%
  arrange(Id, Wave)
# check n 

# dplyr::mutate(org4 = ifelse(Wave == 2019 &
#                               YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
#   dplyr::mutate(hold4 = mean(org4, na.rm = TRUE)) %>%  # Hack0
#   dplyr::filter(hold4 > 0) %>% # 
# 
# length(unique(tab_in$Id)) # 15786
# 
# table1::table1(~   Religious | Wave , data = tab_in, overall = FALSE)
# 

table1::table1(~ factor(Religious)| Wave, data = tab_in)

# increasing rate
tab_in%>%
  group_by(Wave) %>%
  summarise(mean = mean(Env.ClimateChgCause, na.rm = TRUE),
            sd = sd(Env.ClimateChgCause, na.rm = TRUE))

dat %>%
  group_by(Wave) %>%
  summarise(mean = mean(Env.ClimateChgCause, na.rm = TRUE),
            sd = sd(Env.ClimateChgCause, na.rm = TRUE)) %>%
  slice(8:14)

# # Variables
# dat$Env.Eff01.ActionBelief
# dat$Env.Eff02.ActionFeeling

## select vars
df_er <- tab_in %>%
  # dplyr::filter(Id != 9630) %>% # problematic
  select(
    Id,
    YearMeasured,
    Wave,
    Partner,
    Euro,
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
    Religion.Church2,
    Religious,
    Prayer,
    Scripture,
    Christian_nfd,
    # Believe.Spirit,
    #  Believe.God,
    #  Spiritual.Identification,
    SWB.SoC01,
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
    HoursCharity,
    #  GRATITUDE,
    # Volunteers,
    Hours.Work,
    HLTH.SleepHours,
    HLTH.Disability,
    Hours.Exercise,
    #  LIFEMEANING,
    LIFESAT,
    SFHEALTH,
    SELF.CONTROL,
    SFHEALTH,
    SELF.ESTEEM,
    Respect.Self,
    #  GenCohort,
    #  Respect.Self,
    Alcohol.Frequency,
    Alcohol.Intensity,
    HLTH.BMI,
    # GenCohort,
    # Euro,
    # partnerlost_job, rare
    #lost_job,
    #began_relationship,
    Alcohol.Intensity,
    Alcohol.Frequency,
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
    Env.ClimateChgReal,
    Env.ClimateChgCause,
    Env.ClimateChgConcern,
    Env.SatNZEnvironment,
    Env.Native.Species,
    Env.Possum.Control,
    Env.Eff01.ActionBelief,
    Env.Eff02.ActionFeeling,
  ) %>%
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of processing
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
    Church = ifelse(Religion.Church2 > 8, 8, Religion.Church2),
    income_log = log(Household.INC + 1),
  ) %>%
  arrange(Id, Wave)  %>% #
  dplyr::mutate(across(
    c(Church,
      Prayer,
      Scripture,
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    ~ lead(.x, n = 1),
    .names = "{col}_lead1"
  )) %>% # make leads
  dplyr::mutate(across(
    c(
      RWA,
      Env.ClimateChgReal,
      Env.ClimateChgCause,
      Env.ClimateChgConcern,
      Env.SatNZEnvironment,
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::mutate(across(
    c(
      Env.ClimateChgReal,
      Env.ClimateChgCause,
      Env.ClimateChgConcern,
      Env.SatNZEnvironment,
      Env.Native.Species,
      Env.Possum.Control,
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    ~ lead(.x, n = 3),
    .names = "{col}_lead3"
  )) %>% # make leads
  dplyr::mutate(across(
    c(
      Env.ClimateChgReal,
      Env.ClimateChgCause,
      Env.ClimateChgConcern,
      Env.SatNZEnvironment,
      Env.Eff01.ActionBelief,
      Env.Eff02.ActionFeeling,
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    ~ lead(.x, n = 4),
    .names = "{col}_lead4"
  )) %>% # make leads
  dplyr::filter(Wave == 2016) %>%
  dplyr::filter(Religious == 1) %>%
  dplyr::filter(!is.na(Scripture)) %>%
  dplyr::filter(!is.na(Church)) %>%
  dplyr::filter(!is.na(Prayer)) %>%
  dplyr::filter(!is.na(Scripture_lead1)) %>%
  dplyr::filter(!is.na(Church_lead1)) %>%
  dplyr::filter(!is.na(Prayer_lead1)) %>%
  dplyr::select(-c(
    Religion.Church2,
    # EthCat,
    HoursCharity,
    # Respect.Self_lead2,
    Household.INC,
    #  org2018,
    #  not_euro,
    #  not_euro_lead2,
    # hold18,
    #   Euro,
    # Emp.WorkLifeBalance,
    YearMeasured,
    #HLTH.Disability_lead1,
    # org2019,
    # hold19,
    # retired,
    # semiretired,
  )
  ) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing-- do this after imputation
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)


# Filtering retirement -- consistency and positivity assumptions

# number of ids
N <- length(unique(df_er$Id))
N  # 6901

# inspect data
skim(df_er)



# mice model  -------------------------------------------------------------
library(mice)

mice_c <- df_er %>%
  dplyr::select(-c( Wave, Id))  # won't otherwise run

library(naniar)
naniar::gg_miss_var(mice_c)
vis_miss(mice_c,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(mice_c)

# impute
env_rel_rwa_mice <- mice::mice(mice_c,  seed = 0, m = 10)

# save
saveh(env_rel_rwa_mice, "env_rel_rwa_mice")

# read
c_mice <- readh("env_rel_rwa_mice")
c_mice <-env_rel_rwa_mice
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
skimr::skim(mf)

# create variables in z score
ml <- ml %>%
  dplyr::mutate(Scripture_lead1_sqrt = sqrt(Scripture_lead1)) |> 
  dplyr::mutate(Prayer_lead1_sqrt = sqrt(Prayer_lead1)) |> 
  dplyr::mutate(Scripture_sqrt = sqrt(Scripture)) |> 
  dplyr::mutate(Prayer_sqrt = sqrt(Prayer)) |> 
  dplyr::mutate(Scripture_bin = if_else(Scripture> 1, 1, 0)) |> 
  dplyr::mutate(Scripture_lead1_bin = if_else(Scripture_lead1 > 1, 1, 0)) |> 
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) |> 
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity + 1)) |> 
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1)) |> 
  dplyr::mutate(id = as.factor(rep(1:N, 11))) |> # needed for g-comp
  dplyr::group_by(id) |> mutate(Env.Eff = mean(c(Env.Eff01.ActionBelief,Env.Eff02.ActionFeeling), na.rm = TRUE)) |> 
  dplyr::group_by(id) |> mutate(Env.Eff_lead4 = mean(c(Env.Eff01.ActionBelief_lead4,Env.Eff02.ActionFeeling_lead4), na.rm = TRUE)) |> 
  ungroup() |> 
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z))


# for models wihout looping (not advised)

mf <- mf %>%
  dplyr::mutate(Scripture_lead1_sqrt = sqrt(Scripture_lead1)) |> 
  dplyr::mutate(Prayer_lead1_sqrt = sqrt(Prayer_lead1)) |> 
  dplyr::mutate(Prayer_sqrt = sqrt(Prayer)) |> 
  dplyr::mutate(Scripture_bin = if_else(Scripture > 1, 1, 0)) |> 
  dplyr::mutate(Scripture_lead1_bin = if_else(Scripture_lead1 > 1, 1, 0)) |> 
  dplyr::mutate(Scripture_sqrt = sqrt(Scripture)) |> 
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) |> 
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity + 1)) |> 
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1)) |> 
  dplyr::mutate(id = as.factor(rep(1:N, 10))) |> # needed for g-comp
  dplyr::group_by(id) |> mutate(Env.Eff = mean(c(Env.Eff01.ActionBelief,Env.Eff02.ActionFeeling), na.rm = TRUE)) |> 
  dplyr::group_by(id) |> mutate(Env.Eff_lead4 = mean(c(Env.Eff01.ActionBelief_lead4,Env.Eff02.ActionFeeling_lead4), na.rm = TRUE)) |>
  dplyr::group_by(id) |> mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  )) |> 
  dplyr::group_by(id) |> mutate(PWI_lead1 = mean(
    c(
      Your.Future.Security_lead1,
      Your.Personal.Relationships_lead1,
      Your.Health_lead1,
      Standard.Living_lead1
    ),
    na.rm = TRUE
  )) |> 
  dplyr::group_by(id) |> mutate(PWI_lead2 = mean(
    c(
      Your.Future.Security_lead2,
      Your.Personal.Relationships_lead2,
      Your.Health_lead2,
      Standard.Living_lead2
    ),
    na.rm = TRUE
  )) |> 
  dplyr::group_by(id) |> mutate(PWI_lead3 = mean(
    c(
      Your.Future.Security_lead3,
      Your.Personal.Relationships_lead3,
      Your.Health_lead3,
      Standard.Living_lead3
    ),
    na.rm = TRUE
  )) |> 
  ungroup() |> 
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z))

# Get data into shape
mf <- mf %>% mutate_if(is.matrix, as.vector)
ml <- ml %>% mutate_if(is.matrix, as.vector)

ml <- mice::as.mids(ml)

# saveh(ml, "churchl")
# saveh(mf, "churchf")

hist(log(mf$Scripture_lead1+1))
hist(mf$Scripture_lead1_bin)


###### READ THIS DATA IN   #########
# ml <- readh("churchl")
# mf <- readh("churchf")


# model equations ---------------------------------------------------------

cvars = c("Env.Eff", "Env.ClimateChgReal", "Env.ClimateChgCause","Env.ClimateChgConcern","Env.SatNZEnvironment","Env.Native.Species", "Env.Possum.Control", "AGREEABLENESS_z","CONSCIENTIOUSNESS_z","EXTRAVERSION_z","HONESTY_HUMILITY_z","NEUROTICISM_z","OPENNESS_z","Age_z","Alcohol.Frequency_z","Alcohol.Intensity_log_z","BornNZ_z","BELONG_z","CharityDonate_log_z","ChildrenNum_z","Church_z", "community","Edu_z","Employed_z","Euro_z", "Hours.Exercise_log_z","Hours.Work_z","HLTH.BMI_z", "HLTH.Disability_z", "HLTH.Fatigue_z", "HLTH.SleepHours_z", "KESSLER6sum_z", "LIFESAT_z", "Male_z","NZdep_z", "NWI_z","NZSEI13_z","Parent_z","Partner_z","Pol.Orient_z", "Prayer_sqrt_z", "Relid_z", "Respect.Self_z", "RWA_z", "Scripture_sqrt_z", "SDO_z", "SELF.CONTROL_z", "SELF.ESTEEM_z", "SFHEALTH_z","Smoker_z", "Standard.Living_z", "SUPPORT_z","Urban_z", "Volunteers_z", "Your.Health_z", "Your.Future.Security_z", "Your.Personal.Relationships_z")



# functions ---------------------------------------------------------------

# see "funs.R"

## Also use
round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# set up -------------------------------------------------------------


# Scripture set up ---------------------------------------------------------------
#How many times did you pray in the last week?
ylim <- c(-.5,.5)
df <-  ml
m = 10
X = "Scripture_lead1_sqrt"
xlab = "Scripture (sqrt)"
min= 0
max = 6
# baseline
r = 0
# focal contrast
f = 4
c = c(r,f)
# range of estimates
x =  min:max



#church set up -------------------------------------------------------------
# 
# ylim <- c(-.35,.35)
# df <-  ml
# m = 10
# X = "Church_lead1"
# xlab = "Church"
# min= 0
# max = 8
# # baseline
# r = 0
# # focal contrast
# f = 4
# c = c(r,f)
# # range of estimates
# x =  min:max


# PRAYER set up---------------------------------------------------------------
# 
# 
# ylim <- c(-.5,.5)
# df <-  ml
# m = 10
# X = "Prayer_lead1_sqrt"
# xlab = "Prayer frequency (SQRT)"
# min= 0
# max = 8
# # baseline
# r = 0
# # focal contrast
# f = 4
# c = c(r,f)
# # range of estimates
# x =  min:max


# RWA set up---------------------------------------------------------------
# ylim <- c(-.25,.25)
# ylab <- "RWA (SD)"
# df <-  ml
# m = 10
# X = ""
# xlab = "Church"
# min= 0
# max = 8
# # baseline
# r = 0
# # focal contrast
# f = 4
# c = c(r,f)
# # range of estimates
# x =  min:max





# Climate change real ---------------------------------------------------------------------
# prepare
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Env.ClimateChgReal_lead2_z ~ bs(Scripture_lead1_sqrt)+ ",
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
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)



# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, r = r, f = f)

# evalues
round( EValue::evalues.OLS( 0.004, se = 0.018, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)



# climate change caused by humans -----------------------------------------

# prepare
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Env.ClimateChgCause_lead2_z ~ bs(Scripture_lead1_sqrt)+ ",
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

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

out_ct

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, r = r, f = f)

# evalues
round( EValue::evalues.OLS( 0.004, se = 0.018, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# Climate change concern --------------------------------------------------

# prepare
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Env.ClimateChgConcern_lead2_z ~ bs(Scripture_lead1_sqrt) +",
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

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)


out_ct

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, r = r, f = f)

# evalues
round( EValue::evalues.OLS(  -0.135, se = 0.072, sd = 1, delta = 5, true = 0), 3)

#round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)



# Env.SatNZEnvironment  ---------------------------------------------------

# prepare
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Env.SatNZEnvironment_lead2_z ~ bs(Scripture_lead1_sqrt)+",
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

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

out_ct

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, r = r, f = f)

# evalues
round( EValue::evalues.OLS(  -0.135, se = 0.072, sd = 1, delta = 5, true = 0), 3)

#round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# possum --------------------------------------------------------------

out_f = function(formula) {
  with(ml, glm(as.formula(paste("Env.Native.Species_lead3_z ~ bs(Scripture_lead1_sqrt)+",
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

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

out_ct

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, r = r, f = f)

# evalues
round( EValue::evalues.OLS(  -0.135, se = 0.072, sd = 1, delta = 5, true = 0), 3)



# 1080 possum -------------------------------------------------------------

out_f = function(formula) {
  with(ml, glm(as.formula(paste("Env.Possum.Control_lead3_z ~  bs(Scripture_lead1_sqrt) +",
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

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

out_ct

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, r = r, f = f)

# evalues
round( EValue::evalues.OLS(  -0.135, se = 0.072, sd = 1, delta = 5, true = 0), 3)




# Climate change concern --------------------------------------------------

# prepare
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Env.ClimateChgConcern_lead3_z ~ bs(Scripture_lead1_sqrt)+",
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

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

out_ct

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, r = r, f = f)

# evalues
round( EValue::evalues.OLS(  -0.135, se = 0.072, sd = 1, delta = 5, true = 0), 3)

#round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


### PRAYER

# Climate change real ---------------------------------------------------------------------

# prepare
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Env.ClimateChgReal_lead2_z ~ Prayer_lead1_sqrt + ",
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
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

out_ct

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)



# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, r = r, f = f)

# evalues
round( EValue::evalues.OLS( 0.004, se = 0.018, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)



# climate change caused by humans -----------------------------------------

# prepare
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Env.ClimateChgCause_lead2_z ~ Prayer_lead1_sqrt +",
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

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

out_ct

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, r = r, f = f)

# evalues
round( EValue::evalues.OLS( 0.004, se = 0.018, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# Climate change concern --------------------------------------------------

# prepare
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Env.ClimateChgConcern_lead2_z ~ Prayer_lead1_sqrt +",
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

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

out_ct


# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, r = r, f = f)

# evalues
round( EValue::evalues.OLS(  -0.135, se = 0.072, sd = 1, delta = 5, true = 0), 3)

#round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)



# Env.SatNZEnvironment  ---------------------------------------------------

# prepare
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Env.SatNZEnvironment_lead2_z ~ Prayer_lead1_sqrt+",
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

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

out_ct

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, r = r, f = f)

# evalues
round( EValue::evalues.OLS(  -0.135, se = 0.072, sd = 1, delta = 5, true = 0), 3)

#round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# possum --------------------------------------------------------------

out_f = function(formula) {
  with(ml, glm(as.formula(paste("Env.Native.Species_lead3_z ~ Prayer_lead1_sqrt+",
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

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

out_ct

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, r = r, f = f)

# evalues
round( EValue::evalues.OLS(  -0.135, se = 0.072, sd = 1, delta = 5, true = 0), 3)



# 1080 possum -------------------------------------------------------------

out_f = function(formula) {
  with(ml, glm(as.formula(paste("Env.Possum.Control_lead3_z ~ Prayer_lead1_sqrt+",
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

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

out_ct

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, r = r, f = f)

# evalues
round( EValue::evalues.OLS(  -0.135, se = 0.072, sd = 1, delta = 5, true = 0), 3)




# Climate change concern --------------------------------------------------

# prepare
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Env.ClimateChgConcern_lead3_z ~ Prayer_lead1_sqrt+",
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

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

out_ct

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, r = r, f = f)

# evalues
round( EValue::evalues.OLS(  -0.135, se = 0.072, sd = 1, delta = 5, true = 0), 3)

#round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


###############################################################################
# test direct effects lead 4 ----------------------------------------------

ylim <- c(-1,1)
xlab <- "Scripture Frequency (SD)"
df <-  ml
m = 10
X = "Scripture_lead1_sqrt"
min = 0
max = 10
x =  min:max1
xlab = "Prayer (Sqrt/z)"
# reference level
r = 0
C = 5
c = c(r,C)

# prepare
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Env.Eff_lead4_z ~ Scripture_lead1_sqrt+",
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

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

out_ct

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, r = r, f = f)

# evalues
round( EValue::evalues.OLS(  -0.135, se = 0.072, sd = 1, delta = 5, true = 0), 3)

#round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# replicate scripture on RWA ----------------------------------------------

mf$RWA_lead2_z
# prepare
out_f = function(formula) {
  with(ml, glm(as.formula(paste("RWA_lead2_z ~ Scripture_lead1_sqrt+",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "RWA"
ylab = "RWA (SD)"

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
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

out_ct

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, r = r, f = f)

# evalues
round( EValue::evalues.OLS(  -0.135, se = 0.072, sd = 1, delta = 5, true = 0), 3)

#round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)




###############################################################################

# test effect RWA efficacy / climate ----------------------------------------------

ylim <- c(-.25,.25)
ylab <- "RWA (SD)"
df <-  ml
m = 10
X = "RWA_lead2_z"
x =  -2:2
xlab = "RWA (SD)"
# focal contrast
C = 2
# lower reference level
r = 0
c = c(r,C)




# prepare
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Env.Eff_lead4_z ~ bs(RWA_lead2_z) +",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "RWA"
ylab = "RWA (SD)"

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
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

out_ct

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, min = min, r = r, f = f)

# evalues
round( EValue::evalues.OLS(  -0.135, se = 0.072, sd = 1, delta = 5, true = 0), 3)

#round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# prepare
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Env.ClimateChgConcern_lead3_z ~ bs(RWA_lead2_z) +",
                                paste(cvars,
                                      collapse = "+")))) )
}

# labels
main = "RWA on Env Concern"
ylab = "RWA (SD)"

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
ggplot_stglm_contrast(out_ct, ylim = c(-.35,.35), main, xlab, ylab)

out_ct

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, C=C+1)

# evalues
round( EValue::evalues.OLS(  -0.135, se = 0.072, sd = 1, delta = 5, true = 0), 3)

#round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)



#############################################################################
#############################################################################
#############################################################################

# causal mediation --------------------------------------------------------


cma_sc1 <- cmest(data = mf,
                 model = "gformula",
                 outcome = "Env.Eff_lead4",
                 exposure = "Scripture_lead1_sqrt",
                 mediator = "Your.Future.Security_lead2_z",
                 basec = cvars,
                 EMint = TRUE,
                 mreg = list("linear"),
                 yreg = "linear",
                 astar = 0,
                 basecval = NULL,
                 a = 3,
                 mval = list(0),
                 estimation = "imputation",
                 inference = "boot",
                 nboot = 10, 
                 full = FALSE)

library(ggplot2)

# 
sc1 <- ggcmest(cma_sc1, errorbar.colour = c( "blue")) + #coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8))
sc1

summary(cma_sc1, full = FALSE)
cmsens(object = cma_sc1, sens = "uc")

cma_sc2 <- cmest(data = mf,
                 model = "gformula",
                 outcome = "Env.ClimateChgConcern_lead3_z",
                 exposure = "Scripture_lead1_sqrt_z",
                 mediator = "Your.Future.Security_lead2_z",
                 basec = cvars,
                 EMint = TRUE,
                 mreg = list("linear"),
                 yreg = "linear",
                 astar = 0,
                 basecval = NULL,
                 a = 3,
                 mval = list(0),
                 estimation = "imputation",
                 inference = "boot",
                 nboot = 10, 
                 full = FALSE)

library(ggplot2)

summary(cma_sc2)

sc2 <- ggcmest(cma_sc2) +   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8))
cmsens(object = cma_ch2, sens = "uc")

sc2


#

# CHURCH MEDIATION --------------------------------------------------------


# mediation climate concern -----------------------------------------------

cma_c1 <- cmest(data = mf,
                model = "gformula",
                outcome = "Env.Eff_lead4",
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

library(ggplot2)

# saveh(cma_c1, "environment-rwa-mediator-religion_church-eff")

# 
ch1 <- ggcmest(cma_c1, errorbar.colour = c( "blue")) + #coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8))
ch1

summary(cma_c1, full = FALSE)
cmsens(object = cma1, sens = "uc")



# mediation climate concern -----------------------------------------------
rm(cma2)
cma_ch2 <- cmest(data = mf,
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
                 nboot = 100, 
                 full = FALSE)

library(ggplot2)

saveh(cma_ch2, "environment-rwa-mediator-religion_church-concern")

summary(cma_ch2)

ch2 <- ggcmest(cma_ch2) +   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8))
ch2
cmsens(object = cma_ch2, sens = "uc")


# scripture mediation -----------------------------------------------------



cma_pr1 <- cmest(data = mf,
                 model = "gformula",
                 outcome = "Env.Eff_lead4",
                 exposure = "Prayer_lead1_sqrt",
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

library(ggplot2)
# 
pr1  <- ggcmest(cma_pr1, errorbar.colour = c( "blue")) + #coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8))

pr1

summary(cma_pr1, full = FALSE)
cmsens(object = cma_pr1, sens = "uc")



# mediation climate concern -----------------------------------------------
cma_pr2 <- cmest(data = mf,
                 model = "gformula",
                 outcome = "Env.ClimateChgConcern_lead3_z",
                 exposure = "Prayer_lead1_sqrt",
                 mediator = "RWA_lead2_z",
                 basec = cvars,
                 EMint = TRUE,
                 mreg = list("linear"),
                 yreg = "linear",
                 astar = 0,
                 basecval = NULL,
                 a = 5,
                 mval = list(0),
                 estimation = "imputation",
                 inference = "boot",
                 nboot = 10, 
                 full = FALSE)

library(ggplot2)

summary(cma_pr2)

pr2 <- ggcmest(cma_pr2) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8))
cmsens(object = cma_ch2, sens = "uc")
pr2

# sc2
# pr2 + ch2
# pr1 + ch1



# scripture concern -------------------------------------------------------

cma_ch2 <- cmest(data = mf,
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
                 nboot = 100, 
                 full = FALSE)

library(ggplot2)

saveh(cma_ch2, "environment-rwa-mediator-religion_church-concern")

summary(cma_ch2)

ggcmest(cma_ch2) +   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.8))
cmsens(object = cma_ch2, sens = "uc")


