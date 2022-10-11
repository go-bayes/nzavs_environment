# imputation template

# set up digits
options(scipen = 999)
# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

# for original NZAVS data -- need to contact Chris Sibley for access

conflict_prefer("lag", "dplyr")
# for saving models
push_mods <-
  fs::path_expand("~/The\ Virtues\ Project\ Dropbox/outcomewide/mods")
push_figs <-
  fs::path_expand("~/Users/joseph/The\ Virtues\ Project\ Dropbox/outcomewide/figs")


# read data
pull_path <-
  fs::path_expand(
    "~/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/ldf.5"
  )

dt <- readRDS(pull_path)

# Worked example selecting waves 2018 -- 2020 with exposure year as 2019

# A poverty of measures means that we can only work with wave 11 for now
dt_init <- dt %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0),
                SexualOrientation = as.factor(if_else(
                  SexualOrientationL1 == 1,
                  "Heterosexual",
                  if_else(SexualOrientationL1 ==
                            2, "Homosexual", "OtherSexuality")
                ))) %>%
  dplyr::mutate(Gender3 = as.factor(ifelse(
    GendAll == 0,
    "Female",
    if_else(GendAll == 1, "Male", "GenderDiverse")
  ))) %>%
  dplyr::rename(
    kessler_hopeless = SWB.Kessler01,
    # …  you feel hopeless?
    kessler_depressed = SWB.Kessler02,
    #…  you feel so depressed that nothing could cheer you up?
    kessler_restless  = SWB.Kessler03,
    #…  you feel restless or fidgety?
    kessler_effort = SWB.Kessler04,
    #…  you feel that everything was an effort?
    kessler_worthless = SWB.Kessler05,
    #…  you feel worthless?
    kessler_nervous = SWB.Kessler06 #…  you feel nervous?
  ) |>
  dplyr::mutate(Religious = as.numeric(Religious) - 1) %>%
  dplyr::rename(
    Prayer = Religion.Prayer2,
    Scripture = Religion.Scripture2,
    Church = Religion.Church2,
    Env.Native.Species = Env.NATIVE.SPECIES,
    NeighbourhoodCommunity = SWB.SoC01
  ) |>
  # dplyr::filter(Wave == 2019 &  YearMeasured  == 1) |> 
  dplyr::filter((Wave == 2018 &
                   YearMeasured  == 1) |
                  (Wave == 2019 & YearMeasured == 1)|
                  (Wave == 2020 & YearMeasured != -1)) |>
  #  dplyr::filter(Id != 9630) %>% # problematic for income
  group_by(Id) %>%
  dplyr::mutate(org2 = ifelse(Wave == 2018 &
                                YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold2 = mean(org2, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold2 > 0) %>% #
  dplyr::mutate(org1 =  ifelse(Wave == 2019 &
                                 YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold1 = mean(org1, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold1 > 0) %>%
  ungroup() %>%
  droplevels() %>%
  arrange(Id, Wave)

# check n # 34782
length(unique(dt_init$Id)) # 34783

`# how is something changing? (Attrition of disabled people?)
# Do you have a health condition or disability that limits you, and that has lasted for 6+ months?

dt_init %>%
  group_by(Wave) %>%
  summarise(mean(COVID19.Timeline, na.rm = TRUE))
dt_init$LIFEMEANING
# select-variables  -------------------------------------------------------
dt_formice <- dt_init %>%
  select(
    Id,
    YearMeasured,
    Wave,
    REGC_2018,
    Partner,
    EthCat,
    Age,
    Gender3,
    # SexualOrientation,
    NZSEI13,
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    Edu,
    NZdep,
    Employed,
    # HomeOwner,
    Pol.Orient,
    #  SDO,
    #  RWA,
    Urban,
    Household.INC,
    Parent,
    Relid,
    # Religious,
    Church,
    Prayer,
    Scripture,
    Believe.Spirit,
    Believe.God,
    #  Spiritual.Identification,
    NeighbourhoodCommunity,
    # EmotionRegulation1,
    # EmotionRegulation2,
    # EmotionRegulation3,
    Bodysat,
    VENGEFUL.RUMIN,
    retired,
    semiretired,
    BornNZ,
    KESSLER6sum,
    HLTH.Fatigue,
    Rumination,
    Smoker,
    ChildrenNum,
    NWI,
    BELONG,
    SUPPORT,
    CharityDonate,
    HoursCharity,
    GRATITUDE,
    Hours.Work,
    HLTH.SleepHours,
    HLTH.Disability,
    Hours.Exercise,
    LIFEMEANING,
    LIFESAT,
    # PWI,  ##  we use the individual
    NWI,
    SFHEALTH,
    SELF.CONTROL,
    SFHEALTH,
    SELF.ESTEEM,
    Respect.Self,
    #  GenCohort,
    #   Emp.WorkLifeBalance,
    Alcohol.Frequency,
    Alcohol.Intensity,
    HLTH.BMI,
    Smoker,
    ChildrenNum,
    # GenCohort,
    partnerlost_job,
    lost_job,
    began_relationship,
    SexualSatisfaction,
    POWERDEPENDENCE1,
    POWERDEPENDENCE2,
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living,
    PERFECTIONISM,
    PermeabilityIndividual,
    ImpermeabilityGroup,
    NeighbourhoodCommunity,
    #Env.CarbonRegs,
    # Env.Native.Species,
    # #Env.SacNorms,
    # # Env.SacMade,
    # # Env.SacWilling,
    # # Env.RoutineMade,
    # #  Env.RoutineWilling,
    Env.ClimateChgReal,
    Env.ClimateChgCause,
    Env.ClimateChgConcern,
    Env.SatNZEnvironment,
    Env.MotorwaySpend,
    Env.PubTransSubs,
    Env.SatWaterways,
    Env.Eff01.ActionBelief,
    Env.Eff02.ActionFeeling,
  ) |>
  mutate(Edu = as.numeric(Edu)) |> 
  dplyr::mutate(Employed = as.numeric(Employed)) |> 
  mutate(Believe.God = as.numeric(Believe.God)) |> 
  mutate(Believe.Spirit = as.numeric(Believe.Spirit)) |> 
  arrange(Id, Wave)  |> 
  dplyr::mutate(across(
    c(Env.MotorwaySpend,
      Env.PubTransSubs
      # Env.ClimateChgReal,
      # Env.ClimateChgCause,
      # Env.ClimateChgConcern,
      # Env.SatNZEnvironment,
    ),
    ~ lead(.x, n = 1),
    .names = "{col}_lead1"
  )) %>% # make leads
  dplyr::mutate(across(
    c(Env.ClimateChgReal,
      Env.ClimateChgCause,
      Env.ClimateChgConcern,
      Env.SatNZEnvironment,
      Env.Eff01.ActionBelief,
      Env.Eff02.ActionFeeling,
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2018) |> 
  # dplyr::filter(!is.na(Env.MotorwaySpend)) %>%
  # dplyr::filter(!is.na(Env.MotorwaySpend_lead1)) %>%
  # dplyr::filter(!is.na(Env.PubTransSubs)) %>%
  # dplyr::filter(!is.na(Env.PubTransSubs_lead1)) %>%
  dplyr::filter(!is.na(Env.MotorwaySpend)) %>%
  dplyr::filter(!is.na(Env.MotorwaySpend_lead1)) %>%
  dplyr::filter(!is.na(Env.PubTransSubs)) %>%
  dplyr::filter(!is.na(Env.PubTransSubs_lead1)) %>%
  dplyr::mutate(Employed = as.numeric(Employed)) |> 
  arrange(Id, Wave) %>%
  mutate(across(where(is.double), as.numeric)) |> 
  dplyr::select(-c(
    Env.Eff01.ActionBelief,
    Env.Eff02.ActionFeeling,
    YearMeasured
  )) %>%
  #dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing-- do this after imputation
  arrange(Id, Wave) %>%
  data.frame() %>% 
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)


skim(dt_formice) |> 
  arrange(n_missing)
str(dt_formice)

table1::table1(~ Env.ClimateChgConcern_lead1 + Env.ClimateChgConcern  + SCIENCE.TRUST + SCIENCE.TRUST_lead1|Wave, data = dt_formice, overall = FALSE
)

# number of ids
length(unique(dt_formice$Id)) #33982

# MICE --------------------------------------------------------------------
# mice model  -------------------------------------------------------------
library(mice)

mice_cc <- dt_formice %>%
  dplyr::select(-c(Wave, Id))  # won't otherwise run


dev.off()
library(naniar)
naniar::gg_miss_var(mice_cc)

# any colinear vars?
mice:::find.collinear(mice_cc)

# impute
mice_cc <- mice::mice(mice_cc,  seed = 0, m = 10)

# save your mice model
saveh(mice_cc, "environment-motorway_environ")
readh

mice_cc <- readh("environment-motorway_environ")


# check your mice model
outlist2 <-
  row.names(mice_cc)[mice_cc$outflux < 0.5]
length(outlist2)

# checks
head(mice_cc$loggedEvents, 10)

# read your mice model

# we create two completed data sets -- the one without the missing data will be useful for
# determing causal contrasts -- which we'll describe below.

cc_l <- mice::complete(mice_cc, "long", inc = TRUE)



# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(cc_l) |>
  arrange(n_missing)

# for keeping track of ID's in mice data
N <- length(unique(dt_formice$Id))
N
# create variables in z score
cc_l2 <- cc_l %>%
  dplyr::mutate(id = as.factor(rep(1:N, 11))) |> # needed for g-comp
  dplyr::mutate(income_log = log(Household.INC + 1)) |>
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) |>
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) |>
  dplyr::mutate(Employed = as.numeric(Employed)) |> 
  dplyr::mutate(id = as.factor(rep(1:N, 11))) |> # needed for g-comp
  # dplyr::group_by(id) |> mutate(Env.Eff = mean(c(
  #   Env.Eff01.ActionBelief, Env.Eff02.ActionFeeling
  # ), na.rm = TRUE)) |>
  dplyr::mutate(Env.Eff_lead1 = mean(
    c(Env.Eff01.ActionBelief_lead1, Env.Eff02.ActionFeeling_lead1),
    na.rm = TRUE
  )) |>
  droplevels() |>
  dplyr::mutate(KESSLER6sum = round(as.integer(KESSLER6sum, 0))) %>%
  mutate(across(where(is.double), as.numeric)) %>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z)) %>%
  dplyr::mutate(
    EthCat = as.factor(EthCat),
    Gender3  = as.factor(Gender3)
  )



cc_l2 <- cc_l2 %>% mutate_if(is.matrix, as.vector)
# cc3l <- cc_l %>% mutate_if(is.matrix, as.vector)

# imputed data
data_imputed <- mice::as.mids(cc_l2)

saveh(data_imputed, "ml_motorway_environ")

# imputed data in long format

data_long <- mice::complete(data_imputed, "long", inc = F)


# raw data (pre-imputation) for sensitivity analysis
data_raw <- data_long |>
  slice(1:N)

str(data_long)

# EXAMPLE DEMOGRAPHIC TABLE -----------------------------------------------


df_crr <-  df_cr |>
  dplyr::mutate(Volunteers = if_else(HoursCharity > 0, 1, 0))

df_crr <- df_cr |> dplyr::group_by(Id) |> mutate(PWI = mean(
  c(
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living
  ),
  na.rm = TRUE
))

df_crr$Male <- factor(df_cr$Male, labels = c("No", "Yes"))
df_crr$EthCat <-
  factor(df_cr$EthCat, labels = c("Euro", "Maori", "Pacific", "Asian"))
df_crt$Believe.Spirit <-
  factor(df_cr$Believe.Spirit, labels = c("No", "Yes"))
df_crt$Believe.God <-
  factor(df_cr$Believe.God, labels = c("No", "Yes"))
df_crt$Employed <-
  factor(df_cr$Employed, labels = c("No", "Yes"))
df_crt$Volunteers <-
  factor(df_crt$Volunteers, labels = c("No", "Yes"))
df_crt$Parent <- factor(df_cr$Parent, labels = c("No", "Yes"))
df_crt$Partner <-
  factor(df_cr$Partner, labels = c("No", "Yes"))
df_crt$Retired <-
  factor(df_cr$retired, labels = c("No", "Yes"))
df_crt$SemiRetired <-
  factor(df_cr$semiretired, labels = c("No", "Yes"))
df_crt$Urban <- factor(df_cr$Urban, labels = c("No", "Yes"))
df_crt$BigDoms <-
  factor(df_cr$BigDoms,
         labels = c("Buddhist", "Christian", "Muslim", "TheOthers"))
df_crt$NeighbourhoodCommunity <- df_cr$community
df_crt$MajorDenominations <- df_cr$BigDoms


