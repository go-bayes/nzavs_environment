# imputation template

# set up digits
options(scipen = 999)
# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

# for original NZAVS data -- need to contact Chris Sibley for access


conflict_prefer("pool", "mice")
conflict_prefer("cbind", "base")
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

dff <- readRDS(pull_path)

# Worked example selecting waves 2018 -- 2020 with exposure year as 2019

tab_in <- dff %>%
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
    Env.Native.Species = Env.NATIVE.SPECIES
  ) |>
  dplyr::filter((Wave == 2013  &
                   YearMeasured  == 1) |
                  (Wave == 2014  & YearMeasured == 1) |
                  (Wave == 2015 & YearMeasured  != -1) |
                  (Wave == 2016 & YearMeasured  != -1) |
                  (Wave == 2017 & YearMeasured  != -1) |
                  (Wave == 2018 &
                     YearMeasured  != -1)
  )  %>% # Eligibility criteria
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


# check n # 34782
length(unique(tab_in$Id)) # 14878

`# how is something changing? (Attrition of disabled people?)
# Do you have a health condition or disability that limits you, and that has lasted for 6+ months?

tab_in %>%
  group_by(Wave) %>%
  summarise(mean(HLTH.Disability, na.rm = TRUE))

table1::table1( ~ HLTH.Disability | Wave, data = tab_in)
# select-variables  -------------------------------------------------------
df_cr <- tab_in %>%
  select(
    Id,
    YearMeasured,
    Wave,
    Partner,
    EthCat,
    Age,
    Gender3,
    SexualOrientation,
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
    SDO,
    RWA,
    Urban,
    Household.INC,
    Parent,
    Relid,
    Religious,
    Church,
    #  Prayer,
    #  Scripture,
    #   Believe.Spirit,
    #   Believe.God,
    #   Spiritual.Identification,
    SWB.SoC01,
    # EmotionRegulation1,
    # EmotionRegulation2,
    # EmotionRegulation3,
    Bodysat,
    #   VENGEFUL.RUMIN,
    #   retired,
    #   semiretired,
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
    #   GRATITUDE,
    Hours.Work,
    HLTH.SleepHours,
    HLTH.Disability,
    Hours.Exercise,
    #   LIFEMEANING,
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
    #   Alcohol.Frequency,
    #    Alcohol.Intensity,
    HLTH.BMI,
    Smoker,
    ChildrenNum,
    # GenCohort,
    #    partnerlost_job,
    #    lost_job,
    #   began_relationship,
    #    Alcohol.Intensity,
    #    Alcohol.Frequency,
    #   SexualSatisfaction,
    #  POWERDEPENDENCE1,
    #  POWERDEPENDENCE2,
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living,
    #   PERFECTIONISM,
    #    PermeabilityIndividual,
    #    ImpermeabilityGroup,
    SWB.SoC01,
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
    Env.Eff02.ActionFeeling,
    Env.CarbonRegs
  ) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(Employed = as.numeric(Employed)) %>%
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
    Church = ifelse(Church > 8, 8, Church)
  ) %>%
  arrange(Id, Wave)  %>% #
  dplyr::mutate(across(
    c(
      Env.ClimateChgReal,
      Env.ClimateChgCause,
      Env.ClimateChgConcern,
      Env.CarbonRegs,
      Env.SacNorms
    ),
    ~ lead(.x, n = 1),
    .names = "{col}_lead1"
  )) %>% # make leads
  dplyr::mutate(across(
    c(
      Env.ClimateChgReal,
      Env.ClimateChgCause,
      Env.ClimateChgConcern,
      Env.SatNZEnvironment,
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
      Env.Possum.Control
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
      Env.Native.Species,
      Env.Possum.Control,
      Env.Eff01.ActionBelief,
      Env.Eff02.ActionFeeling,
      Env.SacWilling,
      Env.SacMade,
      Env.SacNorms
      
    ),
    ~ lead(.x, n = 4),
    .names = "{col}_lead4"
  )) %>% # make leads
  dplyr::mutate(across(
    c(
      Env.SatNZEnvironment,
      Env.MotorwaySpend,
      Env.PubTransSubs,
    ),
    ~ lead(.x, n = 5),
    .names = "{col}_lead5"
  )) %>% # make leads
  dplyr::filter(Wave == 2013) %>%
  dplyr::filter(!is.na(Env.ClimateChgConcern)) %>%
  dplyr::filter(!is.na(Env.ClimateChgConcern_lead1)) %>%
  dplyr::filter(!is.na(Env.SacNorms)) %>%
  dplyr::filter(!is.na(Env.SacNorms_lead1)) %>%
  dplyr::filter(!is.na(Env.ClimateChgCause)) %>%
  dplyr::filter(!is.na(Env.ClimateChgCause_lead1)) %>%
  dplyr::filter(!is.na(Env.ClimateChgReal)) %>%
  dplyr::filter(!is.na(Env.ClimateChgReal_lead1)) %>%
  dplyr::select(-c(SWB.SoC01,
                   HoursCharity,
                   YearMeasured)) %>%
  #dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing-- do this after imputation
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)


table1::table1(~ Env.SacNorms + Env.SacNorms_lead1 |
                 Wave ,
               data = df_cr,
               overall = FALSE)#11953


table1::table1(
  ~ Env.Eff01.ActionBelief_lead4 + Env.MotorwaySpend_lead5 |
    Wave ,
  data = df_cr,
  overall = FALSE
)#11953





table1::table1(
  ~ Env.ClimateChgConcern + Env.ClimateChgConcern_lead1 |
    Wave ,
  data = df_cr,
  overall = FALSE
)#11953

# hist(df_cr$Env.ClimateChgConcern)
# hist(df_cr$Env.ClimateChgConcern_lead1)

# number of ids
length(unique(df_cr$Id)) #13178


# MICE --------------------------------------------------------------------
# mice model  -------------------------------------------------------------
library(mice)

mice_cc <- df_cr %>%
  dplyr::select(-c(Wave, Id))  # won't otherwise run



library(naniar)
naniar::gg_miss_var(mice_cc)

# any colinear vars?
mice:::find.collinear(mice_cc)

# impute
mice_cc <- mice::mice(mice_cc,  seed = 0, m = 10)

# save your mice model
saveh(mice_cc, "environ_omni_wave5")


mice_cc <- readh("environ_omni_wave5")

# check your mice model
outlist2 <-
  row.names(mice_cc)[mice_cc$outflux < 0.5]
length(outlist2)

# checks
head(mice_cc$loggedEvents, 10)

# read your mice model
#mice_cc<- readRDS(here::here("data", "mice_imputed")

# we create two completed data sets -- the one without the missing data will be useful for
# determing causal contrasts -- which we'll describe below.

cc_l <- mice::complete(mice_cc, "long", inc = TRUE)



# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(cc_l) |>
  arrange(n_missing)

# for keeping track of ID's in mice data
N <- length(unique(df_cr$Id))

# create variables in z score
cc_l2 <- cc_l %>%
  dplyr::mutate(id = as.factor(rep(1:N, 11))) |> # needed for g-comp
  dplyr::mutate(income_log = log(Household.INC + 1)) |>
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) |>
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) |>
  dplyr::mutate(id = as.factor(rep(1:N, 11))) |> # needed for g-comp
  dplyr::group_by(id) |> mutate(Env.Eff = mean(c(
    Env.Eff01.ActionBelief, Env.Eff02.ActionFeeling
  ), na.rm = TRUE)) |>
  dplyr::mutate(Env.Eff_lead4 = mean(
    c(Env.Eff01.ActionBelief_lead4, Env.Eff02.ActionFeeling_lead4),
    na.rm = TRUE
  )) |>
  dplyr::mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  )) |>
  # dplyr::mutate(KESSLER6sum = rowSums(across(
  #   c(
  #     kessler_hopeless,
  #     # …  you feel hopeless?
  #     kessler_depressed,
  #     #…  you feel so depressed that nothing could cheer you up?
  #     kessler_restless,
  #     #…  you feel restless or fidgety?
  #     kessler_effort,
  #     #…  you feel that everything was an effort?
  #     kessler_worthless,
#     #…  you feel worthless?
#     kessler_nervous #…  you feel nervous?
#   )
# ))) |>
# dplyr::mutate(KESSLER6sum_lead2 = rowSums(across(
#   c(
#     kessler_hopeless_lead2,
#     # …  you feel hopeless?
#     kessler_depressed_lead2,
#     #…  you feel so depressed that nothing could cheer you up?
#     kessler_restless_lead2,
#     #…  you feel restless or fidgety?
#     kessler_effort_lead2,
#     #…  you feel that everything was an effort?
#     kessler_worthless_lead2,
#     #…  you feel worthless?
#     kessler_nervous_lead2
#   ) #…  you feel nervous?
# ))) |>
ungroup() |>
  droplevels() |>
  dplyr::mutate(KESSLER6sum = round(as.integer(KESSLER6sum, 0))) %>%
  mutate(across(where(is.double), as.numeric)) %>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z)) %>%
  dplyr::mutate(
    EthCat = as.factor(EthCat),
    Gender3  = as.factor(Gender3),
    SexualOrientation  = as.factor(SexualOrientation)
  )



cc_l2 <- cc_l2 %>% mutate_if(is.matrix, as.vector)
# cc3l <- cc_l %>% mutate_if(is.matrix, as.vector)

# imputed data
data_imputed <- mice::as.mids(cc_l2)

#saveh(data_imputed, "ml_environ_omni_wave5")

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





#and continue this way to obtain factor labels ...etc.

table1::table1(
  ~ Age +
    BornNZ +
    Edu +
    Employed +
    EthCat +
    NZdep +
    NZSEI13 +
    Parent +
    Partner +
    Pol.Orient +
    Male +
    Urban,
  data = df_crr,
  transpose = F
)


# Personality


table1::table1(
  ~ AGREEABLENESS +
    CONSCIENTIOUSNESS +
    EXTRAVERSION +
    HONESTY_HUMILITY +
    NEUROTICISM +
    OPENNESS +
    KESSLER6sum,
  data = df_crr,
  transpose = F
)

table1::table1(
  ~ LIFESAT +
    PWI +
    Respect.Self +
    RWA +
    SDO +
    SELF.CONTROL +
    SELF.ESTEEM +
    SFHEALTH,
  data = df_crr,
  transpose = F
)

# religious
table1::table1(
  ~ Religion.CongregationSize +
    Relid +
    Believe.Spirit +
    Believe.God +
    Church +
    Religion.Prayer +
    Religion.Scripture +
    MajorDenominations,
  data = df_crr,
  transpose = F
)


# Social variables

table1::table1(
  ~ BELONG +
    NeighbourhoodCommunity +
    SUPPORT +
    National.Identity +
    PATRIOT,
  data = df_crr,
  transpose = F
)


# MULTILEVEL DATA ---------------------------------------------------------



### Create multi-level data for comparisions
data_ml <- tab_in |>
  select(
    Id,
    YearMeasured,
    Wave,
    Partner,
    EthCat,
    Age,
    Gender3,
    SexualOrientation,
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
    SDO,
    RWA,
    Urban,
    Household.INC,
    Parent,
    Relid,
    Religious,
    Church,
    #  Prayer,
    #  Scripture,
    #   Believe.Spirit,
    #   Believe.God,
    #   Spiritual.Identification,
    SWB.SoC01,
    # EmotionRegulation1,
    # EmotionRegulation2,
    # EmotionRegulation3,
    Bodysat,
    #   VENGEFUL.RUMIN,
    #   retired,
    #   semiretired,
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
    #   GRATITUDE,
    Hours.Work,
    HLTH.SleepHours,
    HLTH.Disability,
    Hours.Exercise,
    #   LIFEMEANING,
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
    #   Alcohol.Frequency,
    #    Alcohol.Intensity,
    HLTH.BMI,
    Smoker,
    ChildrenNum,
    # GenCohort,
    #    partnerlost_job,
    #    lost_job,
    #   began_relationship,
    #    Alcohol.Intensity,
    #    Alcohol.Frequency,
    #   SexualSatisfaction,
    #  POWERDEPENDENCE1,
    #  POWERDEPENDENCE2,
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living,
    #   PERFECTIONISM,
    #    PermeabilityIndividual,
    #    ImpermeabilityGroup,
    SWB.SoC01,
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
    Env.Eff02.ActionFeeling,
    Env.CarbonRegs
  ) %>%
  dplyr::group_by(id) |> mutate(Env.Eff = mean(c(
    Env.Eff01.ActionBelief, Env.Eff02.ActionFeeling
  ), na.rm = TRUE)) |>
  dplyr::mutate(Env.Eff_lead4 = mean(
    c(Env.Eff01.ActionBelief_lead4, Env.Eff02.ActionFeeling_lead4),
    na.rm = TRUE
  )) |>
  ungroup() |>
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
    Church = ifelse(Church > 8, 8, Church)
  ) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(wave = as.numeric(Wave) - 1) |>
  dplyr::mutate(income_log = log(Household.INC + 1)) |>
  dplyr::mutate(Church = ifelse(Religion.Church > 8, 8, Religion.Church)) |>
  # dplyr::mutate( inc_prop = (income_log / (income_log_lead1) - 1)) |>
  dplyr::mutate(CharityDonate = round(CharityDonate, 0)) %>%
  dplyr::mutate(Volunteers = if_else(HoursCharity > 1, 1, 0)) |>
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0)) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) %>%
  dplyr::mutate(Exercise_log = log(Hours.Exercise + 1)) %>%
  #  dplyr::mutate(Rumination_ord = as.integer(round(Rumination, digits = 0) + 1)) %>%  # needs to start at 1
  dplyr::mutate(NZSEI13_10 =  NZSEI13 / 10) %>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work / 10) %>%
  dplyr::group_by(Id, Wave) |>
  dplyr::mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  )) |>
  ungroup() |>
  droplevels() |>
  dplyr::mutate(KESSLER6sum = round(as.integer(KESSLER6sum, 0))) %>%
  dplyr::mutate(across(
    !c(Id, Wave, EthCat, SexualOrientation, Gender3),
    ~ as.numeric(.x)
  )) %>% # make factors numeric for easy of
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  dplyr::mutate(
    EthCat = as.factor(EthCat),
    Gender3  = as.factor(Gender3),
    SexualOrientation  = as.factor(SexualOrientation)
  )





table1::table1( ~ KESSLER6sum_z + EthCat + SexualOrientation + Gender3 |
                  Wave, data = data_ml)


saveRDS(data_ml, here::here("data", "data_ml"))
