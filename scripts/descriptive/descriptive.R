# ENVIRON NORMS

## Concern study

#template_outcomewide.R

# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

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
table(dff$GenCohort)

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
  dplyr::filter((Wave == 2018  &
                   YearMeasured  == 1) |
                  (Wave == 2019 & YearMeasured != -1) |
                  (Wave == 2020 & YearMeasured != -1) 
  )  %>% # 
  group_by(Id) %>%
  dplyr::mutate(org = ifelse(Wave == 2018 &
                                YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold = mean(org, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold > 0) %>% 
  ungroup() %>%
  droplevels() %>%
  arrange(Id, Wave)


# check n # 34782
length(unique(tab_in$Id)) # 47948

# how is something changing? (Attrition of disabled people?)
# Do you have a health condition or disability that limits you, and that has lasted for 6+ months?

tab_in %>%
  group_by(Wave) %>%
  summarise(mean(Env.ClimateChgConcern, na.rm = TRUE),
            mean(Env.Eff01.ActionBelief,na.rm = TRUE),
            mean(Env.Eff02.ActionFeeling, na.rm = TRUE))

table1::table1( ~ HLTH.Disability | Wave, data = tab_in)

# select-variables  -------------------------------------------------------
df_cr <- tab_in %>%
  select(
    Id,
    YearMeasured,
    Wave,
    Partner,
    EthCat,
    GenCohort,
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
    # Env.SacNorms,
    # Env.SacMade,
    # Env.SacWilling,
    # Env.RoutineMade,
    # Env.RoutineWilling,
    Env.ClimateChgReal,
    Env.ClimateChgCause,
    Env.ClimateChgConcern,
    Env.SatNZEnvironment,
    Env.Native.Species,
    Env.Possum.Control,
    Env.Eff01.ActionBelief,
    Env.Eff02.ActionFeeling,
    Env.CarbonRegs,
    Env.SatWaterways,
    Env.MotorwaySpend,
    Env.PubTransSubs
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
  # arrange(Id, Wave)  %>% #
  # dplyr::mutate(across(
  #   c(Env.Eff01.ActionBelief,
  #     Env.Eff02.ActionFeeling,
  #     Env.ClimateChgReal,
  #     Env.ClimateChgCause,
  #     Env.ClimateChgConcern,
  #     Env.CarbonRegs,
  #     Env.SacNorms
  #   ),
  #   ~ lead(.x, n = 1),
  #   .names = "{col}_lead1"
  # )) %>% # make leads
  # dplyr::mutate(across(
  #   c(
  #     Env.ClimateChgReal,
  #     Env.ClimateChgCause,
  #     Env.ClimateChgConcern,
  #     Env.SatNZEnvironment,
  #   ),
  #   ~ lead(.x, n = 2),
  #   .names = "{col}_lead2"
  # )) %>% # make leads
  # dplyr::mutate(across(
  #   c(
  #     Env.ClimateChgReal,
  #     Env.ClimateChgCause,
  #     Env.ClimateChgConcern,
  #     Env.SatNZEnvironment,
  #     Env.Native.Species,
  #     Env.Possum.Control
  #   ),
  #   ~ lead(.x, n = 3),
  #   .names = "{col}_lead3"
  # )) %>% # make leads
  # dplyr::mutate(across(
  #   c(Env.Eff01.ActionBelief,
  #     Env.Eff02.ActionFeeling,
  #     Env.ClimateChgReal,
  #     Env.ClimateChgCause,
  #     Env.ClimateChgConcern,
  #     Env.SatNZEnvironment,
  #     Env.Native.Species,
  #     Env.Possum.Control,
  #     Env.SacWilling,
  #     Env.SacMade,
  #     Env.SacNorms
  #     
  #   ),
  #   ~ lead(.x, n = 4),
  #   .names = "{col}_lead4"
  # )) %>% # make leads
  # dplyr::mutate(across(
  #   c(
  #     Env.SatNZEnvironment,
  #     Env.MotorwaySpend,
  #     Env.PubTransSubs,
  #   ),
  #   ~ lead(.x, n = 5),
  #   .names = "{col}_lead5"
  # )) %>% # make leads
  # dplyr::select(-c(SWB.SoC01,
  #                  HoursCharity,
  #                  YearMeasured)) %>%
  # #dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing-- do this after imputation
  arrange(Id, Wave) %>%
  data.frame() %>%
#  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)

str(df_cr$GenCohort)

length(unique(df_cr$Id))

table1::table1(~ Env.ClimateChgConcern  + 
               Env.ClimateChgCause + 
               Env.ClimateChgConcern +
               Env.SatNZEnvironment +
               Env.Native.Species +
               Env.Possum.Control +
               Env.Eff01.ActionBelief +
               Env.Eff02.ActionFeeling + 
                Env.CarbonRegs + 
               Env.SatWaterways +
               Env.MotorwaySpend +
               Env.PubTransSubs|
                 GenCohort,
               data = df_cr,
               #transpose = TRUE,
               overall = FALSE, 
               )#11953

dt_temp <- df_cr |> select(GenCohort,
                           Env.ClimateChgReal,
                  Env.ClimateChgConcern ,
                  Env.ClimateChgCause ,
                  Env.SatNZEnvironment ,
                  Env.Native.Species ,
                  Env.Possum.Control ,
                  Env.Eff01.ActionBelief ,
                  Env.Eff02.ActionFeeling ,
                  Env.CarbonRegs ,
               #   Env.SatWaterways ,
                  Env.MotorwaySpend ,
                  Env.PubTransSubs)



dt_long <- pivot_longer(dt_temp,
                          cols = -c("GenCohort"),
                          names_prefix = "Env.",
                          values_to = "Values",
                          names_to = "Target"
  ) |> drop_na()


dev.off()
ggplot(dt_long, aes(x=Target, y=Values, color=Target)) +
    geom_violin() +  coord_flip() +  scale_fill_brewer(palette="RdBu") + theme_minimal()
  
#notch = TRUE, outlier.colour="red", outlier.shape=8,
  #outlier.size=4
  
  # https://ggplot2tutor.com/tutorials/summary_statistics
  
  dt_long |>
    ggplot(aes(x = Target, y = Values, color=Target)) +
    stat_summary(fun.data = "mean_cl_normal",
                 fun.args = list(
                   conf.int = .99),
                 #  position = position_dodge(0.95),
                 size = .1) +
    # stat_summary(fun = mean,
    #              geom = "pointrange",
    #              fun.min = function(x) mean(x) + sd(x),
    #              fun.max = function(x) mean(x) - sd(x)) +
    scale_y_continuous(limits = c(1,7)) +
    coord_flip() +  scale_fill_viridis_d() + theme_minimal()
  
  
  
  # library(beanplot)
  # beanplot(Warmth ~ Target,
  #          data= dt_long,
  #         main="Warmth to Groups in New Zealand",
  #         xlab="Groups",
  #         # notch = TRUE,
  #         ylab="Warmth Rating 1-7",
  #         # horizontal = TRUE,
  #         border="brown",
  #         cex.axis=.7,
  #         coef = 2.5
  # )
  

  
  env_values <- dt_long|>
    dplyr::mutate(Target = forcats::fct_reorder(Target, desc(Values))) |>
    ggplot2::ggplot(aes(Target, Values, fill = Target)) +
    labs(title = "Environmental Values in NZ 2018-2021 (N = 47948)") +
    geom_violin(size =.05 ) +  scale_fill_viridis_d() +
    coord_flip() +
    theme(legend.position = "none")
  
  env_values
  ggsave(
    env_values,
    path = here::here(here::here("figs", "teaching")),
    width = 9,
    height = 9,
    units = "in",
    filename = "env_values.jpg",
    device = 'jpeg',
    limitsize = FALSE,
    dpi = 1000
  )

table(df_cr$GenCohort)

dt_use <- df_cr |> 
  dplyr::mutate(GenCohort = forcats::fct_reorder(GenCohort, desc(Values))) |>
    
## models
  Env.ClimateChgConcern ,
  Env.ClimateChgCause ,
  Env.ClimateChgConcern ,
  Env.SatNZEnvironment ,
  Env.Native.Species ,
  Env.Possum.Control ,
  Env.Eff01.ActionBelief ,
  Env.Eff02.ActionFeeling ,
  Env.CarbonRegs ,
  #   Env.SatWaterways ,
  Env.MotorwaySpend ,
  Env.PubTransSubs

Env.Native.Species ,
Env.Possum.Control ,
Env.Eff01.ActionBelief ,
Env.Eff02.ActionFeeling ,
Env.CarbonRegs ,
#   Env.SatWaterways ,
Env.MotorwaySpend ,
Env.PubTransSubs

distress <- glm( KESSLER6sum ~  GenCohort, data = df_cr, family = "poisson")

real <- glm( Env.ClimateChgReal ~  GenCohort, data = df_cr)
cause <- glm( Env.ClimateChgCause ~  GenCohort, data = df_cr)
concern <- glm( Env.ClimateChgConcern ~  GenCohort, data = df_cr)
satenv <- glm( Env.SatNZEnvironment ~ GenCohort, data = df_cr)
species <- glm( Env.Native.Species ~  GenCohort, data = df_cr)
possum <- glm( Env.Possum.Control ~ GenCohort , data = df_cr)
eff_action <- glm( Env.Eff01.ActionBelief ~ GenCohort, data = df_cr)
motor <- glm( Env.MotorwaySpend ~ GenCohort, data = df_cr)
transport <- glm( Env.PubTransSubs ~ GenCohort, data = df_cr)
summary(possum)
Env.CarbonRegs

hist(df_cr$Env.Possum.Control)
possum

k6_0 <- plot(
  ggeffects::ggpredict(distress,
                       terms = "GenCohort")) + 
  labs(title = "Kessler 6 distress") + scale_y_continuous(limits = c(0,15)) + geom_hline(yintercept=5, color = "orange")  + 
   geom_hline(yintercept=13, color = "red") 


real_1 <- plot(
  ggeffects::ggpredict(real,
                       terms = "GenCohort")) + 
  labs(title = "Climate change is real") + scale_y_continuous(limits = c(4,7))
real_1

cause_2 <- plot(
  ggeffects::ggpredict(cause,
                       terms = "GenCohort")
)+ labs(title = "Climate change is caused by humans") + scale_y_continuous(limits = c(4,7))

cause_2


concern_3 <- plot( ggeffects::ggpredict(concern, 
                           terms = "GenCohort" ))  + labs(title = "I am deeply concerned about climate change") + scale_y_continuous(limits = c(4,7))
concern_3


satenv_3a <- plot( ggeffects::ggpredict(satenv, 
                                        terms = "GenCohort" )) + labs(
                                          title = "The quality of New Zealand’s natural environment.") + scale_y_continuous(limits = c(4,7))

satenv_3a

species_4 <- plot( ggeffects::ggpredict(species, 
                                        terms = "GenCohort" )) + 
  labs(title = "Protecting New Zealand’s native species should be a national priority.") + scale_y_continuous(limits = c(4,7))

species_4

eff_5 <- plot( ggeffects::ggpredict(eff_action, 
                                        terms = "GenCohort" )) + 
  labs(title = "By taking personal action I believe I can make a positive difference to environmental problems.") + scale_y_continuous(limits = c(4,7))

eff_5

possum_6 <- plot( ggeffects::ggpredict(possum, 
                                        terms = "GenCohort" )) + 
  labs(title = "Do you support the use of 1080 poison\nfor possum control in New Zealand?") + scale_y_continuous(limits = c(4,7))

possum_6

motor_7 <- plot( ggeffects::ggpredict(concern, 
                                        terms = "GenCohort" )) + 
  labs(title = "Increased government spending on new motorways.") + scale_y_continuous(limits = c(4,7))

motor_7

transport_8 <- plot( ggeffects::ggpredict(concern, 
                                        terms = "GenCohort" )) + 
  labs(title = "Government subsidy of public transport.") + scale_y_continuous(limits = c(4,7))



real_1
cause_2
concern_3
satenv_3a
satenv_3a
species_4
eff_5
possum_6
motor_7
transport_8




k6_0





ggsave(
  k6_0,
  path = here::here(here::here("figs", "teaching")),
  width = 9,
  height = 9,
  units = "in",
  filename = "k6_0.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)


distress_0 <- ggsave(
  k6_0,
  path = here::here(here::here("figs", "teaching")),
  width = 9,
  height = 9,
  units = "in",
  filename = "real_1.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)
ggsave(
  real_1,
  path = here::here(here::here("figs", "teaching")),
  width = 9,
  height = 9,
  units = "in",
  filename = "real_1.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)

ggsave(
  cause_2,
  path = here::here(here::here("figs", "teaching")),
  width = 9,
  height = 9,
  units = "in",
  filename = "cause_2.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)
  
  
ggsave(
  concern_3,
  path = here::here(here::here("figs", "teaching")),
  width = 9,
  height = 9,
  units = "in",
  filename = "concern_3.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)


ggsave(
  satenv_3a,
  path = here::here(here::here("figs", "teaching")),
  width = 9,
  height = 9,
  units = "in",
  filename = "satenv_3a.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)

ggsave(
  species_4,
  path = here::here(here::here("figs", "teaching")),
  width = 9,
  height = 9,
  units = "in",
  filename = "species_4.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)



ggsave(
  eff_5,
  path = here::here(here::here("figs", "teaching")),
  width = 9,
  height = 9,
  units = "in",
  filename = "eff_5.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)

ggsave(
  possum_6,
  path = here::here(here::here("figs", "teaching")),
  width = 9,
  height = 9,
  units = "in",
  filename = "possum_6.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)



ggsave(
  motor_7,
  path = here::here(here::here("figs", "teaching")),
  width = 9,
  height = 9,
  units = "in",
  filename = "motor_7.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)

ggsave(
  transport_8,
  path = here::here(here::here("figs", "teaching")),
  width = 9,,
  height = 9,
  units = "in",
  filename = "transport_8.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)



###### MAKE FOLDER CALLED "data"  #########

###### MAKE FOLDER CALLED "figs"  #########

###### READ THIS DATA IN   #########

here::here()


###############  RENAME YOUR IMPUTED DATASET  'df"

#df <- readh("ml_pol_orient_environ_omni_wave5")

df <- readRDS(here::here("/Users/bulbuljo/The\ Virtues\ Project\ Dropbox/outcomewide/mods/ml_environ_omni_wave5"))

#df<- data_imputed

############### SET YOUR EXPOSURE VARIABLE, in this case "WORK one year after baseline
## HERE WE USE THE EXAMPLE OF HOURS WORK / 10

# SWB.SoC01,
# Env.CarbonRegs,
# Env.MotorwaySpend,
# Env.PubTransSubs,
# Env.Native.Species,
# Env.SacNorms,
# Env.SacMade,
# Env.SacWilling,
# Env.RoutineMade,
# Env.RoutineWilling,
# Env.ClimateChgReal,
# Env.ClimateChgCause,
# Env.ClimateChgConcern,
# Env.SatNZEnvironment,
# Env.Native.Species,
# Env.Possum.Control,
# Env.Eff01.ActionBelief,
# Env.Eff02.ActionFeeling,
# Env.CarbonRegs

data_long$Env.SacNorms_lead1

X = "Pol.Orient_lead1_z"


# 
# 
# hist(data_long$Env.SacNorms_lead1_z)
# sd(data_long$Env.SacNorms_lead1)
# min(data_long$Env.SacNorms_lead1_z)
# max(data_long$Env.SacNorms_lead1_z)

############### NEXT SET UP VARIABLES FOR MODELS AND GRAPHS

# You may set your label for your graphs  HERE WE STICK TO THE EXAMPLE OF WORK
xlab = "Pol.Orient_lead1_z"  ## Weekly hours devided by 10


# SET THE RANGE for our natural experiment (here from -1 pol.orient to 1)
min = -1
max =  1


# set full range of X
x =  min:max

# baseline
r = -1

# focal contrast for X
f = 1

# REQUIRED for certain model model functions
c = x

# contrast for graphs -- absolute distance from baseline
p = c(r, f) #


# Needed for E-VALUES -- how much do we move on the X scale to obtain our effect?
#delta = 4 #
delta = abs(r - f)

ylim = c(-1, 1)  # SET AS YOU LIKE -- here, how much movement across a standard deviation unit of the outcome
ylim_contrast = c(0, 2)  # SET AS YOU LIKE (FOR CONTRASTS )

# mice imputed data
# n imputations
m = 10

# standard deviation of the outcome (for evalues)
# We have stanadardised the (non-binary) outcomes for comparable effect sizes.
sd = 1



##### BASELINE VARIABLES

cvars = c(
  "AGREEABLENESS_z",
  "CONSCIENTIOUSNESS_z",
  "EXTRAVERSION_z",
  "HONESTY_HUMILITY_z",
  "NEUROTICISM_z",
  "OPENNESS_z",
  "Age_z",
  "Bodysat_z",
  "BornNZ_z",
  "BELONG_z",
  "CharityDonate_log_z",
  "ChildrenNum_z",
  "Church_z",
  #  "community",
  "Edu_z",
  "Employed_z",
  # "Emp.JobSecure_z",
  "EthCat",
  "Gender3",
  #
  "Hours.Exercise_log_z",
  "Hours.Work_z",
  "HLTH.BMI_z",
  "HLTH.Disability_z",
  "HLTH.Fatigue_z",
  "HLTH.SleepHours_z",
  "income_log_z",
  "KESSLER6sum_z",
  "LIFESAT_z",
  "NZdep_z",
  "NWI_z",
  "NZSEI13_z",
  "Parent_z",
  "Partner_z",
  "Pol.Orient_z",
  "Relid_z",
  "Respect.Self_z",
  # "Rumination_z",
  "SELF.CONTROL_z",
  "SELF.ESTEEM_z",
  "SFHEALTH_z",
  "Smoker_z",
  "Standard.Living_z",
  "SUPPORT_z",
  "Urban_z",
  "Volunteers_z",
  "Your.Health_z",
  "Your.Future.Security_z",
  "Your.Personal.Relationships_z",
  "Env.CarbonRegs",
  "Env.MotorwaySpend",
  "Env.PubTransSubs",
  "Env.Native.Species",
  "Env.SacNorms",
  "Env.SacMade",
  "Env.SacWilling",
#  "Env.RoutineMade",
#  "Env.RoutineWilling",
  "Env.ClimateChgReal",
  "Env.ClimateChgCause",
  "Env.ClimateChgConcern",
  "Env.SatNZEnvironment",
  "Env.Native.Species",
  "Env.Possum.Control",
 # "Env.Eff01.ActionBelief",
#  "Env.Eff02.ActionFeeling",
 # "Env.CarbonRegs"
)
cvars

#*** Demographic
# Race
# Age
# Gender
# Marital Status
# *** Economic, Social and Political
# Income
# Education
# Employment
# Social integration Neighborhood
# Religious service attendance
# Political affiliation
### *** Health
# Self-rated health
# Number of health conditions
# Exercise
# Smoking
# Alcohol consumption
# Depression
# Happiness Loneliness
# Parental warmth Purpose/Meaning Big five personality

## STATEMENT OF "VANDERWEEL-E-VALUE FROM TYLER

# “With an observed risk ratio of RR = XX, an unmeasured confounder that was associated with both the outcome and the exposure by a risk ratio of XX -fold each, above and beyond the measured confounders, could explain away the estimate, but weaker joint confounder associations could not; to move the confidence interval to include the null, an unmeasured confounder that was associated with the outcome and the exposure by a risk ratio of XX -fold each could do so, but weaker joint confounder associations could not.”

# EVALUES FOR CONTINOUS VARS - p.448
# For a continuous outcome, with a standardized effect size “d” (obtained by dividing the mean difference on the outcome variable between exposure groups by the pooled standard deviation of the outcome) and a stan- dard error for this effect size sd , an approximate E-value can be obtained (VanderWeele and Ding, 2017) by ap- plying the approximation RR ≈ exp(0.91 × d) and then using the E-value formula above (E-value = RRobs + √RRobs(RRobs − 1)). An approximate confidence inter- val can be found using the approximation
# 􏰛exp{0.91×d −1.78×sd},exp{0.91×d +1.78×sd}􏰜

# We could include statements like this in all empirical papers


# NOTE THAT I HAVE WRITTEN WRAPPER FUNCTIONS TO AUTOMATE REPORTING OF EVALUES, ALSO TO CREATE TABLES -- YOUR WORK IS LIGHT!
# however the code is:


# round(EValue::evalues.OLS(
#   ,
#   se = ,
#   sd = sd,
#   delta = delta,
#   true = 0
# ), 3)
# round(EValue::evalues.RR(, lo =  , hi = , true = 1), 4)
#


################# BELOW THE MANY OUTCOMES!  ########################################


#  Env.ClimateChgCause_lead2_z ------------------------------------------------------------
Y = "Env.ClimateChgCause_lead2_z"
main = "Climate Change is Human Caused +2"
ylab = "Climate Change is Human Caused (SD)"
sub = "Climate change is caused by humans"
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

summary(pool(out_m))
## g-computation

out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct

humancaused2_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
humancaused2_c

# show table
# graph
humancaused2_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

humancaused2_p




#  Env.ClimateChgCause_lead3_z ------------------------------------------------------------
Y = "Env.ClimateChgCause_lead3_z"
main = "Climate Change is Human Caused +3"
ylab = "Climate Change is Human Caused (SD)"
sub = "Climate change is caused by humans"
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

summary(pool(out_m))
## g-computation

out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct

humancaused3_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
humancaused3_c

# show table
# graph
humancaused3_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

humancaused3_p


#  Env.CarbonRegs_lead1_z ------------------------------------------------------------
Y = "Env.ClimateChgCause_lead4_z"
main = "Climate Change is Human Caused +4"
ylab = "Climate Change is Human Caused (SD)"
sub = "Climate change is caused by humans"
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

summary(pool(out_m))
## g-computation

out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct

humancaused4_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
humancaused4_c

# 
# 
# ## table for all contrasts (exploratory )
# carbon1_t <- out_ct %>%
#   slice(1:max) |>
#   tibble() |>
#   rename(
#     Contrast = row,
#     Estimate = est,
#     Std_error = se,
#     CI_hi = ui,
#     CI_lo = li
#   ) |>
#   kbl(caption = main,
#       digits = 3,
#       "html") |>
#   kable_styling() %>%
#   row_spec(c(f + 1 - min),
#            bold = T,
#            color = "white",
#            background = "dodgerblue") |>
#   kable_minimal(full_width = F)

# show table
# graph
humancaused4_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

humancaused4_p
humancaused4_p


# Climate concern ---------------------------------------------------------


#  Env.ClimateChgConcern_lead2_z ------------------------------------------------------------
Y = "Env.ClimateChgConcern_lead2_z"
main = "Climate Concern +2"
ylab = "Climate Concern (SD)"
sub = "I am deeply concerned about climate change."
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

summary(pool(out_m))
## g-computation

out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct

climateconcern2_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
climateconcern2_c

# show table
# graph
climateconcern2_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


#  Env.ClimateChgConcern_lead3_z ------------------------------------------------------------
Y = "Env.ClimateChgConcern_lead3_z"
main = "Climate Concern +3"
ylab = "Climate Concern (SD)"
sub = "I am deeply concerned about climate change."
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

summary(pool(out_m))
## g-computation

out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct

climateconcern3_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
climateconcern3_c

# show table
# graph
climateconcern3_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


climateconcern3_p



#  Env.ClimateChgConcern_lead4_z ------------------------------------------------------------
Y = "Env.ClimateChgConcern_lead4_z"
main = "Climate Concern +4"
ylab = "Climate Concern (SD)"
sub = "I am deeply concerned about climate change."
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

summary(pool(out_m))
## g-computation

out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct

climateconcern4_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
climateconcern4_c

# show table
# graph
climateconcern4_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


climateconcern4_p


#  Env.CarbonRegs_lead1_z ------------------------------------------------------------
Y = "Env.CarbonRegs_lead1_z"
main = "Carbon Regulation +1"
ylab = "Carbon Regulation (SD)"
sub = "Government regulation of carbon emissions"
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

summary(pool(out_m))
## g-computation

out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )
out_ct

carbon1_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
carbon1_c

# 
# 
# ## table for all contrasts (exploratory )
# carbon1_t <- out_ct %>%
#   slice(1:max) |>
#   tibble() |>
#   rename(
#     Contrast = row,
#     Estimate = est,
#     Std_error = se,
#     CI_hi = ui,
#     CI_lo = li
#   ) |>
#   kbl(caption = main,
#       digits = 3,
#       "html") |>
#   kable_styling() %>%
#   row_spec(c(f + 1 - min),
#            bold = T,
#            color = "white",
#            background = "dodgerblue") |>
#   kable_minimal(full_width = F)

# show table
# graph
carbon1_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

carbon1_p



# Env.SatNZEnvironment_lead2_z ----------------------------------------------------------

Y = "Env.SatNZEnvironment_lead2_z"
main = "Satisfied with NZ Environment"
ylab = "Satisifaction with NZ Environment (SD)"
sub = "The quality of New Zealand’s natural environment."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

# coef + estimate
satnzenv2_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
satnzenv2_c

# graph
satnzenv2_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
satnzenv2_p



# Env.SatNZEnvironment_lead3_z ----------------------------------------------------------

Y = "Env.SatNZEnvironment_lead3_z"
main = "Satisfied with NZ Environment + 3"
ylab = "Satisifaction with NZ Environment (SD)"
sub = "The quality of New Zealand’s natural environment."
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

# coef + estimate
satnzenv3_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
satnzenv3_c

# graph
satnzenv3_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


satnzenv3_p

# Env.SatNZEnvironment_lead4_z ----------------------------------------------------------

Y = "Env.SatNZEnvironment_lead4_z"
main = "Satisfied with NZ Environment + 4"
ylab = "Satisifaction with NZ Environment (SD)"
sub = "The quality of New Zealand’s natural environment."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

# coef + estimate
satnzenv4_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
satnzenv4_c

# graph
satnzenv4_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

satnzenv4_p

# Env.SatNZEnvironment_lead4_z ----------------------------------------------------------

Y = "Env.SatNZEnvironment_lead4_z"
main = "Satisfied with NZ Environment + 4"
ylab = "Satisifaction with NZ Environment (SD)"
sub = "The quality of New Zealand’s natural environment."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

# coef + estimate
satnzenv4_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
satnzenv4_c

# graph
satnzenv4_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

satnzenv4_p


# Env.SatNZEnvironment_lead5_z ----------------------------------------------------------

Y = "Env.SatNZEnvironment_lead5_z"
main = "Satisfied with NZ Environment + 5"
ylab = "Satisifaction with NZ Environment (SD)"
sub = "The quality of New Zealand’s natural environment."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

# coef + estimate
satnzenv5_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
satnzenv5_c

# graph
satnzenv5_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )

satnzenv5_p

# Env.Native.Species_lead3_z ----------------------------------------------------------

Y = "Env.Native.Species_lead3_z"
main = "Protecting NZ Species +3"
ylab = "Protecting NZ Species (SD)"
sub = "Protecting New Zealand’s native\nspecies should be a national priority."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

# coef + estimate
natspecies3_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
natspecies3_c

# graph
natspecies3_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


natspecies3_p


# Env.Native.Species_lead4_z ----------------------------------------------------------

Y = "Env.Native.Species_lead4_z"
main = "Protecting NZ Species +4"
ylab = "Protecting NZ Species (SD)"
sub = "Protecting New Zealand’s native\nspecies should be a national priority."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

# coef + estimate
natspecies4_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
natspecies4_c

# graph
natspecies4_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


natspecies4_p



# Env.Possum.Control_lead3 ----------------------------------------------------------

Y = "Env.Possum.Control_lead3_z"
main = "Possum Control +3"
ylab = "Possum Control (SD)"
sub = "Do you support the use of 1080 poison\nfor possum control in New Zealand?"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

# coef + estimate
possum3_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
possum3_c

# graph
possum3_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


possum3_p




# Env.Possum.Control_lead4 ----------------------------------------------------------

Y = "Env.Possum.Control_lead4_z"
main = "Possum Control +4"
ylab = "Possum Control (SD)"
sub = "Do you support the use of 1080 poison\nfor possum control in New Zealand?"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

# coef + estimate
possum4_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
possum4_c

# graph
possum4_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


possum4_p



# Env.Eff01.Actionfeeling_lead4_z ----------------------------------------------------------

Y = "Env.Eff02.ActionFeeling_lead4_z"
main = "Action Feeling +4"
ylab = "Action Feeling (SD)"
sub = "I feel I can make a difference to\nthe state of the environment."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

# coef + estimate
feeling4_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
feeling4_c

# graph
feeling4_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


feeling4_p



# Env.Eff01.ActionBelief_lead4_z ----------------------------------------------------------

Y = "Env.Eff01.ActionBelief_lead4_z"
main = "Action Belief +4"
ylab = "Action Belief (SD)"
sub = "By taking personal action I believe\nI can make a positive difference to environmental problems."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

# coef + estimate
action4_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
action4_c

# graph
action4_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


action4_p





# Env.SacWilling_lead4_z ----------------------------------------------------------

Y = "Env.SacWilling_lead4_z "
main = "Sacrifice Willing +4"
ylab = "Sacrifice Willing(SD)"
sub = "Are you willing to make sacrifices to your standard of living\n(e.g., accepted higher prices, driven less,\nconserved energy) in order to protect the environment?"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

# coef + estimate
sacrificewilling4_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
sacrificewilling4_c

# graph
sacrificewilling4_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


sacrificewilling4_c



# Env.SacMade_lead4_z ----------------------------------------------------------

Y = "Env.SacMade_lead4_z "
main = "Sacrifice Made +4"
ylab = "Sacrifice Made(SD)"
sub = "Have you made sacrifices to your standard of living\n(e.g., accepted higher prices, driven less,\nconserved energy) in order to protect the environment?"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

# coef + estimate
sacrificemade4_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
sacrificemade4_c

# graph
sacrificemade4_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


sacrificemade4_p



# Env.SacNorms_lead4_z ----------------------------------------------------------
#Do you think most New Zealanders are willing to make sacrifices to their standard of living in order to protect the environment?

Y = "Env.SacNorms_lead4_z"
main = "Sacrifice Norms +4"
ylab = "Sacrifice Norms(SD)"
sub = "Do you think most New Zealanders are willing to make sacrifices\nto their standard of living in order to protect the environment?"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

# coef + estimate
sacrificenorms4_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
sacrificenorms4_c

# graph
sacrificenorms4_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


sacrificenorms4_p




#       Env.MotorwaySpend_lead5_z ----------------------------------------------------------
#Do you think most New Zealanders are willing to make sacrifices to their standard of living in order to protect the environment?

Y = "Env.MotorwaySpend_lead5_z"
main = "Motorways Spending +5"
ylab = "Motorways Spending (SD)"
sub = "Increased government spending on new motorways."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

# coef + estimate
motorway5_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
motorway5_c

# graph
motorway5_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


motorway5_p



#       Env.MotorwaySpend_lead5_z ----------------------------------------------------------
#Do you think most New Zealanders are willing to make sacrifices to their standard of living in order to protect the environment?

Y = " Env.PubTransSubs_lead5_z"
main = "Public Transport Subsidy +5"
ylab = "Public Transport Subsidy (SD)"
sub = "Government subsidy of public transport."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

# coef + estimate
publictransport5_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
publictransport5_c

# graph
publictransport5_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )


publictransport5_p



# COMPARE TABLES  --------------------------------------------------
main = "Comparison of Year-wise Causal Effects (Stated Environmental Attitudes) / Evalues"
h_tab <- rbind(
  humancaused2_c,
  humancaused3_c,
  humancaused4_c,
  climateconcern2_c,
  climateconcern3_c,
  climateconcern4_c,
  satnzenv3_c,
  satnzenv4_c,
  satnzenv5_c,
  natspecies3_c,
  natspecies4_c,
  possum3_c,
  possum4_c
)

h_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  #kable_styling() %>%
  # row_spec(c(1:5,8:11),  # Bold out the lines where EVALUES do not cross zero or for ratios, 1
  #          bold = T,
  #          # color = "black",
  #          background = "bold") |>
  kable_minimal(full_width = F)





# TABLE STATED ATTITUDES --------------------------------------------------
main = "Stated Environmental Attitudes / Evalues"
h_tab <- rbind(#satnzenv3_c,
  humancaused4_c,
  climateconcern4_c,
  satnzenv4_c,
  #  satnzenv5_c,
  #  natspecies3_c,
  natspecies4_c,
  #  possum3_c,
  action4_c,
  feeling4_c,
  sacrificewilling4_c,
  sacrificemade4_c
  #  sacrificenorms4_c
)

h_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(3,7:9),  # Bold out the lines where EVALUES do not cross zero or for ratios, 1
           bold = T,
           # color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)




# TABLE REVEALED ATTITUDES ------------------------------------------------

main = "Revealed Environmental Attitudes / Evalues"
h_tab <- rbind(carbon1_c,
               possum4_c,
               motorway5_c,
               publictransport5_c)

h_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  #kable_styling() %>%
  row_spec(c(1),  # Bold out the lines where EVALUES do not cross zero or for ratios, 1
           bold = T,
           # color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)


# GRAPHS STATED VALUES ----------------------------------------------------


reflective_plots <- #satnzenv3_p +
  humancaused4_p +
  climateconcern4_p +
  satnzenv4_p +
  #satnzenv5_p +
  #natspecies3_p +
  natspecies4_p +
  #possum3_p +
  possum4_p +
  action4_p +
  feeling4_p +
  sacrificewilling4_p +
  sacrificemade4_p +
  # sacrificenorms4_p +
  plot_annotation(title = "Causal effects of political orientation on stated environmental values") +
  plot_layout(guides = 'collect')

reflective_plots

# save

ggsave(
  reflective_plots,
  path = here::here(here::here("figs", "figs_pols", "reflective_plots")),
  width = 9,
  height = 9,
  units = "in",
  filename = "reflective_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


# REVEALED PLOTS ----------------------------------------------------------


revealed_plots <- carbon1_p +
  possum4_p +
  motorway5_p +
  publictransport5_p +
  plot_annotation(title = "Causal effects of political orientation on revealed environmental values") +
  plot_layout(guides = 'collect')

revealed_plots

# save

ggsave(
  revealed_plots,
  path = here::here(here::here("figs", "figs_pols", "revealed_plots")),
  width = 9,
  height = 12,
  units = "in",
  filename = "revealed_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)
# save






##
# Confounding control variables  ---------------------------------------------------------
# These variables can be modified depending on your model and assumptions.
#  Here, we use vanderweele's "disjunctive cause criterion"

# FROM Outcomewide longitudinal designs: https://doi.org/10.1214/19-STS728
#" A modified disjunctive cause criterion that might thus be more useful in practice could articulated as follows (VanderWeele, 2019): control for each covari- ate that is a cause of the exposure, or of the outcome, or of both; exclude from this set any variable known to be an instrumental variable; and include as a covariate any proxy for an unmeasured variable that is a common cause of both the exposure and the outcome." p.443

# TYLERS LIST,  https://doi.org/10.1214/19-STS728 p.442
# *** Demographic
# Race
# Age
# Gender
# Marital Status
# *** Economic, Social and Political
# Income
# Education
# Employment
# Social integration Neighborhood
# Religious service attendance
# Political affiliation
### *** Health
# Self-rated health
# Number of health conditions
# Exercise
# Smoking
# Alcohol consumption
# Depression
# Happiness Loneliness
# Parental warmth Purpose/Meaning Big five personality

# NOTE: WE USE MORE VARIABLES

