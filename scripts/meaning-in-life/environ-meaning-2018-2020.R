## Concern study

#template_outcomewide.R

# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")



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


###### MAKE FOLDER CALLED "data"  #########

###### MAKE FOLDER CALLED "figs"  #########

###### READ THIS DATA IN   #########



###############  RENAME YOUR IMPUTED DATASET  'df"

df <- readh("ml_science_environ")

#df<- data_imputed

############### SET YOUR EXPOSURE VARIABLE,
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


data_long$Env.ClimateChgCause_lead1
X = "SCIENCE.TRUST_lead1_z"
#Climate change is caused by humans.
# hist(data_long$Env.ClimateChgCause_lead1)
# sd(data_long$Env.ClimateChgCause_lead1)
# min(data_long$Env.ClimateChgCause_lead1_z)
# max(data_long$Env.ClimateChgCause_lead1_z)


############### NEXT SET UP VARIABLES FOR MODELS AND GRAPHS

# You may set your label for your graphs  HERE WE STICK TO THE EXAMPLE OF WORK
xlab = "I have a high degree of confidence in the scientific community.\nOur society places too much emphasis on science.(n) (SD)"  ## Weekly hours devided by 10

#Climate change is caused by humans.

# SET THE RANGE
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

ylim = c(-.1,.25)  # SET AS YOU LIKE -- here, how much movement across a standard deviation unit of the outcome
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
 # "Church_z",
  "NeighbourhoodCommunity_z",
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
  "SCIENCE.TRUST_z",
  # "Rumination_z",
  "SELF.CONTROL_z",
  "SELF.ESTEEM_z",
  "SFHEALTH_z",
  "Smoker_z",
  "Standard.Living_z",
  "SUPPORT_z",
  "Urban_z",
 # "Volunteers_z",
  "Your.Health_z",
  "Your.Future.Security_z",
  "Your.Personal.Relationships_z",
  "Env.ClimateChgReal_z",
  "Env.ClimateChgCause_z",
  "Env.ClimateChgConcern_z",
  "Env.SatNZEnvironment_z",
  "Env.MotorwaySpend_z",
  "Env.PubTransSubs_z"

)
cvars
# Env.ClimateChgReal,
# Env.ClimateChgCause,
# Env.ClimateChgConcern,
# Env.SatNZEnvironment,
# Env.Eff01.ActionBelief,
# Env.Eff02.ActionFeeling,



################# BELOW THE MANY OUTCOMES!  ########################################


# Env.ClimateChgReal_lead2_z ----------------------------------------------

Y = "Env.ClimateChgReal_lead1_z"
main = "Climate Change is real +1"
ylab = "Climate change is real (SD)"
sub = "Climate change is real"
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

real1_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
real1_c

# show table
# graph
real1_p <-
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

real1_p


ggsave(
  real1_p,
  path = here::here(here::here("figs", "figs_science")),
  width = 16,
  height = 9,
  units = "in",
  filename = "1_real1_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


#  Env.ClimateChgCause_lead1_z ------------------------------------------------------------
Y = "Env.ClimateChgCause_lead1_z"
main = "Climate Change is Human Caused +1"
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

humancaused1_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
humancaused1_c

# show table
# graph
humancaused1_p <-
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

humancaused1_p

ggsave(
  humancaused1_p,
  path = here::here(here::here("figs", "figs_science")),
  width = 16,
  height = 9,
  units = "in",
  filename = "2_humancaused1_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)

#  Env.ClimateChgConcern_lead1_z ------------------------------------------------------------
Y = "Env.ClimateChgConcern_lead1_z"
main = "Climate Concern +1"
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

climateconcern1_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
climateconcern1_c

# show table
# graph
climateconcern1_p <-
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
climateconcern1_p



ggsave(
  climateconcern1_p,
  path = here::here(here::here("figs", "figs_science")),
  width = 16,
  height = 9,
  units = "in",
  filename = "3_climateconcern1_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


# Env.SatNZEnvironment_lead2_z ----------------------------------------------------------

Y = "Env.SatNZEnvironment_lead1_z"
main = "Satisfied with NZ Environment + 1"
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
satnzenv1_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
satnzenv1_c

# graph
satnzenv1_p <-
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
satnzenv1_p



ggsave(
  satnzenv1_p,
  path = here::here(here::here("figs", "figs_science")),
  width = 16,
  height = 9,
  units = "in",
  filename = "4_satnzenv1_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)



# Env.Eff01.Actionfeeling_lead4_z ----------------------------------------------------------

Y = "Env.Eff02.ActionFeeling_lead1_z"
main = "Action Feeling +1"
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
feeling1_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
feeling1_c

# graph
feeling1_p <-
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


feeling1_p


ggsave(
  feeling1_p,
  path = here::here(here::here("figs", "figs_science")),
  width = 16,
  height = 9,
  units = "in",
  filename = "5_feeling1_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)

# Env.Eff01.ActionBelief_lead4_z ----------------------------------------------------------

Y = "Env.Eff01.ActionBelief_lead1_z"
main = "Action Belief +1"
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
action1_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
action1_c

# graph
action1_p <-
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


action1_p

ggsave(
  action1_p,
  path = here::here(here::here("figs", "figs_science")),
  width = 16,
  height = 9,
  units = "in",
  filename = "6_action1_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


# COMPARE TABLES  --------------------------------------------------
main = "Comparison of Year-wise Causal Effects (Stated Environmental Attitudes) / Evalues"

h_tab <- rbind(
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



#sacrificenorms4_c

# TABLE STATED ATTITUDES --------------------------------------------------
main = "Causal effects of trust in science on Environmental Beliefs / Evalues"
h_tab <- rbind(real1_c,
               humancaused1_c,
               climateconcern1_c,
               satnzenv1_c,
               feeling1_c,
               action1_c
)

h_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  #kable_styling() %>%
  row_spec(c(1:3,5,6),  # Bold out the lines where EVALUES do not cross zero or for ratios, 1
           bold = T,
           # color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)


real1_c
humancaused1_c
climateconcern1_c
satnzenv1_c
feeling1_c
action1_c

# GRAPHS  ----------------------------------------------------

#satnzenv4_p
reflective_plots <- real1_p +
humancaused1_p +
climateconcern1_p +
satnzenv1_p +
feeling1_p +
action1_p +
  plot_annotation(title = "Causal Effects of Trust in Science on Environmental Beliefs") +
  plot_layout(guides = 'collect')

reflective_plots

# save

ggsave(
  reflective_plots,
  path = here::here(here::here("figs", "figs_science")),
  width = 16,
  height = 12,
  units = "in",
  filename = "science_environ_graphs.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


# REVEALED PLOTS ----------------------------------------------------------


revealed_plots <- carbon1_p +
  possum4_p +
  motorway5_p +
  publictransport5_p +
  plot_annotation(title = "Causal effects of belief in human caused climate change on revealed environmental values") +
  plot_layout(guides = 'collect')

revealed_plots

# save

ggsave(
  revealed_plots,
  path = here::here(here::here("figs", "figs_human_cause", "revealed_plots")),
  width = 16,
  height = 12,
  units = "in",
  filename = "revealed_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)
# save




# individual plots --------------------------------------------------------

# individual stated plots -----------------------------------------------

ggsave(
  climateconcern4_p,
  path = here::here(here::here("figs", "figs_human_cause")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "0_climateconcern4_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)




ggsave(
  satnzenv4_p,
  path = here::here(here::here("figs", "figs_human_cause")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "1_satnzenv4_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)

ggsave(
  natspecies4_p,
  path = here::here(here::here("figs", "figs_human_cause")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "2_natspecies4_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)



ggsave(
  action4_p,
  path = here::here(here::here("figs", "figs_human_cause")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "3_action4_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


ggsave(
  feeling4_p,
  path = here::here(here::here("figs", "figs_human_cause")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "4_feeling4_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


ggsave(
  sacrificewilling4_p,
  path = here::here(here::here("figs", "figs_human_cause")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "5_sacrificewilling4_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


ggsave(
  sacrificemade4_p,
  path = here::here(here::here("figs", "figs_human_cause")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "6_sacrificemade4_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)



ggsave(
  sacrificenorms4_p,
  path = here::here(here::here("figs", "figs_human_cause")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "7_sacrificenorms4_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


# individual revealed plots  ----------------------------------------------

ggsave(
  carbon1_p,
  path = here::here(here::here("figs", "figs_human_cause")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "8_carbon1_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


ggsave(
  possum4_p,
  path = here::here(here::here("figs", "figs_human_cause")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "9_possum4_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)



ggsave(
  motorway5_p,
  path = here::here(here::here("figs", "figs_human_cause")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "10_motorway5_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)



ggsave(
  publictransport5_p,
  path = here::here(here::here("figs", "figs_human_cause")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "11_publictransport5_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)





# GROWTH CURVE ------------------------------------------------------------



data_ml <- readRDS(here::here("data", "data_ml"))
library(lme4)


## cvars remove prior outcome 

cvars2 = c(
  #"Env.ClimateChgCause", # move exposure to front 
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
  "Env.RoutineMade",
  "Env.RoutineWilling",
  "Env.ClimateChgReal",
  "Env.ClimateChgConcern",
  # "Env.SatNZEnvironment", # blocked out
  "Env.Native.Species",
  "Env.Possum.Control",
  "Env.Eff01.ActionBelief",
  "Env.Eff02.ActionFeeling",
  "Env.CarbonRegs"
)


f1<- as.formula(paste(
  paste("Env.SatNZEnvironment","~", X,"+ (1|Id)"),
  paste(cvars2,collapse = "+")))
f1
m0 <- lmer(f1, data =   data_ml)



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
