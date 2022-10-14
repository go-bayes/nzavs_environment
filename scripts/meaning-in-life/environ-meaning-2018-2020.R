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



###############  RENAME YOUR IMPUTED DATASET  'df"

df <- readh("ml_meaning_environ")

mf <- mice::complete(df)
min(mf$LIFEMEANING_lead1_z)
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

#Climate change is caused by humans.
# hist(data_long$Env.ClimateChgCause_lead1)
# sd(data_long$Env.ClimateChgCause_lead1)
# min(data_long$Env.ClimateChgCause_lead1_z)
# max(data_long$Env.ClimateChgCause_lead1_z)


############### NEXT SET UP VARIABLES FOR MODELS AND GRAPHS
X = "LIFEMEANING_lead1_z"

# You may set your label for your graphs  HERE WE STICK TO THE EXAMPLE OF WORK
xlab = "My life has a clear sense of purpose.\nI have a good sense of what makes my life meaningful."
# Steger, M. F., Frazier, P., Oishi, S., & Kaler, M. (2006). The Meaning in Life Questionnaire: Assessing the presence of and search for meaning in life. Journal of Counseling Psychology, 53, 80-93.

#Climate change is caused by humans.

# SET THE RANGE
min = -2
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

ylim = c(-.2, .3)  # SET AS YOU LIKE -- here, how much movement across a standard deviation unit of the outcome
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
  "LIFEMEANING_z",
  "LIFESAT_z",
  "NZdep_z",
  "NWI_z",
  "NZSEI13_z",
  "Parent_z",
  "Partner_z",
  "Pol.Orient_z",
  "Relid_z",
  "Respect.Self_z",
  # "SCIENCE.TRUST_z",
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

Y = "Env.ClimateChgReal_lead2_z"
main = "Climate Change is real +2"
ylab = "Climate change is real (SD)"
sub = "Climate change is real"
# regression
out_m <- mice_gaussian(df = df,
                       X = X,
                       Y = Y,
                       cvars = cvars)

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

real2_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
real2_c

# show table
# graph
real2_p <-
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

real2_p



ggsave(
  real2_p,
  path = here::here(here::here("figs", "figs_meaning")),
  width = 16,
  height = 9,
  units = "in",
  filename = "1_real2_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)

#  Env.ClimateChgCause_lead2_z ------------------------------------------------------------
Y = "Env.ClimateChgCause_lead2_z"
main = "Climate Change is Human Caused +2"
ylab = "Climate Change is Human Caused (SD)"
sub = "Climate change is caused by humans"
# regression
out_m <- mice_gaussian(df = df,
                       X = X,
                       Y = Y,
                       cvars = cvars)

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


ggsave(
  humancaused2_p,
  path = here::here(here::here("figs", "figs_meaning")),
  width = 16,
  height = 9,
  units = "in",
  filename = "2_humancaused2_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)




#  Env.ClimateChgConcern_lead2_z ------------------------------------------------------------
Y = "Env.ClimateChgConcern_lead2_z"
main = "Climate Concern +2"
ylab = "Climate Concern (SD)"
sub = "I am deeply concerned about climate change."
# regression
out_m <- mice_gaussian(df = df,
                       X = X,
                       Y = Y,
                       cvars = cvars)

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

climateconcern2_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
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

climateconcern2_p
ggsave(
  humancaused2_p,
  path = here::here(here::here("figs", "figs_meaning")),
  width = 16,
  height = 9,
  units = "in",
  filename = "3_climateconcern2_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


# Env.SatNZEnvironment_lead2_z ----------------------------------------------------------

Y = "Env.SatNZEnvironment_lead2_z"
main = "Satisfied with NZ Environment"
ylab = "Satisifaction with NZ Environment (SD)"
sub = "The quality of New Zealand’s natural environment."

# regression
out_m <- mice_gaussian(df = df,
                       X = X,
                       Y = Y,
                       cvars = cvars)

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


ggsave(
  satnzenv2_p,
  path = here::here(here::here("figs", "figs_meaning")),
  width = 16,
  height = 9,
  units = "in",
  filename = "4_satnzenv2_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)



# Env.Eff2 ----------------------------------------------------------

Y = "Env.Eff_lead2_z"
main = "Environmental Efficacy +2"
ylab = "Action Belief (SD)"
sub = "I feel I can make a difference to\nthe state of the environment.\nBy taking personal action I believe\nI can make a positive difference to environmental problems."

# regression
out_m <- mice_gaussian(df = df,
                       X = X,
                       Y = Y,
                       cvars = cvars)

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
efficacy2_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
efficacy2_c

# graph
efficacy2_p <-
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


efficacy2_p


ggsave(
  efficacy2_p,
  path = here::here(here::here("figs", "figs_meaning")),
  width = 16,
  height = 9,
  units = "in",
  filename = "5_efficacy2_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


# COMPARE TABLES  --------------------------------------------------
main = "Comparison of Year-wise Causal Effects (Stated Environmental Attitudes) / Evalues"
h_tab <- rbind(#  humancaused2_c,
  real2_c,
  humancaused2_c,
  climateconcern2_c,
  satnzenv2_c,
  efficacy2_c)

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




# graphs ------------------------------------------------------------------

revealed_plots <-  real2_p +
  humancaused2_p +
  climateconcern2_p +
  climateconcern3_p +
  climateconcern4_p +
  satnzenv2_p +
  efficacy2_p +
  plot_annotation(title = "Causal effects of political orientation on environmental values") +
  plot_layout(guides = 'collect')

revealed_plots

# save

ggsave(
  revealed_plots,
  path = here::here(here::here("figs", "figs_meaning", "revealed_plots")),
  width = 16,
  height = 12,
  units = "in",
  filename = "revealed_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


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
