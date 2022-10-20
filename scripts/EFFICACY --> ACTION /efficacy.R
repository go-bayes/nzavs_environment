


# ENVIRON NORMS

## Concern study

#template_outcomewide.R

# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")
#
# conflict_prefer("pool", "mice")
# conflict_prefer("cbind", "base")

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

#df <- readh("ml_pol_orient_environ_omni_wave5")

df  <- readh("ml_environ_5_efficacy")

summary(df$data$Env.Eff_lead1_z)

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

#data_long$Env.SacNorms_lead1


# dplyr::mutate(Env.Eff_lead1 = mean(
#   c(Env.Eff01.ActionBelief_lead1, Env.Eff02.ActionFeeling_lead1)
#

X = "Env.Eff_lead1_z"  ## Weekly hours devided by 10


#
#
# hist(data_long$Env.SacNorms_lead1_z)
# sd(data_long$Env.SacNorms_lead1)
# min(data_long$Env.SacNorms_lead1_z)
# max(data_long$Env.SacNorms_lead1_z)

############### NEXT SET UP VARIABLES FOR MODELS AND GRAPHS

# You may set your label for your graphs  HERE WE STICK TO THE EXAMPLE OF WORK
xlab = "By taking personal action I believe I can make a positive difference to environmental problems."

hist(data_long$Env.Eff_lead1_z)
# SET THE RANGE for our natural experiment (here from -1 pol.orient to 1)
min = -2.5
max =  1.5


#cor(data_long$Env.Eff01.ActionBelief_lead1_z,data_long$Env.Eff02.ActionFeeling_lead1_z)  #.78

# set full range of X
x =  min:max

# baseline
r = -.5

# focal contrast for X
f = 1.5

# REQUIRED for certain model model functions
c = x

# contrast for graphs -- absolute distance from baseline
p = c(r, f) #


# Needed for E-VALUES -- how much do we move on the X scale to obtain our effect?
#delta = 4 #
delta = abs(r - f)

ylim = c(-.2, .2)  # SET AS YOU LIKE -- here, how much movement across a standard deviation unit of the outcome
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
  "Env.CarbonRegs_z",
  "Env.MotorwaySpend_z",
  "Env.PubTransSubs_z",
  "Env.Native.Species_z",
  "Env.SacNorms_z",
  "Env.SacMade_z",
  "Env.SacWilling_z",
  "Env.RoutineMade_z",
  "Env.RoutineWilling_z",
  "Env.ClimateChgReal_z",
  "Env.ClimateChgCause_z",
  "Env.ClimateChgConcern_z",
  "Env.SatNZEnvironment_z",
  "Env.Native.Species_z",
  "Env.Possum.Control_z",
  "Env.Eff01.ActionBelief_z",
  "Env.Eff02.ActionFeeling_z",
  "Env.CarbonRegs_z"
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



# Env.ClimateChgReal_lead3_z ----------------------------------------------

Y = "Env.ClimateChgReal_lead3_z"
main = "Climate Change is real +3"
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

real3_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
real3_c

# show table
# graph
real3_p <-
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

real3_p


# Env.ClimateChgReal_lead4_z ----------------------------------------------

Y = "Env.ClimateChgReal_lead4_z"
main = "Climate Change is real +4"
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

real4_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
real4_c

# show table
# graph
real4_p <-
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

real4_p




# Env.ClimateChgReal_lead5_z ----------------------------------------------
# 
# Y = "Env.ClimateChgReal_lead5_z"
# main = "Climate Change is real +5"
# ylab = "Climate change is real (SD)"
# sub = "Climate change is real"
# # regression
# out_m <- mice_gaussian(df = df,
#                        X = X,
#                        Y = Y,
#                        cvars = cvars)
# 
# summary(pool(out_m))
# ## g-computation
# 
# out_ct <-
#   pool_stglm_contrast(
#     out_m,
#     df = df,
#     m = 10,
#     X = X,
#     x = x,
#     r = r
#   )
# out_ct
# 
# real5_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
# real5_c
# 
# # show table
# # graph
# real5_p <-
#   ggplot_stglm(
#     out_ct,
#     ylim = ylim,
#     main,
#     xlab,
#     ylab,
#     min = min,
#     p = p,
#     sub = sub
#   )
# 
# real5_p
# 
# 


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



dev.off()
#  Env.ClimateChgCause_lead3_z ------------------------------------------------------------
Y = "Env.ClimateChgCause_lead3_z"
main = "Climate Change is Human Caused +3"
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


#  Env.ClimateChgConcern_lead3_z ------------------------------------------------------------
Y = "Env.ClimateChgConcern_lead3_z"
main = "Climate Concern +3"
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

climateconcern3_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
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

climateconcern4_c <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
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



# Env.SatNZEnvironment_lead3_z ----------------------------------------------------------

Y = "Env.SatNZEnvironment_lead3_z"
main = "Satisfied with NZ Environment + 3"
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



# # Env.Eff01.Actionfeeling_lead4_z ----------------------------------------------------------
#
# Y = "Env.Eff02.ActionFeeling_lead4_z"
# main = "Action Feeling +4"
# ylab = "Action Feeling (SD)"
# sub = "I feel I can make a difference to\nthe state of the environment."
#
# # regression
# out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
#
# ## g-computation
# out_ct <-
#   pool_stglm_contrast(
#     out_m,
#     df = df,
#     m = 10,
#     X = X,
#     x = x,
#     r = r
#   )
#
# # coef + estimate
# feeling4_c <-
#   vanderweelevalue_ols(out_ct, f - min, delta, sd)
# feeling4_c
#
# # graph
# feeling4_p <-
#   ggplot_stglm(
#     out_ct,
#     ylim = ylim,
#     main,
#     xlab,
#     ylab,
#     min = min,
#     p = p,
#     sub = sub
#   )
#
#
# feeling4_p


#
# # Env.Eff01.ActionBelief_lead4_z ----------------------------------------------------------
#
# Y = "Env.Eff01.ActionBelief_lead4_z"
# main = "Action Belief +4"
# ylab = "Action Belief (SD)"
# sub = "By taking personal action I believe\nI can make a positive difference to environmental problems."
#
# # regression
# out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
#
# ## g-computation
# out_ct <-
#   pool_stglm_contrast(
#     out_m,
#     df = df,
#     m = 10,
#     X = X,
#     x = x,
#     r = r
#   )
#
# # coef + estimate
# action4_c <-
#   vanderweelevalue_ols(out_ct, f - min, delta, sd)
# action4_c
#
# # graph
# action4_p <-
#   ggplot_stglm(
#     out_ct,
#     ylim = ylim,
#     main,
#     xlab,
#     ylab,
#     min = min,
#     p = p,
#     sub = sub
#   )
#
#
# action4_p
#
#



# Env.SacWilling_lead4_z ----------------------------------------------------------

Y = "Env.SacWilling_lead4_z "
main = "Sacrifice Willing +4"
ylab = "Sacrifice Willing(SD)"
sub = "Are you willing to make sacrifices to your standard of living\n(e.g., accepted higher prices, driven less,\nconserved energy) in order to protect the environment?"

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


sacrificewilling4_p



# Env.SacMade_lead4_z ----------------------------------------------------------

Y = "Env.SacMade_lead4_z "
main = "Sacrifice Made +4"
ylab = "Sacrifice Made(SD)"
sub = "Have you made sacrifices to your standard of living\n(e.g., accepted higher prices, driven less,\nconserved energy) in order to protect the environment?"

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
  #  humancaused2_c,
  real2_c,
  real3_c,
  real4_c,
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

dev.off()
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
h_tab <- rbind(
  #satnzenv3_c,
  humancaused4_c,
  climateconcern4_c,
  satnzenv4_c,
  #  satnzenv5_c,
  #  natspecies3_c,
  natspecies4_c,
  #  possum3_c,
  # action4_c,
  #  feeling4_c,
  sacrificewilling4_c,
  sacrificemade4_c
  #  sacrificenorms4_c
)

h_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  #  kable_styling() %>%
  row_spec(c(2, 4, 5, 6),  # Bold out the lines where EVALUES do not cross zero or for ratios, 1
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
  # action4_p +
  # feeling4_p +
  sacrificewilling4_p +
  sacrificemade4_p +
  # sacrificenorms4_p +
  plot_annotation(title = "Causal effects of change in efficacy beliefs (+1sd -> 0)\non stated environmental values") +
  plot_layout(guides = 'collect')

reflective_plots

# save

ggsave(
  reflective_plots,
  path = here::here(here::here(
    "figs", "figs_efficacy", "reflective_plots"
  )),
  width = 16,
  height = 12,
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
  plot_annotation(title = "Causal effects of efficacy beliefs (+1sd -> 0)\non revealed environmental values") +
  plot_layout(guides = 'collect')

revealed_plots

# save
path = here::here(here::here("figs", "figs_efficacy", "reflective_plots")),

ggsave(
  revealed_plots,
  path = here::here(here::here(
    "figs", "figs_efficacy",  "revealed_plots"
  )),
  width = 16,
  height = 12,
  units = "in",
  filename = "revealed_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)
# save


# Individual plots


# individual stated plots -----------------------------------------------

ggsave(
  real4_p,
  path = here::here(here::here("figs", "figs_efficacy")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "-2_real4_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)





ggsave(
  humancaused4_p,
  path = here::here(here::here("figs", "figs_efficacy")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "-1_humancaused4_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)




ggsave(
  climateconcern4_p,
  path = here::here(here::here("figs", "figs_efficacy")),
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
  path = here::here(here::here("figs", "figs_efficacy")),
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
  path = here::here(here::here("figs", "figs_efficacy")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "2_natspecies4_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


#
# ggsave(
#   action4_p,
#   path = here::here(here::here("figs", "figs_efficacy")),
#   width = 12.8,
#   height = 7.2,
#   units = "in",
#   filename = "3_action4_p.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 600
# )
#

sacrificenorms4_p
ggsave(
  sacrificenorms4_p,
  path = here::here(here::here("figs", "figs_efficacy")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "3_sacrificenorms4_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)

ggsave(
  sacrificewilling4_p,
  path = here::here(here::here("figs", "figs_efficacy")),
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
  path = here::here(here::here("figs", "figs_efficacy")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "6_sacrificemade4_p.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


#
# ggsave(
#   sacrificenorms4_p,
#   path = here::here(here::here("figs", "figs_efficacy")),
#   width = 12.8,
#   height = 7.2,
#   units = "in",
#   filename = "7_sacrificenorms4_p.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 600
# )


# individual revealed plots  ----------------------------------------------

ggsave(
  carbon1_p,
  path = here::here(here::here("figs", "figs_efficacy")),
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
  path = here::here(here::here("figs", "figs_efficacy")),
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
  path = here::here(here::here("figs", "figs_efficacy")),
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
  path = here::here(here::here("figs", "figs_efficacy")),
  width = 12.8,
  height = 7.2,
  units = "in",
  filename = "11_publictransport5_p.jpg",
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

# tab all ---------------------------------------------------------------
main = "Environmental Efficacy estimands / Evalues"
efficacy <- rbind(
  real2_c,
  real3_c,
  real4_c,
  humancaused2_c,
  humancaused3_c,
  humancaused4_c,
  climateconcern2_c,
  climateconcern3_c,
  climateconcern4_c,
  satnzenv2_c,
  satnzenv3_c,
  satnzenv4_c,
  satnzenv5_c,
  natspecies3_c,
  natspecies4_c,
  possum3_c,
  possum4_c,
  sacrificewilling4_c,
  sacrificemade4_c,
  carbon1_c,
  possum4_c,
  motorway5_c,
  publictransport5_c
)


efficacy_tab <- efficacy |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  # kable_styling() %>%
  row_spec(c(1:5,7:9,14,15,18:20,23),  # Bold out the lines where EVALUES do not cross zero or for ratios, 1
           bold = T,
           # color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)

efficacy_tab

#save
saveRDS(efficacy_tab, here::here("mods", "efficacy", "efficacy_tab"))

# read
efficacy_tab <- readRDS(here::here("mods", "efficacy", "efficacy_tab"))

efficacy_tab
# forestplots -------------------------------------------------------------


list_outcomes_efficacy <- c(
  list(
    # real2_p,
    # real3_p,
    real4_p,
    # humancaused2_p,
    # humancaused3_p,
    humancaused4_p,
    # climateconcern2_p,
    # climateconcern3_p,
    climateconcern4_p,
    # satnzenv2_p,
    # satnzenv3_p,
    satnzenv4_p,
    # satnzenv5_p,
  #  natspecies3_p,
    natspecies4_p,
  #  possum3_p,
    possum4_p,
    sacrificewilling4_p,
    sacrificemade4_p,
    carbon1_p,
    possum4_p,
    motorway5_p,
    publictransport5_p
  )
)


out_efficacy <- bind_forestplot(list_outcomes_efficacy)
out_efficacy

saveRDS(out_efficacy, here::here("mods", "efficacy", "out_efficacy"))

gcomp_forestplot_efficacy <-
  gcomp_forestplot(out_efficacy,
                   title = "Outcomewide Efficacy + 4/5 years",
                   ylim = c(-.5, .5),
                   xlab = "Incidence Efficacy Attitudes (SD)")

gcomp_forestplot_efficacy


ggsave(
  gcomp_forestplot_efficacy,
  path = here::here(here::here("figs", "figs_efficacy")),
  width = 12,
  height = 8,
  units = "in",
  filename = "gcomp_forestplot_efficacy.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

# 
# ## Risk ratio plot
# out_rr_church <- bind_forestplot(list(smoker_p, volunteers_p))
# 
# # save for future use
# saveh(out_rr_church, "outcomewide-belief-out_rr_church")
# 
# # plot
# gcomp_forestplot_rr_church <-
#   gcomp_forestplot_rr(out_rr_church, title = "Outcomewide Church Attendance RR",
#                       ylim = c(.5, 1.5))
# gcomp_forestplot_rr_church
# 
# ggsave(
#   gcomp_forestplot_rr_church,
#   path = here::here(here::here("figs", "figs_church")),
#   width = 12,
#   height = 8,
#   units = "in",
#   filename = "gcomp_forestplot_rr_church.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 1200
# )
