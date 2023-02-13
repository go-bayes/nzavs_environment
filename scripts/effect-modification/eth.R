
# set digits
options(scipen = 999)

#libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

# for saving models (bulbulia only - use own paths for simulated data)

# set paths for JB
# ** YOU WILL NEED TO SET PATHS TO YOUR OWN COMPUTER**
# for saving models
push_mods <-
  fs::path_expand("~/The\ Virtues\ Project\ Dropbox/outcomewide/mods")
push_figs <-
  fs::path_expand("~/Users/joseph/The\ Virtues\ Project\ Dropbox/outcomewide/figs")



pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )




# import data -------------------------------------------------------------



# note the synthetic dataset is about 20% the size of the full NZAVS dataset
dat <- arrow::read_parquet(here::here(pull_path))


dt <- dat |> 
  select(Id, Wave, YearMeasured, EthnicCats, Env.ClimateChgConcern, GenCohort, Gender) |> 
  filter(Wave == "2013"  & YearMeasured == 1| Wave == "2021" & YearMeasured == 1) |> 
  group_by(Id) %>%
  dplyr::mutate(org = ifelse(Wave == 2013 &
                                YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold = mean(org, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold > 0) %>% #
  ungroup() %>%
  droplevels() %>%
  arrange(Id, Wave) |> 
  drop_na()

str(dt)

length(unique(dt$Id)) #17470

# dt |>
#   filter(Wave == 2018 & YearMeasured==1) |>
#   select(Env.ClimateChgConcern, Gender, Id, Wave) |>
#   drop_na() |>
#   summarise(count_distinct = n_distinct(Id))

# Change label 
table1::label(dt$Env.ClimateChgConcern) <- "Deeply Concerned About Climate"
table1::label(dt$EthnicCats) <- "Ethnicities" 


table1::table1( ~ Env.ClimateChgConcern |
          Wave * EthnicCats,
        data = dt,
        transpose = F, 
        overall = F) # ALL


length(unique(dt$Id))

dt |>
  ggplot(aes(x=as.factor(EthnicCats), y=Env.ClimateChgConcern, colour = factor(EthnicCats))) +
  facet_wrap( .~ Wave) + 
  geom_boxplot(notch = TRUE) + geom_jitter(shape=16, position=position_jitter(0.2), alpha = .1) + labs(
    title = "Climate Concern by Ethnicity: NZAVS years 2013/14 & 2018/29",
    y = "Climate Concern (1-7)",
    x = "Ethnic Categories") + scale_color_viridis_d(option = "D")

length(unique(dt$Id))


m0 <- lmer( data = dt, 
    Env.ClimateChgConcern ~ EthnicCats + Wave  + (1|Id))



model_parameters(m0)


pstar<- plot ( ggeffects::ggemmeans( m0, terms = c( "Wave",  "EthnicCats") ) ) + 
  labs(
    title = "Climate Concern by Ethnicity\nNew Zealand Attitudes Values Study\nN = 17273, years 2013 & 2022",
   # y = "'I am deeply concerned about climate' (1-7)'",
    y = "Climate Concern (1-7)",
    x = "Years") + scale_color_okabe_ito() + theme_classic() +
  scale_y_continuous(limits= c(4,6))
  
p0

p0 <- pstar + theme(legend.position="bottom") + 
  theme(
    legend.position = "right",
    plot.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 15, face = "bold"),
    legend.title = element_text(color = "Black",face = "bold", size = 15),
    axis.text=element_text(size=15, face = "bold"),
    axis.title=element_text(size=15, face = "bold")
  ) 

p0

# save
ggsave(
  p0,
  path = here::here(here::here("figs", "treatment-confounder-feedback")),
  width = 16,
  height = 9,
  units = "in",
  filename = "eth.jpg",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)


dag1



# import dag
library(magick)
dag1 <-
  image_ggplot(image_read(here::here("figs", "treatment-confounder-feedback", "tfc.tiff")),
               interpolate = T)



# important prev images, not working. 
# science_action <-
#   image_ggplot(image_read(here::here("figs","treatment-confounder-feedback",  "action_science.jpg")),
#                interpolate = T)
# 
# science_action
# 
# norms_action <-
#   image_ggplot(image_read(here::here("figs","treatment-confounder-feedback",  "norms_concern.jpg")), interpolate = T)

# Get graphs by running code in "Scripts" folder for the trust in science and norm-perception

norms_concern
science_concern


library(ggpubr)
fig1 <-
  ggarrange(
    ggarrange( p0,
    dag1,  ncol = 1,    labels = c("A","B") ),
    ggarrange( norms_concern,
               science_concern,
              labels = c( "C", "D"),
              nrow= 1),
  # widths = c(1,2),
   # heights = c(1,1),
   # ncol = 2,
    nrow = 1
  )


  
  
fig1 <-
  ggarrange(
      ggarrange( dag1, 
                 ggarrange ( norms_concern,
               science_concern,
               labels = c( "C", "D"),
               nrow= 1), nrow = 2),
      p0,
    widths = c(2, 1.5),
    labels = c( "A", "B"),
    # heights = c(1,1),
    # ncol = 2,
    nrow = 1
  )

fig1


# 
# 
# fig1 <-
#   ggarrange(
#     ggarrange( p0,
#                dag1,  ncol = 1,    labels = c("A","B") ),
#     ggarrange( norms_concern,
#                science_concern,
#                labels = c( "C", "D"),
#                nrow= 1),
#     # widths = c(1,2),
#     # heights = c(1,1),
#     # ncol = 2,
#     nrow = 1
#   )



ggsave(
  fig1,
  path = here::here(here::here("figs", "treatment-confounder-feedback")),
  width = 28,
  height = 10,
  units = "in",
  filename = "grant_fig.jpg",
  device = 'jpg',
  limitsize = FALSE,
  dpi = 600
)



m1 <- lm( data = dt, 
          Env.ClimateChgConcern ~ GenCohort * Wave )
# for interest
p1 <- plot ( ggeffects::ggemmeans( m1, terms = c( "GenCohort", "Wave") ) )
p1


###############  RENAME YOUR IMPUTED DATASET  'df"

df <- readh(# PLACE DATA HERE)

# mf <- mice::complete(df)
# hist(mf$KESSLER6sum_lead1_z)
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
X = "KESSLER6sum_lead1_z"

# You may set your label for your graphs  HERE WE STICK TO THE EXAMPLE OF WORK
xlab = "Kessler 6 distress (SD)."

#Climate change is caused by humans.

# SET THE RANGE
min = -1
max =  3


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

ylim = c(-.2,.2)  # SET AS YOU LIKE -- here, how much movement across a standard deviation unit of the outcome
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
out_m <- mice_gaussian(df = df, X = X, Y = Y, 
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
  path = here::here(here::here("figs", "figs_distress")),
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


ggsave(
  humancaused2_p,
  path = here::here(here::here("figs", "figs_distress")),
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

climateconcern2_p

ggsave(
  climateconcern2_p,
  path = here::here(here::here("figs", "figs_distress")),
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


ggsave(
  satnzenv2_p,
  path = here::here(here::here("figs", "figs_distress")),
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
  path = here::here(here::here("figs", "figs_distress")),
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
h_tab <- rbind(
  #  humancaused2_c,
  real2_c,
  humancaused2_c,
  climateconcern2_c,
  satnzenv2_c,
  efficacy2_c
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
  path = here::here(here::here("figs", "figs_distress", "revealed_plots")),
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


