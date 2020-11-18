#libs

# libraries
library(dplyr)
library(tidyr)
library(ggthemes)
library(ggplot2)
library(gghighlight)
library(ggpubr)
library(ggsci)
library(sjstats)
library(sjPlot)
library(patchwork)
library(table1)
library(ggeffects)
library(brms)
library(rstan)
library(bayesplot)
library(lme4)
library(car) # VIF
library(performance)
library(gghighlight)
#library(texreg)
library(parameters) # great tables, plots


# settings
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores ())
#theme_set(theme_few())
set_theme(base=theme_sjplot())
