#libs

# libraries
library(dplyr)
library(tidyverse)
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
library(ggpubr)
library("mathjaxr")
library(ggsci)
library(sjstats)
library(sjPlot)
library(patchwork)
library(table1)
library("prettycode")
library("styler")
library(ggeffects)
library(brms)
library(rstan)
library(bayesplot)
library(lme4)
library(car) # VIF
library(see)# plots
library(equatiomatic) 
#library(texreg) # for latex
library(parameters) # great tables, plots
library(report)
library(performance)
library(ggpmisc)
library(ggfortify)
#library(magrittr) 
library(lubridate)
library(dagitty)
library(ggdag)

# settings
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores ())
#theme_set(theme_few())
set_theme(base=theme_sjplot())
