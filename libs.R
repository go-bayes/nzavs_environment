#libs
# libraries
library("tidyverse")
library("ggthemes")
library("ggplot2")
library("ggpubr")
library("ggsci")
library("sjstats")
library("sjPlot")
library("patchwork")
library("ggeffects")
library("brms")
library("rstan")
library("bayesplot")
library("parameters") # great tables, plots
library("mathjaxr")
library("prettycode")
library("styler")
library("see")# plots
library("bayestestR") # for latex
library("performance")
library("ggpmisc")
library("dagitty")
library("ggdag")

# settings
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores ())
#theme_set(theme_few())
set_theme(base=theme_sjplot())
