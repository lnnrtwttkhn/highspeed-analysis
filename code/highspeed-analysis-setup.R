# find the path to the root of this project:
if (!requireNamespace("here")) install.packages("here")
if ( basename(here::here()) == "highspeed" ) {
  path_root = here::here("highspeed-analysis")
} else {
  path_root = here::here()
}
source(file.path(path_root, "code", "highspeed-analysis-source.R"))
# reverting to ggplot2 version 3.2.1 for lemon compatibility
# cf. https://github.com/stefanedwards/lemon/issues/20
#devtools::install_version("ggplot2", version = "3.3.0", repos = "http://cran.us.r-project.org")
# create an array with all required packages:
packages <- c("R.matlab", "ggplot2", "dplyr", "ggpubr", "psyphy", "ggm",
             "corrplot", "reshape2", "afex", "polycor", "tinytex", "extrafont",
             "viridis", "rjson", "jsonlite", "tidyr", "combinat", "rlist",
             "lemon", "doMC", "plyr", "styler", "gridExtra", "grid",
             "data.table", "NameNeedle", "textreuse", "stringdist", "emmeans",
             "RColorBrewer", "tidyverse", "gtools", "cowplot",
             "assertr", "lavaan", "rmarkdown", "readr", "caTools", "bitops",
             "broom", "ggridges", "nloptr", "devtools", "bookdown", "rstatix",
             "lomb")
# load packages using the load_packages function:
load_packages(packages_list = packages)
# specify paths:
source(file.path(path_root, "code", "highspeed-cluster-permutation.R"))
source(file.path(path_root, "code", "raincloud-plots", "tutorial_R", "R_rainclouds.R"))
source(file.path(path_root, "code", "raincloud-plots", "tutorial_R", "summarySE.R"))
# path to figures created by the analysis code:
path_figures <- here::here("figures")
# path to the participants.tsv file (according to BIDS):
# datalad get data/bids/participants.tsv
path_participants <- file.path(path_root, "data", "bids", "participants.tsv")
# path to the events.tsv files (according to BIDS):
path_events <- file.path(path_root, "data", "bids", "*", "*", "func", "*events.tsv")
# path to data from the decoding analysis:
path_pred <- file.path(path_root, "data", "decoding", "*", "data", "*decoding.csv")
# path to the data from the mask thresholding:
path_thresh <- file.path(path_root, "data", "decoding", "*", "data", "*thresholding.csv")
# path to the data of the voxel patterns:
path_patterns <- file.path(path_root, "data", "decoding", "*", "data", "*voxel_patterns_union.csv")
# Load all [BIDS](http://bids.neuroimaging.io/) events files:
# read all events files and concatenate them in a data table:
# datalad get data/bids/sub-*/ses-*/func/*events.tsv
dt_events <- do.call(rbind, lapply(Sys.glob(path_events), fread))
# add the cue of the current trial and the response accuracy on a given trial:
dt_events[, by = .(subject, condition, trial), ":=" (
  trial_cue = stim_label[!is.na(target) & target != 0],
  trial_accuracy = accuracy[!is.na(accuracy)]
)]
# read all decoding data files and concatenate them in a data table:
#dt_pred <- do.call(rbind, lapply(Sys.glob(path_pred), data.table::fread))
# prepare data (add additional variables like a speed trial counter etc.)
#dt_pred <- prepare_data(dt_pred)
# read all data from the mask thresholding:
#dt_thresh = do.call(rbind, lapply(Sys.glob(path_thresh), data.table::fread))
# read all data from the mask thresholding:
#dt_patterns = lapply(Sys.glob(path_patterns), data.table::fread)
# create color list for probabilities of individual sequence events:
color_events <- rev(hcl.colors(5, "Zissou 1"))
# define global lmer settings used in all mixed effects lmer models:
lcctrl <- lmerControl(
  optimizer = c("bobyqa"), optCtrl = list(maxfun = 500000),
  calc.derivs = FALSE)
