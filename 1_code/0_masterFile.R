###############################################################################
##============================= 0_masterFile.R ==============================##
###############################################################################

# once you've cloned the repository:
#  1) set your working directory to the folder "Zempeltzi_etal_2020/1_code"
#  2) choose which analyses you want to run (a few lines down)
#  3) run this file

## load necessary libraries ##
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2)     # needed for plotting in all files
p_load(DescTools)   # needed in 2_ANOVAs: EtaSq()
p_load(dplyr)       # needed for some operations/data tyding/...
p_load(ggpubr)      # needed for stat_compare_means in ggplot
p_load(tidyr)       # needed for gather()
p_load(lme4, MuMIn) # needed for GLMMs: car, sjPlot, r.squaredGLMM
p_load(grid)        # needed for plotting multiple ggplots in one
p_load(viridis)     # needed for ggplot colour scaling

# load data file
load("../0_data/Dataset.RData")

# the paper only looks at the first two phases
gesamt_data <- gesamt_data[gesamt_data$phase %in% c("detect01", "discr01"),]

## TODO:
# include this in ReadingIn-File -> Dataset only includes phase 1 and 2

mainDirPlots <- paste0("../3_output/figures/")
mainDirCSV   <- paste0("../3_output/statistics/")
mainDirPipeline <- paste0("../2_pipeline/")

###=== CHOOSE which analyses you want to run ===###

# corresponds to Figure 1
runDescriptives <- TRUE

# corresponds to Figure 2 and 3
runANOVAs <- TRUE

# corresponds to Figure 4, 5 and 6
runGLMMs <- FALSE

# corresponds to Figure 7
runTemporalGLMMs <- TRUE

runAllGLMMs <- TRUE  # FALSE recommended if you want to save time
# for convenience the output csv files needed to plot the temporal resolution
#   of the GLMMs is already provided in the folder 2_pipeline! 
# you can also choose to rerun the GLMMs for all bins and variables.
#   be however aware that this will take a good amount of time to run.

###=== CALLS all requested analysis files ===###
if(runDescriptives){
  source("1_descriptiveStats.R")
}

if(runANOVAs){
  source("2_ANOVAs.R")
}

if(runGLMMs){
  source("3_GLMMs_1binBeforeReaction.R")
}

if(runTemporalGLMMs){
  if(runAllGLMMs){
    source("4_GLMMs_2to4binBeforeReaction.R")
  }
  source("5_Time-resolvedR2mPlots.R")
}

# The output figures and stats tables will be in 3_output!

