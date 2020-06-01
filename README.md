Code and data to reproduce the statistical analyses in Zempeltzi et al. ,2020, in Communications Biology.

***
## Title: Task rule and choice are reflected by layer-specific processing in rodent auditory cortical microcircuits ##
#### Authors: Marina M. Zempeltzi, Martin Kisse, Michael G. K. Brunk, Claudia Glemser, Sümeyra Aksit, Katrina E. Deane, Shivam Maurya, Lina Schneider, Frank W. Ohl, Matthias Deliano & Max F. K. Happel ####

___Please cite us if you use these scripts.___
***

The data needed is found in 0_data:
- Dataset.RData contains the preprocessed electrophysiological data used for statistical analysis
- Raw LFP/CSD data (.mat files) for this project are available upon request.

The analysis scripts will produce the publication’s figures (.pdf) and statistics(.csv), which are placed in the folder 3_output. They are ordered and organized by their purpose in the 1_code folder:
- _0_masterFile.R_: here the data will be loaded and you can choose which analyses you want to run by setting the corresponding Boolean variables
- _1_descriptiveStats.R_: creates the RT histograms 
- _2_ANOVAs.R_: runs all the ANOVAs and saves their corresponding statistics (with posthoc tests) and plots
- _3_GLMMs_1binBeforeReaction.R_: this will run all GLMMs shown in the paper, plotting the ones for the AVREC RMS and giving the additional statistical results for all layers and binary variables
- _4_GLMMs_2to4binBeforeReaction.R_: this will run the time-resolved GLMMs, tracking back 2,3 and 4 bins before the animal's reaction time. These analyses are summarized and displayed in Figure 7, which is created in the next file
- _5_Time-resolvedR2mPlots.R_: this will create the R2m plots shown in the paper, based on the results of file 4.

Since code file 4 takes a longer time to run, it is recommended to set runAllGLMMs to FALSE in the 0_masterFile.R.
2_pipeline stores the results of the time-resolved GLMMs for further analysis in script 5, as well as some original functions written to facilitate the plotting and handling of the GLMMs.

With these guidelines, further, more detailled comments in the code and the modular setup of the analysis, we want to allow the user to rerun and understand our statistical analysis.

Please raise an issue in this repository if something is not running. Thank you!
