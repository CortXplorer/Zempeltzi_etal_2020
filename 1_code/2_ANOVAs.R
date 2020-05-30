###############################################################################
##============================= 2_ANOVAs.R ==================================##
###############################################################################

# calculates and plots the reported ANOVAs (Figure 2 and 3).
#
# input: gesamt_data from 0_masterFile.R
# output:
# - figures:   Figure2c_detection.pdf, Figure2c_discrimination.pdf,
#              Figure3a_detection.pdf, Figure3a_discrimination.pdf,
# - csv files: Figure2c,3a_detection.csv, Figure2c,3a_discrimination.csv,
#              Figure3a_posthoc_detection.csv,
#              Figure3a_posthoc_discrimination.csv


##===========================##
##== analysis preparations ==##
##===========================##


# define function to set correct colouring for the plots
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# select variables to be analyzed: avrec_rms.stim.1 - .4
# overall avrecRMS
avrecVars     <- grep("avrec_rms", names(gesamt_data), value = TRUE)
# avrecRMS after each tone
avrecVarsStim <- grep("avrec_rms.stim.", names(gesamt_data), value = TRUE)

# use copy fo gesamt_data (subset_data) for ANOVA analyses 
subset_data <- gesamt_data 


# z-normalise all variables within session and animal
for(current_variable in avrecVars){
  eval(parse(text = paste0("subset_data$", current_variable,
    " <- ave(subset_data$", current_variable,
    ", subset_data$session, subset_data$animal, FUN=scale)")))  
}


##==================================##
##== analyses for detection phase ==##
##==================================##

# select only the trials in detection phase
current_phase <- "detect01"
current_data_set_phase <- subset_data[subset_data$phase == current_phase,]

# specify how many colours are needed for plots
cols <- gg_color_hue(2)              # for 1kHz, 4kHz
cols4 <- gg_color_hue(4)[c(1,3,2,4)] # for hits, miss, fa, cr

# select only relevant columns for this analysis
data_wide <- current_data_set_phase[, c("session", "animal",
  "frequency", "answer", "answerWithFreq", avrecVarsStim)]

# and reshape it into a long format for analysis
data_long <- gather(data_wide, stim, measurement,
  avrecVarsStim, factor_key = TRUE)
data_long$frequency <- factor(data_long$frequency, levels = c(1,4),
  labels = c("1kHz", "4kHz"))

##== Figure 2c: frequency ==##

# set up data.frame to save the ANOVA results
finalDF <- NULL

# aggregate along order.stim (=1kHz vs 4kHz) and animal
data_long_freq <- aggregate(measurement ~ animal + frequency,
														mean, na.rm = TRUE, data = data_long)

# one-way ANOVA for effect of frequency with repeated measures along animals
aov.freq <- aov(measurement ~ frequency + Error(animal/frequency),
  data_long_freq)

# saves the stats for the main effect of frequency 
temp <- summary(aov.freq)[["Error: animal:frequency"]][[1]]
effvec <- c("1wayANOVA:frequency", temp[1,1], temp[2,1],
            unlist(temp[1,4:5]), EtaSq(aov.freq, 1)[,3])
finalDF <- rbind(finalDF, effvec)

       
p.stim.all <- ggboxplot(data = data_long_freq, x = "frequency",
	  y = "measurement", col = "frequency") + ylim(-0.3, 0.4) +
  stat_compare_means(label = "p.signif", method = "t.test",
		comparisons = list(c("1kHz", "4kHz")), tip.length = .00, paired = TRUE,
		symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
											 symbols = c("***", "**", "*", "ns"))) +
	theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab(current_variable)



file_name <-
  paste0(mainDirPlots, "Figure2c_detection.pdf")
pdf(file = file_name, width = 10, height = 8)
print(p.stim.all)
dev.off()


##== Figure 3a: response ==##

# aggregate by animal, stim & answerWithFreq (hit & miss at 1 and 4 kHz)
data_long_avg <- aggregate(measurement ~ animal + answerWithFreq + stim,
  mean, na.rm = TRUE, data =  data_long)

## 2way ANOVA ##
aov.answer.2way <- aov(measurement ~ answerWithFreq*stim +
  Error(animal/answerWithFreq), data_long_avg)

# read out stats for the output file
obj      <- summary(aov.answer.2way)
etasqs   <- EtaSq(aov.answer.2way, 1)

effTab1  <- obj[["Error: animal:answerWithFreq"]][[1]]
mainEff1 <- c("2wayANOVA:answerWithFreq", effTab1[,1], effTab1[1,4], effTab1[1,5], etasqs[1,3])
effTab2  <- obj[["Error: Within"]][[1]]
mainEff2 <- c("2wayANOVA:stimulus", effTab2[c(1,3),1], effTab2[1,4], effTab2[1,5], etasqs[2,3])
mainEff3 <- c("2wayANOVA:interaction", effTab2[c(2,3),1], effTab2[2,4], effTab2[2,5], etasqs[3,3])

finalDF <- rbind(finalDF, mainEff1, mainEff2, mainEff3)
colnames(finalDF) <- c("effect", "df1", "df2", "F", "p", "EtaSq")
write.table(finalDF, file = paste0(mainDirCSV, "Figure2c,3a_detection.csv"),
						sep = ",", dec = ".", row.names = FALSE)

## for the Holm-adjusted posthoc tests:
# separate data into one dataset per stimulus: 
data_long_avg_stim1 <- data_long_avg[
  data_long_avg$stim == grep("1", levels(data_long_avg$stim), value = TRUE),]
data_long_avg_stim2 <- data_long_avg[
  data_long_avg$stim == grep("2", levels(data_long_avg$stim), value = TRUE),]
data_long_avg_stim3 <- data_long_avg[
  data_long_avg$stim == grep("3", levels(data_long_avg$stim), value = TRUE),]
data_long_avg_stim4 <- data_long_avg[
  data_long_avg$stim == grep("4", levels(data_long_avg$stim), value = TRUE),]

# run pairwise t-tests on all without correction:
ptt1 <- pairwise.t.test(data_long_avg_stim1$measurement,
											 	data_long_avg_stim1$answerWithFreq, "none",
												paired = TRUE)$p.value
ptt2 <- pairwise.t.test(data_long_avg_stim2$measurement,
									      data_long_avg_stim2$answerWithFreq, "none",
									      paired = TRUE)$p.value
ptt3 <- pairwise.t.test(data_long_avg_stim3$measurement,
									      data_long_avg_stim3$answerWithFreq, "none",
									      paired = TRUE)$p.value
ptt4 <- pairwise.t.test(data_long_avg_stim4$measurement,
									      data_long_avg_stim4$answerWithFreq, "none",
									      paired= TRUE)$p.value

# adjust resulting pvector for all 4 stimuli according to Holm-Bonferroni
pvec <- na.omit(c(ptt1, ptt2, ptt3, ptt4))
p.adjNEW <- p.adjust(pvec, "holm")


## plotting ##

# 6 posthoc comparisons per ANOVA: corrected within each ANOVA
for(i in 1:4){
	eval(parse(text = paste0("tib", i,  "<- compare_means(measurement ~ answerWithFreq, data_long_avg_stim", i,
	  ", method = \"t.test\", paired = TRUE, p.adj = \"holm\") %>% mutate(y.position = ",
		"c(1.5, 1.75, 2, 2.25, 2.5, 2.75), stim = \"avrec_rms.stim.", i, "\")")))
}

tib.all <- rbind(tib1, tib2, tib3, tib4)

# overwrite p values adjusted within each ANOVA with the ones adjusted
#   across all 24 comparisons, a done above.
tib.all$p.adj <- p.adjNEW

# how to display significance in the plot (ns., *, **, ***)
tib.all$p.signif <- ifelse(tib.all$p.adj > .05, "ns.",
    ifelse(tib.all$p.adj > .01, "*",
      ifelse(tib.all$p.adj > .001, "**", "***")))

# only show significant comparisons
tib.plot <- filter(tib.all, p.signif != "ns.")

p.answFreq <-  ggboxplot(
  data = data_long_avg,
  x = "answerWithFreq",
  y = "measurement", col = "answerWithFreq")+
  scale_colour_manual(values = cols4) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("avrec_rms") + 
  facet_wrap(~stim, ncol = 4) +
  stat_pvalue_manual(tib.plot, label = "p.signif", tip.length = 0)
  

# save plot
file_name <-
  paste0(mainDirPlots, "Figure3a_detection.pdf")
pdf(file = file_name, width = 10, height = 8)    
print(p.answFreq)
dev.off()
    
# write posthoc comparisons in csv file
write.table(tib.all,
  paste0(mainDirCSV, "Figure3a_posthoc_detection.csv"),
	sep = ",", dec = ".", row.names = FALSE)



##==================================##
##== analyses for discrimination phase ==##
##==================================##

# select only the trials in discrimination phase
current_phase <- "discr01"
current_data_set_phase <- subset_data[subset_data$phase == current_phase,]

# specify how many colours are needed for plots
cols <- gg_color_hue(2)              # for 1kHz, 4kHz
cols4 <- gg_color_hue(4)[c(1,3,2,4)] # for hits, miss, fa, cr

# select only relevant columns for this analysis
data_wide <- current_data_set_phase[, c("session", "animal",
  "frequency", "answer", avrecVarsStim)]

# and reshape it into a long format for analysis
data_long <- gather(data_wide, stim, measurement,
  avrecVarsStim, factor_key = TRUE)
data_long$frequency <- factor(data_long$frequency, levels = c(1,4),
  labels = c("1kHz", "4kHz"))

##== Figure 2c: frequency ==##

# set up data.frame to save the ANOVA results
finalDF <- NULL

# aggregate along order.stim (=1kHz vs 4kHz) and animal
data_long_freq <- aggregate(measurement ~ animal + frequency,
														mean, na.rm = TRUE, data = data_long)

# one-way ANOVA for effect of frequency with repeated measures along animals
aov.freq <- aov(measurement ~ frequency + Error(animal/frequency),
  data_long_freq)

# saves the stats for the main effect of frequency 
temp <- summary(aov.freq)[["Error: animal:frequency"]][[1]]
effvec <- c("1wayANOVA:frequency", temp[1,1], temp[2,1],
            unlist(temp[1,4:5]), EtaSq(aov.freq, 1)[,3])
finalDF <- rbind(finalDF, effvec)
       
p.stim.all <- ggboxplot(data = data_long_freq, x = "frequency",
	  y = "measurement", col = "frequency") +
  stat_compare_means(label = "p.signif", method = "t.test",
		comparisons = list(c("1kHz", "4kHz")), tip.length = .00, paired = TRUE,
		symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
											 symbols = c("***", "**", "*", "ns"))) +
	theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab(current_variable)



file_name <-
  paste0(mainDirPlots, "Figure2c_discrimination.pdf")
pdf(file = file_name, width = 10, height = 8)
print(p.stim.all)
dev.off()


##== Figure 3a: response ==##

# aggregate by animal, stim & answer (hit & miss at 1 and 4 kHz)
data_long_avg <- aggregate(measurement ~ animal + answer + stim,
  mean, na.rm = TRUE, data =  data_long)

## 2way ANOVA ##
aov.answer.2way <- aov(measurement ~ answer*stim +
  Error(animal/answer), data_long_avg)

# read out stats for the output file
obj      <- summary(aov.answer.2way)
etasqs   <- EtaSq(aov.answer.2way, 1)

effTab1  <- obj[["Error: animal:answer"]][[1]]
mainEff1 <- c("2wayANOVA:answer", effTab1[,1], effTab1[1,4], effTab1[1,5], etasqs[1,3])
effTab2  <- obj[["Error: Within"]][[1]]
mainEff2 <- c("2wayANOVA:stimulus", effTab2[c(1,3),1], effTab2[1,4], effTab2[1,5], etasqs[2,3])
mainEff3 <- c("2wayANOVA:interaction", effTab2[c(2,3),1], effTab2[2,4], effTab2[2,5], etasqs[3,3])

finalDF <- rbind(finalDF, mainEff1, mainEff2, mainEff3)
colnames(finalDF) <- c("effect", "df1", "df2", "F", "p", "EtaSq")
write.table(finalDF, file = paste0(mainDirCSV, "Figure2c,3a_discrimination.csv"),
						sep = ",", dec = ".", row.names = FALSE)

## for the Holm-adjusted posthoc tests:
# separate data into one dataset per stimulus: 
data_long_avg_stim1 <- data_long_avg[
  data_long_avg$stim == grep("1", levels(data_long_avg$stim), value = TRUE),]
data_long_avg_stim2 <- data_long_avg[
  data_long_avg$stim == grep("2", levels(data_long_avg$stim), value = TRUE),]
data_long_avg_stim3 <- data_long_avg[
  data_long_avg$stim == grep("3", levels(data_long_avg$stim), value = TRUE),]
data_long_avg_stim4 <- data_long_avg[
  data_long_avg$stim == grep("4", levels(data_long_avg$stim), value = TRUE),]

# run pairwise t-tests on all without correction:
ptt1 <- pairwise.t.test(data_long_avg_stim1$measurement,
											 	data_long_avg_stim1$answer, "none",
												paired = TRUE)$p.value
ptt2 <- pairwise.t.test(data_long_avg_stim2$measurement,
									      data_long_avg_stim2$answer, "none",
									      paired = TRUE)$p.value
ptt3 <- pairwise.t.test(data_long_avg_stim3$measurement,
									      data_long_avg_stim3$answer, "none",
									      paired = TRUE)$p.value
ptt4 <- pairwise.t.test(data_long_avg_stim4$measurement,
									      data_long_avg_stim4$answer, "none",
									      paired= TRUE)$p.value

# adjust resulting pvector for all 4 stimuli according to Holm-Bonferroni
pvec <- na.omit(c(ptt1, ptt2, ptt3, ptt4))
p.adjNEW <- p.adjust(pvec, "holm")


## plotting ##

# 6 posthoc comparisons per ANOVA: corrected within each ANOVA
for(i in 1:4){
	eval(parse(text = paste0("tib", i,  "<- compare_means(measurement ~ answer, data_long_avg_stim", i,
	  ", method = \"t.test\", paired = TRUE, p.adj = \"holm\") %>% mutate(y.position = ",
		"c(2, 2.25, 2.5, 2.75, 3, 3.25), stim = \"avrec_rms.stim.", i, "\")")))
}

tib.all <- rbind(tib1, tib2, tib3, tib4)

# overwrite p values adjusted within each ANOVA with the ones adjusted
#   across all 24 comparisons, a done above.
tib.all$p.adj <- p.adjNEW

# how to display significance in the plot (ns., *, **, ***)
tib.all$p.signif <- ifelse(tib.all$p.adj > .05, "ns.",
    ifelse(tib.all$p.adj > .01, "*",
      ifelse(tib.all$p.adj > .001, "**", "***")))

# only show significant comparisons
tib.plot <- filter(tib.all, p.signif != "ns.")

p.answFreq <-  ggboxplot(
  data = data_long_avg,
  x = "answer",
  y = "measurement", col = "answer")+
  scale_colour_manual(values = cols4) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("avrec_rms") + 
  facet_wrap(~stim, ncol = 4) +
  stat_pvalue_manual(tib.plot, label = "p.signif", tip.length = 0)
  

# save plot
file_name <-
  paste0(mainDirPlots, "Figure3a_discrimination.pdf")
pdf(file = file_name, width = 10, height = 8)    
print(p.answFreq)
dev.off()
    
# write posthoc comparisons in csv file
write.table(tib.all,
  paste0(mainDirCSV, "Figure3a_posthoc_discrimination.csv"),
	sep = ",", dec = ".", row.names = FALSE)
