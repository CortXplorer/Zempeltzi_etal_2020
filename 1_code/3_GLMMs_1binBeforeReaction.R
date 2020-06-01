###############################################################################
##==================== 3_GLMMs_1binBeforeReaction.R =========================##
###############################################################################

# requires the plotting functions defined in 2_pipeline
source("../2_pipeline/3,4_GLMM_plottingFunction.R")

###############################################################################   
     
# split data into detection and discrimination phase
# discrimination phase excludes data from animal7

data_det01   <- gesamt_data[gesamt_data$phase == "detect01",]
data_discr01 <- gesamt_data[gesamt_data$phase == "discr01",]
data_discr01 <- droplevels(data_discr01[data_discr01$animal != 7,])


# read out all independent variables used in the GLMM analyses
variables_binbeforeRT <- 
  c("avrec_rms.bin.before.rt", "GS1.trace.rms.bin.before.rt",
		"VI.trace.rms.bin.before.rt", "Va.trace.rms.bin.before.rt",
	  "Vb.trace.rms.bin.before.rt", "SGS1.trace.rms.bin.before.rt")
variables_binbeforeRT <- grep(
  "*bin.before.rt*", names(gesamt_data), value = TRUE)
variables_binbeforeRT <- grep(
  "relres|GS2", variables_binbeforeRT, value = TRUE, invert = TRUE)

# read out all binary dependent variables used in the GLMM analyses
variable_answer_binary <-
  c("frequency_4khz_0_1khz_1",
    "hit_0_miss_1",
    "false_alarm_below_6_0_cr_1",
    "hit_0_cr_1", 
    "false_alarm_below_6_0_miss_1")


##== 5a) frequency ==##

##==============================##
## analyses for detection phase ##
##==============================##

current_data_set <- data_det01

# first analysis: 1vs4 kHz using entire trace:

# avrec_rms
data_no_na_det01 <- data_det01[!is.na(data_det01$avrec_rms),]
gmodel.avrec <- glmer(frequency_4khz_0_1khz_1 ~ scale(avrec_rms) +
								 (1 + scale(avrec_rms)|animal), data = data_no_na_det01,
				         family = "binomial")
# plot figure 4a (detection):
R2_avrec <- plot_glmm(gmodel.avrec, data_no_na_det01, mainDirPlots, "detect01",
					"avrec_rms", "frequency_4khz_0_1khz_1", "Figure4a_detection")

# GS1.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$GS1.trace.rms),]
gmodel.GS1 <- glmer(frequency_4khz_0_1khz_1 ~ scale(GS1.trace.rms) +
								 (1 + scale(GS1.trace.rms)|animal), data = data_no_na_det01,
				         family = "binomial")

R2_GS1 <- R2out(gmodel.GS1, data_no_na_det01, "detect01",
                "GS1.trace.rms", "frequency_4khz_0_1khz_1")

# VI.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$VI.trace.rms),]
gmodel.VI <- glmer(frequency_4khz_0_1khz_1 ~ scale(VI.trace.rms) +
								 (1 + scale(VI.trace.rms)|animal), data = data_no_na_det01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_det01, "detect01",
                "VI.trace.rms", "frequency_4khz_0_1khz_1")

# Va.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$Va.trace.rms),]
gmodel.Va <- glmer(frequency_4khz_0_1khz_1 ~ scale(Va.trace.rms) +
								 (1 + scale(Va.trace.rms)|animal), data = data_no_na_det01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_det01, "detect01",
                "Va.trace.rms", "frequency_4khz_0_1khz_1")

# Vb.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$Vb.trace.rms),]
gmodel.Vb <- glmer(frequency_4khz_0_1khz_1 ~ scale(Vb.trace.rms) +
								 (1 + scale(Vb.trace.rms)|animal), data = data_no_na_det01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_det01, "detect01",
                "Vb.trace.rms", "frequency_4khz_0_1khz_1")

# SGS1.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$SGS1.trace.rms),]
gmodel.SGS1 <- glmer(frequency_4khz_0_1khz_1 ~ scale(SGS1.trace.rms) +
								     (1 + scale(SGS1.trace.rms)|animal), data = data_no_na_det01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.VI, data_no_na_det01, "detect01",
                 "SGS1.trace.rms", "frequency_4khz_0_1khz_1")


out_det01_freq <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
	

## same in discr01 ##

# first the frequency with overall trace

# avrec_rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$avrec_rms),]
gmodel.avrec <- glmer(frequency_4khz_0_1khz_1 ~ scale(avrec_rms) +
								 (1 + scale(avrec_rms)|animal), data = data_no_na_discr01,
				         family = "binomial")

R2_avrec <- plot_glmm(gmodel.avrec, data_no_na_discr01, mainDirPlots, "discr01",
					"avrec_rms", "frequency_4khz_0_1khz_1", "Figure4a_discrimination")

# GS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$GS1.trace.rms),]
gmodel.GS1 <- glmer(frequency_4khz_0_1khz_1 ~ scale(GS1.trace.rms) +
								 (1 + scale(GS1.trace.rms)|animal), data = data_no_na_discr01,
				         family = "binomial")

R2_GS1 <- R2out(gmodel.GS1, data_no_na_det01, "discr01",
                "GS1.trace.rms", "frequency_4khz_0_1khz_1")


# VI.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$VI.trace.rms),]
gmodel.VI <- glmer(frequency_4khz_0_1khz_1 ~ scale(VI.trace.rms) +
								 (1 + scale(VI.trace.rms)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_det01, "discr01",
                "VI.trace.rms", "frequency_4khz_0_1khz_1")

# Va.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Va.trace.rms),]
gmodel.Va <- glmer(frequency_4khz_0_1khz_1 ~ scale(Va.trace.rms) +
								 (1 + scale(Va.trace.rms)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_det01, "discr01",
                "Va.trace.rms", "frequency_4khz_0_1khz_1")

# Vb.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Vb.trace.rms),]
gmodel.Vb <- glmer(frequency_4khz_0_1khz_1 ~ scale(Vb.trace.rms) +
								 (1 + scale(Vb.trace.rms)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_det01, "discr01",
                "Vb.trace.rms", "frequency_4khz_0_1khz_1")

# SGS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$SGS1.trace.rms),]
gmodel.SGS1 <- glmer(frequency_4khz_0_1khz_1 ~ scale(SGS1.trace.rms) +
								     (1 + scale(SGS1.trace.rms)|animal), data = data_no_na_discr01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_det01, "discr01",
                 "SGS1.trace.rms", "frequency_4khz_0_1khz_1")
		
out_discr01_freq <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)

out_freq <- rbind(out_det01_freq, out_discr01_freq)

file_name <- paste0(mainDirCSV, "/Figure4b_GLMMs-freqs.csv")
write.table(x = out_freq, file_name,
  sep = ",", dec = ".", row.names = FALSE) 



##== 5b) binary answer categories ==##

## detect01: hit vs miss with bin.before.rt ##

## hit vs miss

# # avrec_rms.bin.before.rt
data_no_na_det01 <- data_det01[!is.na(data_det01$avrec_rms.bin.before.rt),]

gmodel.avrec <- glmer(hit_0_miss_1 ~ scale(avrec_rms.bin.before.rt) +
								 (1 + scale(avrec_rms.bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_avrec <- plot_glmm(gmodel.avrec, data_no_na_det01, mainDirPlots, "detect01",
 					    "avrec_rms.bin.before.rt", "hit_0_miss_1", "Figure5a-left_detection_hit-miss")
 
# GS1.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$GS1.trace.rms.bin.before.rt),]
gmodel.GS1 <- glmer(hit_0_miss_1 ~ scale(GS1.trace.rms.bin.before.rt) +
 								 (1 + scale(GS1.trace.rms.bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_det01, "detect01",
               "GS1.trace.rms.bin.before.rt", "hit_0_miss_1")

# VI.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$VI.trace.rms.bin.before.rt),]
gmodel.VI <- glmer(hit_0_miss_1 ~ scale(VI.trace.rms.bin.before.rt) +
								 (1 + scale(VI.trace.rms.bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_det01, "detect01",
               "VI.trace.rms.bin.before.rt", "hit_0_miss_1")


# Va.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$Va.trace.rms.bin.before.rt),]
gmodel.Va <- glmer(hit_0_miss_1 ~ scale(Va.trace.rms.bin.before.rt) +
 								 (1 + scale(Va.trace.rms.bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_det01, "detect01",
               "Va.trace.rms.bin.before.rt", "hit_0_miss_1")
 
# Vb.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$Vb.trace.rms.bin.before.rt),]
gmodel.Vb <- glmer(hit_0_miss_1 ~ scale(Vb.trace.rms.bin.before.rt) +
 								 (1 + scale(Vb.trace.rms.bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_det01, "detect01",
               "Vb.trace.rms.bin.before.rt", "hit_0_miss_1")
 
# SGS1.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$SGS1.trace.rms.bin.before.rt),]
gmodel.SGS1 <- glmer(hit_0_miss_1 ~ scale(SGS1.trace.rms.bin.before.rt) +
 								     (1 + scale(SGS1.trace.rms.bin.before.rt)|animal), data = data_no_na_det01,
 				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_det01, "detect01",
                "SGS1.trace.rms.bin.before.rt", "hit_0_miss_1")

 
out_det01_hitmiss <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
 	
 
 
## discr01: hit vs miss with bin.before.rt ##

## hit vs miss

# avrec_rms.bin.before.rt
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$avrec_rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.avrec <- glmer(hit_0_miss_1 ~ scale(avrec_rms.bin.before.rt) +
								 (1 + scale(avrec_rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_avrec <- plot_glmm(gmodel.avrec, data_no_na_discr01, mainDirPlots, "discr01",
					   "avrec_rms.bin.before.rt", "hit_0_miss_1", "Figure5a-middle_discrimination_hit-miss")

# GS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$GS1.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.GS1 <- glmer(hit_0_miss_1 ~ scale(GS1.trace.rms.bin.before.rt) +
								 (1 + scale(GS1.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_discr01, "discr01",
                "GS1.trace.rms.bin.before.rt", "hit_0_miss_1")

# VI.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$VI.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.VI <- glmer(hit_0_miss_1 ~ scale(VI.trace.rms.bin.before.rt) +
								 (1 + scale(VI.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_discr01, "discr01",
                "VI.trace.rms.bin.before.rt", "hit_0_miss_1")

# Va.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Va.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.Va <- glmer(hit_0_miss_1 ~ scale(Va.trace.rms.bin.before.rt) +
								 (1 + scale(Va.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_discr01, "discr01",
                "Va.trace.rms.bin.before.rt", "hit_0_miss_1")

# Vb.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Vb.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.Vb <- glmer(hit_0_miss_1 ~ scale(Vb.trace.rms.bin.before.rt) +
								 (1 + scale(Vb.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_discr01, "discr01",
                "Vb.trace.rms.bin.before.rt", "hit_0_miss_1")

# SGS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$SGS1.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.SGS1 <- glmer(hit_0_miss_1 ~ scale(SGS1.trace.rms.bin.before.rt) +
								     (1 + scale(SGS1.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_discr01, "discr01",
                "SGS1.trace.rms.bin.before.rt", "hit_0_miss_1")
		
out_discr01_hitmiss <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
	



## fa vs cr
 
# avrec_rms.bin.before.rt
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$avrec_rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.avrec <- glmer(false_alarm_below_6_0_cr_1 ~ scale(avrec_rms.bin.before.rt) +
								 (1 + scale(avrec_rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_avrec <- plot_glmm(gmodel.avrec, data_no_na_discr01, mainDirPlots, "discr01",
					   "avrec_rms.bin.before.rt", "false_alarm_below_6_0_cr_1",
						 "Figure5a-right_falseAlarm-correctRejection")

# GS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$GS1.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.GS1 <- glmer(false_alarm_below_6_0_cr_1 ~ scale(GS1.trace.rms.bin.before.rt) +
								 (1 + scale(GS1.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_discr01, "discr01",
                "GS1.trace.rms.bin.before.rt", "false_alarm_below_6_0_cr_1")

# VI.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$VI.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.VI <- glmer(false_alarm_below_6_0_cr_1 ~ scale(VI.trace.rms.bin.before.rt) +
								 (1 + scale(VI.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_discr01, "discr01",
                "VI.trace.rms.bin.before.rt", "false_alarm_below_6_0_cr_1")

# Va.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Va.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.Va <- glmer(false_alarm_below_6_0_cr_1 ~ scale(Va.trace.rms.bin.before.rt) +
								 (1 + scale(Va.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_discr01, "discr01",
                "Va.trace.rms.bin.before.rt", "false_alarm_below_6_0_cr_1")

# Vb.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Vb.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.Vb <- glmer(false_alarm_below_6_0_cr_1 ~ scale(Vb.trace.rms.bin.before.rt) +
								 (1 + scale(Vb.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_discr01, "discr01",
                "Vb.trace.rms.bin.before.rt", "false_alarm_below_6_0_cr_1")

# SGS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$SGS1.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.SGS1 <- glmer(false_alarm_below_6_0_cr_1 ~ scale(SGS1.trace.rms.bin.before.rt) +
								     (1 + scale(SGS1.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_discr01, "discr01",
                "SGS1.trace.rms.bin.before.rt", "false_alarm_below_6_0_cr_1")


out_discr01_facr <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
 
 
 
## hit vs cr
 
# avrec_rms.bin.before.rt
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$avrec_rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.avrec <- glmer(hit_0_cr_1 ~ scale(avrec_rms.bin.before.rt) +
								 (1 + scale(avrec_rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_avrec <- plot_glmm(gmodel.avrec, data_no_na_discr01, mainDirPlots, "discr01",
					   "avrec_rms.bin.before.rt", "hit_0_cr_1", "Figure6a-left_hit-correctRejection")

# GS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$GS1.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.GS1 <- glmer(hit_0_cr_1 ~ scale(GS1.trace.rms.bin.before.rt) +
								 (1 + scale(GS1.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_discr01, "discr01",
                "GS1.trace.rms.bin.before.rt", "hit_0_cr_1")

# VI.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$VI.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.VI <- glmer(hit_0_cr_1 ~ scale(VI.trace.rms.bin.before.rt) +
								 (1 + scale(VI.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_discr01, "discr01",
                "VI.trace.rms.bin.before.rt", "hit_0_cr_1")

# Va.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Va.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.Va <- glmer(hit_0_cr_1 ~ scale(Va.trace.rms.bin.before.rt) +
								 (1 + scale(Va.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_discr01, "discr01",
                "Va.trace.rms.bin.before.rt", "hit_0_cr_1")

# Vb.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Vb.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.Vb <- glmer(hit_0_cr_1 ~ scale(Vb.trace.rms.bin.before.rt) +
								 (1 + scale(Vb.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_discr01, "discr01",
                "Vb.trace.rms.bin.before.rt", "hit_0_cr_1")

# SGS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$SGS1.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.SGS1 <- glmer(hit_0_cr_1 ~ scale(SGS1.trace.rms.bin.before.rt) +
								     (1 + scale(SGS1.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_discr01, "discr01",
                "SGS1.trace.rms.bin.before.rt", "hit_0_cr_1")


out_discr01_hitcr <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
 
 
 
## fa vs miss
 
# avrec_rms.bin.before.rt
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$avrec_rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.avrec <- glmer(false_alarm_below_6_0_miss_1 ~ scale(avrec_rms.bin.before.rt) +
								 (1 + scale(avrec_rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_avrec <- plot_glmm(gmodel.avrec, data_no_na_discr01, mainDirPlots, "discr01",
					   "avrec_rms.bin.before.rt", "false_alarm_below_6_0_miss_1", "Figure6a-right_miss-falseAlarm")

# GS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$GS1.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.GS1 <- glmer(false_alarm_below_6_0_miss_1 ~ scale(GS1.trace.rms.bin.before.rt) +
								 (1 + scale(GS1.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_discr01, "discr01",
                "GS1.trace.rms.bin.before.rt", "false_alarm_below_6_0_miss_1")

# VI.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$VI.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.VI <- glmer(false_alarm_below_6_0_miss_1 ~ scale(VI.trace.rms.bin.before.rt) +
								 (1 + scale(VI.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_discr01, "discr01",
                "VI.trace.rms.bin.before.rt", "false_alarm_below_6_0_miss_1")

# Va.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Va.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.Va <- glmer(false_alarm_below_6_0_miss_1 ~ scale(Va.trace.rms.bin.before.rt) +
								 (1 + scale(Va.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_discr01, "discr01",
                "Va.trace.rms.bin.before.rt", "false_alarm_below_6_0_miss_1")

# Vb.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Vb.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.Vb <- glmer(false_alarm_below_6_0_miss_1 ~ scale(Vb.trace.rms.bin.before.rt) +
								 (1 + scale(Vb.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_discr01, "discr01",
                "Vb.trace.rms.bin.before.rt", "false_alarm_below_6_0_miss_1")

# SGS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$SGS1.trace.rms.bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.SGS1 <- glmer(false_alarm_below_6_0_miss_1 ~ scale(SGS1.trace.rms.bin.before.rt) +
								     (1 + scale(SGS1.trace.rms.bin.before.rt)|animal), data = data_no_na_discr01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_discr01, "discr01",
                "SGS1.trace.rms.bin.before.rt", "false_alarm_below_6_0_miss_1")



out_discr01_famiss <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
 
 

out_answer  <- rbind(out_det01_hitmiss, out_discr01_hitmiss, out_discr01_facr,
 										 out_discr01_hitcr, out_discr01_famiss)
 
 
# save into output to compare it with Figure 5b & 6b:
file_name <- paste0(mainDirCSV, "/Figure5b,6b_GLMMs-answer.csv")
write.table(x = out_answer, file_name,
   sep = ",", dec = ".", row.names = FALSE) 

# also save into pipeline for R2m-Plots:
file_name <- paste0(mainDirPipeline, "GLMM_answers_1bin.csv")
write.table(x = out_answer, file_name,
   sep = ",", dec = ".", row.names = FALSE) 
