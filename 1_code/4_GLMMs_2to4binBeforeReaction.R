###############################################################################
##=================== 4_GLMMs_2to4binBeforeReaction.R =======================##
###############################################################################

# requires the plotting functions defined in 2_pipeline
source("../2_pipeline/3,4_GLMM_plottingFunction.R")

##== binary answer categories: 2bin.before.rt ==##

## detect01: hit vs miss with 2bin.before.rt ##

## hit vs miss

# # avrec_rms.2bin.before.rt
data_no_na_det01 <- data_det01[!is.na(data_det01$avrec_rms.2bin.before.rt),]

gmodel.avrec <- glmer(hit_0_miss_1 ~ scale(avrec_rms.2bin.before.rt) +
								 (1 + scale(avrec_rms.2bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_avrec <- R2out(gmodel.avrec, data_no_na_det01,"detect01",
 					    "avrec_rms.2bin.before.rt", "hit_0_miss_1")
 
# GS1.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$GS1.trace.rms.2bin.before.rt),]
gmodel.GS1 <- glmer(hit_0_miss_1 ~ scale(GS1.trace.rms.2bin.before.rt) +
 								 (1 + scale(GS1.trace.rms.2bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_det01, "detect01",
               "GS1.trace.rms.2bin.before.rt", "hit_0_miss_1")

# VI.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$VI.trace.rms.2bin.before.rt),]
gmodel.VI <- glmer(hit_0_miss_1 ~ scale(VI.trace.rms.2bin.before.rt) +
								 (1 + scale(VI.trace.rms.2bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_det01, "detect01",
               "VI.trace.rms.2bin.before.rt", "hit_0_miss_1")


# Va.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$Va.trace.rms.2bin.before.rt),]
gmodel.Va <- glmer(hit_0_miss_1 ~ scale(Va.trace.rms.2bin.before.rt) +
 								 (1 + scale(Va.trace.rms.2bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_det01, "detect01",
               "Va.trace.rms.2bin.before.rt", "hit_0_miss_1")
 
# Vb.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$Vb.trace.rms.2bin.before.rt),]
gmodel.Vb <- glmer(hit_0_miss_1 ~ scale(Vb.trace.rms.2bin.before.rt) +
 								 (1 + scale(Vb.trace.rms.2bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_det01, "detect01",
               "Vb.trace.rms.2bin.before.rt", "hit_0_miss_1")
 
# SGS1.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$SGS1.trace.rms.2bin.before.rt),]
gmodel.SGS1 <- glmer(hit_0_miss_1 ~ scale(SGS1.trace.rms.2bin.before.rt) +
 								     (1 + scale(SGS1.trace.rms.2bin.before.rt)|animal), data = data_no_na_det01,
 				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_det01, "detect01",
                "SGS1.trace.rms.2bin.before.rt", "hit_0_miss_1")

 
out_det01_hitmiss <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
 	
 
 
## discr01: hit vs miss with 2bin.before.rt ##

## hit vs miss

# avrec_rms.2bin.before.rt
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$avrec_rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.avrec <- glmer(hit_0_miss_1 ~ scale(avrec_rms.2bin.before.rt) +
								 (1 + scale(avrec_rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_avrec <- R2out(gmodel.avrec, data_no_na_discr01, "discr01",
					   "avrec_rms.2bin.before.rt", "hit_0_miss_1")

# GS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$GS1.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.GS1 <- glmer(hit_0_miss_1 ~ scale(GS1.trace.rms.2bin.before.rt) +
								 (1 + scale(GS1.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_discr01, "discr01",
                "GS1.trace.rms.2bin.before.rt", "hit_0_miss_1")

# VI.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$VI.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.VI <- glmer(hit_0_miss_1 ~ scale(VI.trace.rms.2bin.before.rt) +
								 (1 + scale(VI.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_discr01, "discr01",
                "VI.trace.rms.2bin.before.rt", "hit_0_miss_1")

# Va.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Va.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.Va <- glmer(hit_0_miss_1 ~ scale(Va.trace.rms.2bin.before.rt) +
								 (1 + scale(Va.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_discr01, "discr01",
                "Va.trace.rms.2bin.before.rt", "hit_0_miss_1")

# Vb.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Vb.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.Vb <- glmer(hit_0_miss_1 ~ scale(Vb.trace.rms.2bin.before.rt) +
								 (1 + scale(Vb.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_discr01, "discr01",
                "Vb.trace.rms.2bin.before.rt", "hit_0_miss_1")

# SGS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$SGS1.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.SGS1 <- glmer(hit_0_miss_1 ~ scale(SGS1.trace.rms.2bin.before.rt) +
								     (1 + scale(SGS1.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_discr01, "discr01",
                "SGS1.trace.rms.2bin.before.rt", "hit_0_miss_1")
		
out_discr01_hitmiss <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
	



## fa vs cr
 
# avrec_rms.2bin.before.rt
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$avrec_rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.avrec <- glmer(false_alarm_below_6_0_cr_1 ~ scale(avrec_rms.2bin.before.rt) +
								 (1 + scale(avrec_rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_avrec <- R2out(gmodel.avrec, data_no_na_discr01, "discr01",
					   "avrec_rms.2bin.before.rt", "false_alarm_below_6_0_cr_1")

# GS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$GS1.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.GS1 <- glmer(false_alarm_below_6_0_cr_1 ~ scale(GS1.trace.rms.2bin.before.rt) +
								 (1 + scale(GS1.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_discr01, "discr01",
                "GS1.trace.rms.2bin.before.rt", "false_alarm_below_6_0_cr_1")

# VI.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$VI.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.VI <- glmer(false_alarm_below_6_0_cr_1 ~ scale(VI.trace.rms.2bin.before.rt) +
								 (1 + scale(VI.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_discr01, "discr01",
                "VI.trace.rms.2bin.before.rt", "false_alarm_below_6_0_cr_1")

# Va.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Va.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.Va <- glmer(false_alarm_below_6_0_cr_1 ~ scale(Va.trace.rms.2bin.before.rt) +
								 (1 + scale(Va.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_discr01, "discr01",
                "Va.trace.rms.2bin.before.rt", "false_alarm_below_6_0_cr_1")

# Vb.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Vb.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.Vb <- glmer(false_alarm_below_6_0_cr_1 ~ scale(Vb.trace.rms.2bin.before.rt) +
								 (1 + scale(Vb.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_discr01, "discr01",
                "Vb.trace.rms.2bin.before.rt", "false_alarm_below_6_0_cr_1")

# SGS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$SGS1.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.SGS1 <- glmer(false_alarm_below_6_0_cr_1 ~ scale(SGS1.trace.rms.2bin.before.rt) +
								     (1 + scale(SGS1.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_discr01, "discr01",
                "SGS1.trace.rms.2bin.before.rt", "false_alarm_below_6_0_cr_1")


out_discr01_facr <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
 
 
 
## hit vs cr
 
# avrec_rms.2bin.before.rt
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$avrec_rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.avrec <- glmer(hit_0_cr_1 ~ scale(avrec_rms.2bin.before.rt) +
								 (1 + scale(avrec_rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_avrec <- R2out(gmodel.avrec, data_no_na_discr01, "discr01",
					   "avrec_rms.2bin.before.rt", "hit_0_cr_1")

# GS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$GS1.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.GS1 <- glmer(hit_0_cr_1 ~ scale(GS1.trace.rms.2bin.before.rt) +
								 (1 + scale(GS1.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_discr01, "discr01",
                "GS1.trace.rms.2bin.before.rt", "hit_0_cr_1")

# VI.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$VI.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.VI <- glmer(hit_0_cr_1 ~ scale(VI.trace.rms.2bin.before.rt) +
								 (1 + scale(VI.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_discr01, "discr01",
                "VI.trace.rms.2bin.before.rt", "hit_0_cr_1")

# Va.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Va.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.Va <- glmer(hit_0_cr_1 ~ scale(Va.trace.rms.2bin.before.rt) +
								 (1 + scale(Va.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_discr01, "discr01",
                "Va.trace.rms.2bin.before.rt", "hit_0_cr_1")

# Vb.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Vb.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.Vb <- glmer(hit_0_cr_1 ~ scale(Vb.trace.rms.2bin.before.rt) +
								 (1 + scale(Vb.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_discr01, "discr01",
                "Vb.trace.rms.2bin.before.rt", "hit_0_cr_1")

# SGS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$SGS1.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.SGS1 <- glmer(hit_0_cr_1 ~ scale(SGS1.trace.rms.2bin.before.rt) +
								     (1 + scale(SGS1.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_discr01, "discr01",
                "SGS1.trace.rms.2bin.before.rt", "hit_0_cr_1")


out_discr01_hitcr <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
 
 
 
## fa vs miss
 
# avrec_rms.2bin.before.rt
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$avrec_rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.avrec <- glmer(false_alarm_below_6_0_miss_1 ~ scale(avrec_rms.2bin.before.rt) +
								 (1 + scale(avrec_rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_avrec <- R2out(gmodel.avrec, data_no_na_discr01, "discr01",
					   "avrec_rms.2bin.before.rt", "false_alarm_below_6_0_miss_1")

# GS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$GS1.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.GS1 <- glmer(false_alarm_below_6_0_miss_1 ~ scale(GS1.trace.rms.2bin.before.rt) +
								 (1 + scale(GS1.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_discr01, "discr01",
                "GS1.trace.rms.2bin.before.rt", "false_alarm_below_6_0_miss_1")

# VI.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$VI.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.VI <- glmer(false_alarm_below_6_0_miss_1 ~ scale(VI.trace.rms.2bin.before.rt) +
								 (1 + scale(VI.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_discr01, "discr01",
                "VI.trace.rms.2bin.before.rt", "false_alarm_below_6_0_miss_1")

# Va.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Va.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.Va <- glmer(false_alarm_below_6_0_miss_1 ~ scale(Va.trace.rms.2bin.before.rt) +
								 (1 + scale(Va.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_discr01, "discr01",
                "Va.trace.rms.2bin.before.rt", "false_alarm_below_6_0_miss_1")

# Vb.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Vb.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.Vb <- glmer(false_alarm_below_6_0_miss_1 ~ scale(Vb.trace.rms.2bin.before.rt) +
								 (1 + scale(Vb.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_discr01, "discr01",
                "Vb.trace.rms.2bin.before.rt", "false_alarm_below_6_0_miss_1")

# SGS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$SGS1.trace.rms.2bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.SGS1 <- glmer(false_alarm_below_6_0_miss_1 ~ scale(SGS1.trace.rms.2bin.before.rt) +
								     (1 + scale(SGS1.trace.rms.2bin.before.rt)|animal), data = data_no_na_discr01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_discr01, "discr01",
                "SGS1.trace.rms.2bin.before.rt", "false_alarm_below_6_0_miss_1")



out_discr01_famiss <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
 
 

out_answer  <- rbind(out_det01_hitmiss, out_discr01_hitmiss, out_discr01_facr,
 										 out_discr01_hitcr, out_discr01_famiss)
 
 
# also save into pipeline for R2m-Plots:
file_name <- paste0(mainDirPipeline, "4,5_GLMM_answers_2bin.csv")
write.table(x = out_answer, file_name,
   sep = ",", dec = ".", row.names = FALSE)



##== binary answer categories: 3bin.before.rt ==##

## detect01: hit vs miss with 3bin.before.rt ##

## hit vs miss

# # avrec_rms.3bin.before.rt
data_no_na_det01 <- data_det01[!is.na(data_det01$avrec_rms.3bin.before.rt),]

gmodel.avrec <- glmer(hit_0_miss_1 ~ scale(avrec_rms.3bin.before.rt) +
								 (1 + scale(avrec_rms.3bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_avrec <- R2out(gmodel.avrec, data_no_na_det01, "detect01",
 					    "avrec_rms.3bin.before.rt", "hit_0_miss_1")
 
# GS1.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$GS1.trace.rms.3bin.before.rt),]
gmodel.GS1 <- glmer(hit_0_miss_1 ~ scale(GS1.trace.rms.3bin.before.rt) +
 								 (1 + scale(GS1.trace.rms.3bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_det01, "detect01",
               "GS1.trace.rms.3bin.before.rt", "hit_0_miss_1")

# VI.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$VI.trace.rms.3bin.before.rt),]
gmodel.VI <- glmer(hit_0_miss_1 ~ scale(VI.trace.rms.3bin.before.rt) +
								 (1 + scale(VI.trace.rms.3bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_det01, "detect01",
               "VI.trace.rms.3bin.before.rt", "hit_0_miss_1")


# Va.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$Va.trace.rms.3bin.before.rt),]
gmodel.Va <- glmer(hit_0_miss_1 ~ scale(Va.trace.rms.3bin.before.rt) +
 								 (1 + scale(Va.trace.rms.3bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_det01, "detect01",
               "Va.trace.rms.3bin.before.rt", "hit_0_miss_1")
 
# Vb.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$Vb.trace.rms.3bin.before.rt),]
gmodel.Vb <- glmer(hit_0_miss_1 ~ scale(Vb.trace.rms.3bin.before.rt) +
 								 (1 + scale(Vb.trace.rms.3bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_det01, "detect01",
               "Vb.trace.rms.3bin.before.rt", "hit_0_miss_1")
 
# SGS1.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$SGS1.trace.rms.3bin.before.rt),]
gmodel.SGS1 <- glmer(hit_0_miss_1 ~ scale(SGS1.trace.rms.3bin.before.rt) +
 								     (1 + scale(SGS1.trace.rms.3bin.before.rt)|animal), data = data_no_na_det01,
 				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_det01, "detect01",
                "SGS1.trace.rms.3bin.before.rt", "hit_0_miss_1")

 
out_det01_hitmiss <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
 	
 
 
## discr01: hit vs miss with 3bin.before.rt ##

## hit vs miss

# avrec_rms.3bin.before.rt
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$avrec_rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.avrec <- glmer(hit_0_miss_1 ~ scale(avrec_rms.3bin.before.rt) +
								 (1 + scale(avrec_rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_avrec <- R2out(gmodel.avrec, data_no_na_discr01, "discr01",
					   "avrec_rms.3bin.before.rt", "hit_0_miss_1")

# GS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$GS1.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.GS1 <- glmer(hit_0_miss_1 ~ scale(GS1.trace.rms.3bin.before.rt) +
								 (1 + scale(GS1.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_discr01, "discr01",
                "GS1.trace.rms.3bin.before.rt", "hit_0_miss_1")

# VI.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$VI.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.VI <- glmer(hit_0_miss_1 ~ scale(VI.trace.rms.3bin.before.rt) +
								 (1 + scale(VI.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_discr01, "discr01",
                "VI.trace.rms.3bin.before.rt", "hit_0_miss_1")

# Va.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Va.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.Va <- glmer(hit_0_miss_1 ~ scale(Va.trace.rms.3bin.before.rt) +
								 (1 + scale(Va.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_discr01, "discr01",
                "Va.trace.rms.3bin.before.rt", "hit_0_miss_1")

# Vb.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Vb.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.Vb <- glmer(hit_0_miss_1 ~ scale(Vb.trace.rms.3bin.before.rt) +
								 (1 + scale(Vb.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_discr01, "discr01",
                "Vb.trace.rms.3bin.before.rt", "hit_0_miss_1")

# SGS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$SGS1.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.SGS1 <- glmer(hit_0_miss_1 ~ scale(SGS1.trace.rms.3bin.before.rt) +
								     (1 + scale(SGS1.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_discr01, "discr01",
                "SGS1.trace.rms.3bin.before.rt", "hit_0_miss_1")
		
out_discr01_hitmiss <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
	



## fa vs cr
 
# avrec_rms.3bin.before.rt
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$avrec_rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.avrec <- glmer(false_alarm_below_6_0_cr_1 ~ scale(avrec_rms.3bin.before.rt) +
								 (1 + scale(avrec_rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_avrec <- R2out(gmodel.avrec, data_no_na_discr01, "discr01",
					   "avrec_rms.3bin.before.rt", "false_alarm_below_6_0_cr_1")

# GS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$GS1.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.GS1 <- glmer(false_alarm_below_6_0_cr_1 ~ scale(GS1.trace.rms.3bin.before.rt) +
								 (1 + scale(GS1.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_discr01, "discr01",
                "GS1.trace.rms.3bin.before.rt", "false_alarm_below_6_0_cr_1")

# VI.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$VI.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.VI <- glmer(false_alarm_below_6_0_cr_1 ~ scale(VI.trace.rms.3bin.before.rt) +
								 (1 + scale(VI.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_discr01, "discr01",
                "VI.trace.rms.3bin.before.rt", "false_alarm_below_6_0_cr_1")

# Va.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Va.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.Va <- glmer(false_alarm_below_6_0_cr_1 ~ scale(Va.trace.rms.3bin.before.rt) +
								 (1 + scale(Va.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_discr01, "discr01",
                "Va.trace.rms.3bin.before.rt", "false_alarm_below_6_0_cr_1")

# Vb.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Vb.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.Vb <- glmer(false_alarm_below_6_0_cr_1 ~ scale(Vb.trace.rms.3bin.before.rt) +
								 (1 + scale(Vb.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_discr01, "discr01",
                "Vb.trace.rms.3bin.before.rt", "false_alarm_below_6_0_cr_1")

# SGS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$SGS1.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.SGS1 <- glmer(false_alarm_below_6_0_cr_1 ~ scale(SGS1.trace.rms.3bin.before.rt) +
								     (1 + scale(SGS1.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_discr01, "discr01",
                "SGS1.trace.rms.3bin.before.rt", "false_alarm_below_6_0_cr_1")


out_discr01_facr <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
 
 
 
## hit vs cr
 
# avrec_rms.3bin.before.rt
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$avrec_rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.avrec <- glmer(hit_0_cr_1 ~ scale(avrec_rms.3bin.before.rt) +
								 (1 + scale(avrec_rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_avrec <- R2out(gmodel.avrec, data_no_na_discr01, "discr01",
					   "avrec_rms.3bin.before.rt", "hit_0_cr_1")

# GS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$GS1.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.GS1 <- glmer(hit_0_cr_1 ~ scale(GS1.trace.rms.3bin.before.rt) +
								 (1 + scale(GS1.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_discr01, "discr01",
                "GS1.trace.rms.3bin.before.rt", "hit_0_cr_1")

# VI.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$VI.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.VI <- glmer(hit_0_cr_1 ~ scale(VI.trace.rms.3bin.before.rt) +
								 (1 + scale(VI.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_discr01, "discr01",
                "VI.trace.rms.3bin.before.rt", "hit_0_cr_1")

# Va.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Va.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.Va <- glmer(hit_0_cr_1 ~ scale(Va.trace.rms.3bin.before.rt) +
								 (1 + scale(Va.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_discr01, "discr01",
                "Va.trace.rms.3bin.before.rt", "hit_0_cr_1")

# Vb.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Vb.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.Vb <- glmer(hit_0_cr_1 ~ scale(Vb.trace.rms.3bin.before.rt) +
								 (1 + scale(Vb.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_discr01, "discr01",
                "Vb.trace.rms.3bin.before.rt", "hit_0_cr_1")

# SGS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$SGS1.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.SGS1 <- glmer(hit_0_cr_1 ~ scale(SGS1.trace.rms.3bin.before.rt) +
								     (1 + scale(SGS1.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_discr01, "discr01",
                "SGS1.trace.rms.3bin.before.rt", "hit_0_cr_1")


out_discr01_hitcr <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
 
 
 
## fa vs miss
 
# avrec_rms.3bin.before.rt
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$avrec_rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.avrec <- glmer(false_alarm_below_6_0_miss_1 ~ scale(avrec_rms.3bin.before.rt) +
								 (1 + scale(avrec_rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_avrec <- R2out(gmodel.avrec, data_no_na_discr01, "discr01",
					   "avrec_rms.3bin.before.rt", "false_alarm_below_6_0_miss_1")

# GS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$GS1.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.GS1 <- glmer(false_alarm_below_6_0_miss_1 ~ scale(GS1.trace.rms.3bin.before.rt) +
								 (1 + scale(GS1.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_discr01, "discr01",
                "GS1.trace.rms.3bin.before.rt", "false_alarm_below_6_0_miss_1")

# VI.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$VI.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.VI <- glmer(false_alarm_below_6_0_miss_1 ~ scale(VI.trace.rms.3bin.before.rt) +
								 (1 + scale(VI.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_discr01, "discr01",
                "VI.trace.rms.3bin.before.rt", "false_alarm_below_6_0_miss_1")

# Va.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Va.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.Va <- glmer(false_alarm_below_6_0_miss_1 ~ scale(Va.trace.rms.3bin.before.rt) +
								 (1 + scale(Va.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_discr01, "discr01",
                "Va.trace.rms.3bin.before.rt", "false_alarm_below_6_0_miss_1")

# Vb.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Vb.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.Vb <- glmer(false_alarm_below_6_0_miss_1 ~ scale(Vb.trace.rms.3bin.before.rt) +
								 (1 + scale(Vb.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_discr01, "discr01",
                "Vb.trace.rms.3bin.before.rt", "false_alarm_below_6_0_miss_1")

# SGS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$SGS1.trace.rms.3bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.SGS1 <- glmer(false_alarm_below_6_0_miss_1 ~ scale(SGS1.trace.rms.3bin.before.rt) +
								     (1 + scale(SGS1.trace.rms.3bin.before.rt)|animal), data = data_no_na_discr01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_discr01, "discr01",
                "SGS1.trace.rms.3bin.before.rt", "false_alarm_below_6_0_miss_1")



out_discr01_famiss <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
 
 

out_answer  <- rbind(out_det01_hitmiss, out_discr01_hitmiss, out_discr01_facr,
 										 out_discr01_hitcr, out_discr01_famiss)
 
 

# save into pipeline for R2m-Plots:
file_name <- paste0(mainDirPipeline, "4,5_GLMM_answers_3bin.csv")
write.table(x = out_answer, file_name,
   sep = ",", dec = ".", row.names = FALSE)





###############################################################################
##=================== 4_GLMMs_2to4binBeforeReaction.R =======================##
###############################################################################

# requires the plotting functions defined in 2_pipeline
source("../2_pipeline/3,4_GLMM_plottingFunction.R")

##== 5e) binary answer categories: 4bin.before.rt ==##

## detect01: hit vs miss with 4bin.before.rt ##

## hit vs miss

# # avrec_rms.4bin.before.rt
data_no_na_det01 <- data_det01[!is.na(data_det01$avrec_rms.4bin.before.rt),]

gmodel.avrec <- glmer(hit_0_miss_1 ~ scale(avrec_rms.4bin.before.rt) +
								 (1 + scale(avrec_rms.4bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_avrec <- R2out(gmodel.avrec, data_no_na_det01, "detect01",
 					    "avrec_rms.4bin.before.rt", "hit_0_miss_1")
 
# GS1.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$GS1.trace.rms.4bin.before.rt),]
gmodel.GS1 <- glmer(hit_0_miss_1 ~ scale(GS1.trace.rms.4bin.before.rt) +
 								 (1 + scale(GS1.trace.rms.4bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_det01, "detect01",
               "GS1.trace.rms.4bin.before.rt", "hit_0_miss_1")

# VI.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$VI.trace.rms.4bin.before.rt),]
gmodel.VI <- glmer(hit_0_miss_1 ~ scale(VI.trace.rms.4bin.before.rt) +
								 (1 + scale(VI.trace.rms.4bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_det01, "detect01",
               "VI.trace.rms.4bin.before.rt", "hit_0_miss_1")


# Va.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$Va.trace.rms.4bin.before.rt),]
gmodel.Va <- glmer(hit_0_miss_1 ~ scale(Va.trace.rms.4bin.before.rt) +
 								 (1 + scale(Va.trace.rms.4bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_det01, "detect01",
               "Va.trace.rms.4bin.before.rt", "hit_0_miss_1")
 
# Vb.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$Vb.trace.rms.4bin.before.rt),]
gmodel.Vb <- glmer(hit_0_miss_1 ~ scale(Vb.trace.rms.4bin.before.rt) +
 								 (1 + scale(Vb.trace.rms.4bin.before.rt)|animal), data = data_no_na_det01,
 				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_det01, "detect01",
               "Vb.trace.rms.4bin.before.rt", "hit_0_miss_1")
 
# SGS1.trace.rms
data_no_na_det01 <- data_det01[!is.na(data_det01$SGS1.trace.rms.4bin.before.rt),]
gmodel.SGS1 <- glmer(hit_0_miss_1 ~ scale(SGS1.trace.rms.4bin.before.rt) +
 								     (1 + scale(SGS1.trace.rms.4bin.before.rt)|animal), data = data_no_na_det01,
 				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_det01, "detect01",
                "SGS1.trace.rms.4bin.before.rt", "hit_0_miss_1")

 
out_det01_hitmiss <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
 	
 
 
## discr01: hit vs miss with 4bin.before.rt ##

## hit vs miss

# avrec_rms.4bin.before.rt
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$avrec_rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.avrec <- glmer(hit_0_miss_1 ~ scale(avrec_rms.4bin.before.rt) +
								 (1 + scale(avrec_rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_avrec <- R2out(gmodel.avrec, data_no_na_discr01, "discr01",
					   "avrec_rms.4bin.before.rt", "hit_0_miss_1")

# GS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$GS1.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.GS1 <- glmer(hit_0_miss_1 ~ scale(GS1.trace.rms.4bin.before.rt) +
								 (1 + scale(GS1.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_discr01, "discr01",
                "GS1.trace.rms.4bin.before.rt", "hit_0_miss_1")

# VI.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$VI.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.VI <- glmer(hit_0_miss_1 ~ scale(VI.trace.rms.4bin.before.rt) +
								 (1 + scale(VI.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_discr01, "discr01",
                "VI.trace.rms.4bin.before.rt", "hit_0_miss_1")

# Va.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Va.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.Va <- glmer(hit_0_miss_1 ~ scale(Va.trace.rms.4bin.before.rt) +
								 (1 + scale(Va.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_discr01, "discr01",
                "Va.trace.rms.4bin.before.rt", "hit_0_miss_1")

# Vb.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Vb.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.Vb <- glmer(hit_0_miss_1 ~ scale(Vb.trace.rms.4bin.before.rt) +
								 (1 + scale(Vb.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_discr01, "discr01",
                "Vb.trace.rms.4bin.before.rt", "hit_0_miss_1")

# SGS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$SGS1.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_miss_1),]

gmodel.SGS1 <- glmer(hit_0_miss_1 ~ scale(SGS1.trace.rms.4bin.before.rt) +
								     (1 + scale(SGS1.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_discr01, "discr01",
                "SGS1.trace.rms.4bin.before.rt", "hit_0_miss_1")
		
out_discr01_hitmiss <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
	



## fa vs cr
 
# avrec_rms.4bin.before.rt
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$avrec_rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.avrec <- glmer(false_alarm_below_6_0_cr_1 ~ scale(avrec_rms.4bin.before.rt) +
								 (1 + scale(avrec_rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_avrec <- R2out(gmodel.avrec, data_no_na_discr01, "discr01",
					   "avrec_rms.4bin.before.rt", "false_alarm_below_6_0_cr_1")

# GS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$GS1.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.GS1 <- glmer(false_alarm_below_6_0_cr_1 ~ scale(GS1.trace.rms.4bin.before.rt) +
								 (1 + scale(GS1.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_discr01, "discr01",
                "GS1.trace.rms.4bin.before.rt", "false_alarm_below_6_0_cr_1")

# VI.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$VI.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.VI <- glmer(false_alarm_below_6_0_cr_1 ~ scale(VI.trace.rms.4bin.before.rt) +
								 (1 + scale(VI.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_discr01, "discr01",
                "VI.trace.rms.4bin.before.rt", "false_alarm_below_6_0_cr_1")

# Va.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Va.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.Va <- glmer(false_alarm_below_6_0_cr_1 ~ scale(Va.trace.rms.4bin.before.rt) +
								 (1 + scale(Va.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_discr01, "discr01",
                "Va.trace.rms.4bin.before.rt", "false_alarm_below_6_0_cr_1")

# Vb.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Vb.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.Vb <- glmer(false_alarm_below_6_0_cr_1 ~ scale(Vb.trace.rms.4bin.before.rt) +
								 (1 + scale(Vb.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_discr01, "discr01",
                "Vb.trace.rms.4bin.before.rt", "false_alarm_below_6_0_cr_1")

# SGS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$SGS1.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_cr_1),]

gmodel.SGS1 <- glmer(false_alarm_below_6_0_cr_1 ~ scale(SGS1.trace.rms.4bin.before.rt) +
								     (1 + scale(SGS1.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_discr01, "discr01",
                "SGS1.trace.rms.4bin.before.rt", "false_alarm_below_6_0_cr_1")


out_discr01_facr <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
 
 
 
## hit vs cr
 
# avrec_rms.4bin.before.rt
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$avrec_rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.avrec <- glmer(hit_0_cr_1 ~ scale(avrec_rms.4bin.before.rt) +
								 (1 + scale(avrec_rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_avrec <- R2out(gmodel.avrec, data_no_na_discr01, "discr01",
					   "avrec_rms.4bin.before.rt", "hit_0_cr_1")

# GS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$GS1.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.GS1 <- glmer(hit_0_cr_1 ~ scale(GS1.trace.rms.4bin.before.rt) +
								 (1 + scale(GS1.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_discr01, "discr01",
                "GS1.trace.rms.4bin.before.rt", "hit_0_cr_1")

# VI.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$VI.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.VI <- glmer(hit_0_cr_1 ~ scale(VI.trace.rms.4bin.before.rt) +
								 (1 + scale(VI.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_discr01, "discr01",
                "VI.trace.rms.4bin.before.rt", "hit_0_cr_1")

# Va.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Va.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.Va <- glmer(hit_0_cr_1 ~ scale(Va.trace.rms.4bin.before.rt) +
								 (1 + scale(Va.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_discr01, "discr01",
                "Va.trace.rms.4bin.before.rt", "hit_0_cr_1")

# Vb.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Vb.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.Vb <- glmer(hit_0_cr_1 ~ scale(Vb.trace.rms.4bin.before.rt) +
								 (1 + scale(Vb.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_discr01, "discr01",
                "Vb.trace.rms.4bin.before.rt", "hit_0_cr_1")

# SGS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$SGS1.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$hit_0_cr_1),]

gmodel.SGS1 <- glmer(hit_0_cr_1 ~ scale(SGS1.trace.rms.4bin.before.rt) +
								     (1 + scale(SGS1.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_discr01, "discr01",
                "SGS1.trace.rms.4bin.before.rt", "hit_0_cr_1")


out_discr01_hitcr <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
 
 
 
## fa vs miss
 
# avrec_rms.4bin.before.rt
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$avrec_rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.avrec <- glmer(false_alarm_below_6_0_miss_1 ~ scale(avrec_rms.4bin.before.rt) +
								 (1 + scale(avrec_rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_avrec <- R2out(gmodel.avrec, data_no_na_discr01, "discr01",
					   "avrec_rms.4bin.before.rt", "false_alarm_below_6_0_miss_1")

# GS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$GS1.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.GS1 <- glmer(false_alarm_below_6_0_miss_1 ~ scale(GS1.trace.rms.4bin.before.rt) +
								 (1 + scale(GS1.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_GS1 <- R2out(gmodel.GS1, data_no_na_discr01, "discr01",
                "GS1.trace.rms.4bin.before.rt", "false_alarm_below_6_0_miss_1")

# VI.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$VI.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.VI <- glmer(false_alarm_below_6_0_miss_1 ~ scale(VI.trace.rms.4bin.before.rt) +
								 (1 + scale(VI.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_VI <- R2out(gmodel.VI, data_no_na_discr01, "discr01",
                "VI.trace.rms.4bin.before.rt", "false_alarm_below_6_0_miss_1")

# Va.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Va.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.Va <- glmer(false_alarm_below_6_0_miss_1 ~ scale(Va.trace.rms.4bin.before.rt) +
								 (1 + scale(Va.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Va <- R2out(gmodel.Va, data_no_na_discr01, "discr01",
                "Va.trace.rms.4bin.before.rt", "false_alarm_below_6_0_miss_1")

# Vb.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$Vb.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.Vb <- glmer(false_alarm_below_6_0_miss_1 ~ scale(Vb.trace.rms.4bin.before.rt) +
								 (1 + scale(Vb.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				         family = "binomial")
R2_Vb <- R2out(gmodel.Vb, data_no_na_discr01, "discr01",
                "Vb.trace.rms.4bin.before.rt", "false_alarm_below_6_0_miss_1")

# SGS1.trace.rms
data_no_na_discr01 <- data_discr01[!is.na(data_discr01$SGS1.trace.rms.4bin.before.rt),]
data_no_na_discr01 <- data_no_na_discr01[!is.na(data_no_na_discr01$false_alarm_below_6_0_miss_1),]

gmodel.SGS1 <- glmer(false_alarm_below_6_0_miss_1 ~ scale(SGS1.trace.rms.4bin.before.rt) +
								     (1 + scale(SGS1.trace.rms.4bin.before.rt)|animal), data = data_no_na_discr01,
				             family = "binomial")
R2_SGS1 <- R2out(gmodel.SGS1, data_no_na_discr01, "discr01",
                "SGS1.trace.rms.4bin.before.rt", "false_alarm_below_6_0_miss_1")



out_discr01_famiss <- rbind(R2_avrec, R2_GS1, R2_SGS1, R2_Va, R2_Vb, R2_VI)
 
 

out_answer  <- rbind(out_det01_hitmiss, out_discr01_hitmiss, out_discr01_facr,
 										 out_discr01_hitcr, out_discr01_famiss)
 
 
# also save into pipeline for R2m-Plots:
file_name <- paste0(mainDirPipeline, "4,5_GLMM_answers_4bin.csv")
write.table(x = out_answer, file_name,
   sep = ",", dec = ".", row.names = FALSE)


