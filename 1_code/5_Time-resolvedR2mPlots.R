
# plots the time-resolved R2m plots, using the outpur in 2_pipeline
# you can either use the already given files, or run the GLMMs yourself


dat <- read.csv(paste0(mainDirPipeline, "4,5_GLMM_answers_4bin.csv"))
dat <- rbind(dat, read.csv(paste0(mainDirPipeline, "4,5_GLMM_answers_3bin.csv")))
dat <- rbind(dat, read.csv(paste0(mainDirPipeline, "4,5_GLMM_answers_2bin.csv")))
dat <- rbind(dat, read.csv(paste0(mainDirPipeline, "4,5_GLMM_answers_1bin.csv")))

for(currPhase in c("detect01", "discr01")){
  
  binaryVars <- unique(dat[dat$phase == currPhase,]$current_answer_binary)

  for(currBinary in binaryVars){
    currDat <- dat[dat$phase == currPhase & dat$current_answer_binary == currBinary,]

    # read out data and make it into plottable data frame
    avrecR2m <- currDat[grepl("avrec", currDat$current_variable), c("R2m", "p")]
    GS1R2m   <- currDat[grepl("^GS1", currDat$current_variable), c("R2m", "p")]
    SGS1R2m  <- currDat[grepl("SGS1", currDat$current_variable), c("R2m", "p")]
    VaR2m    <- currDat[grepl("Va", currDat$current_variable), c("R2m", "p")]
    VbR2m    <- currDat[grepl("Vb", currDat$current_variable), c("R2m", "p")]
    VIR2m    <- currDat[grepl("VI", currDat$current_variable), c("R2m", "p")]
    
    datPlot      <- rbind(avrecR2m, GS1R2m, SGS1R2m, VaR2m, VbR2m, VIR2m)
    datPlot$var  <- factor(rep(c("avrec", "GS1", "SGS1", "Va", "Vb", "VI"), each = 4)) 
    datPlot$bbRT <- factor(rep(c("4binsbeforeRT", "3binsbeforeRT",
                                 "2binsbeforeRT", "1binbeforeRT"), 6))
    datPlot$pcut <- cut(datPlot$p, c(-Inf, 0.001, 0.01, 0.05, Inf))
    levels(datPlot$pcut) <- c("p < .001", "p < .01", "p < .05", "ns.")

    # plotting:
    p1 <- ggplot(datPlot, aes(x = bbRT, y = R2m, group = var)) + 
      geom_line(aes(color = var), size = 1) +
			ylim(0, 0.55) +
      geom_point(aes(group = pcut, fill = pcut), shape = 21, size = 3) +
      scale_color_viridis(discrete = TRUE, option = "D") +
      scale_fill_grey(start = 0, end = 1, drop = FALSE) +
      theme_bw()    
   
   file_name <-  paste0(mainDirPlots, "Figure7_", currPhase, "_", currBinary, ".pdf")
   pdf(file = file_name, width = 10, height = 8)  
   print(p1)
   dev.off()    
 
  }
} 
