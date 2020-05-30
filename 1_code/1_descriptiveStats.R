###############################################################################
##========================= 1_descriptiveStats.R ============================##
###############################################################################

# plots the reaction time histograms for the detection and discrimination
# phase
#
# input: gesamt_data from 0_masterFile.R
# output_ Figure1c_detect.pdf, Figure1c_discrimination.pdf

# exclude all reaction times above 6 seconds
gesamt_data_rt_smaller_6 <- subset(gesamt_data, rt <= 6)

## barchart for detection phase
temp_data <- subset(gesamt_data_rt_smaller_6, phase == "detect01")

# sort data into bins according to reaction time  
temp_data$RTbin <- findInterval(temp_data$rt, c(seq(0, 6, length.out = 40)))

# plot barchart()
p1 <- ggplot(temp_data, aes(x = RTbin, fill = answer)) +
      geom_bar() + 
      scale_x_continuous(breaks = seq(1, 40, length.out = 13),
                         labels = seq(0,6,0.5),
                         expand = expand_scale(add = c(1, 1))) +
      facet_wrap(~phase) +  # facet_wrap to make it prettier: phase on top
      geom_vline(xintercept = seq(1, 40, length.out = 5), col = "red") +
      theme_bw()
   
# save as pdf        
file_name <-  paste0(mainDirPlots, "Figure1c_detection.pdf")
pdf(file = file_name, width = 10, height = 8)  
print(p1)
dev.off()
     

## barchart for detection phase
temp_data <- subset(gesamt_data_rt_smaller_6, phase == "discr01")

# sort data into bins according to reaction time  
temp_data$RTbin <- findInterval(temp_data$rt, c(seq(0, 6, length.out = 40)))

# plot barchart()
p1 <- ggplot(temp_data, aes(x = RTbin, fill = answer)) +
      geom_bar() + 
      scale_x_continuous(breaks = seq(1, 40, length.out = 13),
                         labels = seq(0,6,0.5),
                         expand = expand_scale(add = c(1, 1))) +
      facet_wrap(~phase) +  # facet_wrap to make it prettier: phase on top
      geom_vline(xintercept = seq(1, 40, length.out = 5), col = "red") +
      theme_bw()
      
# save as pdf     
file_name <-  paste0(mainDirPlots, "Figure1c_discrimination.pdf")
pdf(file = file_name, width = 10, height = 8)  
print(p1)
dev.off()
