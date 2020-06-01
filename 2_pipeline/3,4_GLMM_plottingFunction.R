##################### defines plotting functions for GLMMs #####################

##==================================================##
## first define multiplot() used within plot_glmm() ##
##==================================================##

# plots several ggplots with the indicated layout
#
# Arguments:
#   - ...:          ggplot elements
#   - plotlist:     defaults to NULL -> list of additional plots
#   - cols:         (num) number of columns for the plot grid, defaults to 1
#   - layout:       if NULL (default), cols determines layout
#   
# Output: - 
#   
# Notes:
#   - requires grid package

multiplot <-
  function(...,
           plotlist = NULL,
           file,
           cols = 1,
           layout = NULL) {
 
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                       ncol = cols,
                       nrow = ceiling(numPlots / cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]],
              vp = viewport(
                layout.pos.row = matchidx$row,
                layout.pos.col = matchidx$col
              ))
      }
    }
  }


# takes model with current dataset, phase and variables
# outputs a summary vector with stats
R2out <- function(gmodel, subset_data_no_na, phase,
									current_variable, current_answer_binary){
  R2_total <- NULL
  n_0 <- eval(parse(
    text  = paste0(
      "table(subset_data_no_na$",
      current_answer_binary,
      ")[1]"
    )
  ))
  n_1 <- eval(parse(
    text  = paste0(
      "table(subset_data_no_na$",
      current_answer_binary,
      ")[2]"
    )
  ))
  n_all <- eval(parse(
    text  = paste0("length(subset_data_no_na$",
                   current_answer_binary, ")")
  ))
      
	# MuMIn:: r.squaredGLMM
  R2 <- r.squaredGLMM(gmodel, null.fit(gmodel, RE.keep = TRUE, evaluate = TRUE))
  R2m <- R2[1, 1]
  R2c <- R2[1, 2]

   
  R2_total <-
    data.frame(
      R2m = round(R2m, 3),
      R2c = round(R2c, 3),
      p = round(summary(gmodel)$coefficients[2,4], 3),
      n_0,
      n_1,
      n_all,
      phase,
      current_variable,
      current_answer_binary
    )
	
	return(R2_total)
}


##==============================##
## now define plotting function ##
##==============================##

plot_glmm <- function(gmodel, subset_data_no_na, mainDir, phase,
											current_variable, current_answer_binary, file_name){
  
  # read out important stats of the model
  tmp_summary <- summary(gmodel)
  intercept <- tmp_summary$coefficients[1]
  slope <- tmp_summary$coefficients[2]
	p <- tmp_summary$coefficients[2,4]
  
	# calculate predicted values using the logistic function
  fun.log_glmer_random_intercept_slope <- function(x){
    1 / (1 + exp(-(slope * x + intercept)))}
      
  x_min <- min(scale(subset_data_no_na[, current_variable]), na.rm = T)
  x_max <- max(scale(subset_data_no_na[, current_variable]), na.rm = T)
      
  # If you want the plots to end at the (longer) whiskers, use this:
  x_min_box <- boxplot.stats(scale(subset_data_no_na[, current_variable]))$stats[c(1, 5)][1]
  x_max_box <- boxplot.stats(scale(subset_data_no_na[, current_variable]))$stats[c(1, 5)][2]

      
	# boxplot
  p1 <- ggplot(subset_data_no_na,
      eval(parse(text  = paste0(
        "aes(x = as.factor(",
        current_answer_binary,
        "), y = scale(",
        current_variable,
        "))"
      )
    ))) +
    geom_boxplot() +
    theme_bw() +
    scale_x_discrete(limits = c(1)) +
    theme(axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) +
    xlab("1") #+
      
      
  # logistic function for each animal
  p2 <-
	  ggplot(subset_data_no_na) +
    theme_bw() +
    stat_smooth(
      eval(parse(text = paste0(
        "aes(y = ",
        current_answer_binary,
        ", x = scale(",
        current_variable,
        "), col = animal)"
      ))),
      method = "glm",
      method.args = list(family = "binomial"),
      se = FALSE,
      fullrange = TRUE) +
    scale_color_manual(guide = FALSE, values = rep("grey", 9)) +
    stat_function(fun = fun.log_glmer_random_intercept_slope,
		  col = "green", size = 2) +
    theme(axis.text.y = element_blank(),
					axis.ticks.y = element_blank()) + ylab(" ") #+
    
		
  p3 <-
	  ggplot(subset_data_no_na,
      eval(parse(text  = paste0(
        "aes(x = as.factor(",
        current_answer_binary,
        "), y = scale(",
        current_variable,
        "))"
      )
    ))) +
    geom_boxplot() +
    theme_bw() +
    scale_x_discrete(limits = c(0)) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    xlab("0") +
    ylab(paste0("scale(", current_variable, ")")) 

  # save everything as pdf
  pdf(file = paste0(mainDir, file_name, ".pdf"), width = 10, height = 8)
      
      
  multiplot(p1 + coord_flip(ylim = c(x_min_box, x_max_box)), 
            p2 + coord_cartesian(xlim = c(x_min_box,x_max_box)), 
            p3 + coord_flip(ylim = c(x_min_box, x_max_box)))
      
  dev.off()
       
  
  R2_total <- R2out(gmodel, subset_data_no_na, phase,
				            current_variable, current_answer_binary)

  p1 <- ggplot(subset_data_no_na,
               #aes(y = jump, x = scale(avrec_rms),  col = animal)) +
               eval(parse(
                 text  = paste0(
                   "aes(y = ",
                   current_answer_binary,
                   ", x = scale(",
                   current_variable,
                   "), col = animal)"
                 )
               ))) +
    # geom_point() +
    theme_bw() +
    stat_smooth(
      method = "glm",
      method.args = list(family = "binomial"),
      se = FALSE,
      fullrange = TRUE
    ) + facet_wrap(~ animal, scales = "free_x") +
    ylim(0,1) + 
    ggtitle(
      paste(
        "R2m:",
        round(R2_total$R2m, 3),
        "R2c:",
        round(R2_total$R2c, 3),
        "p_value:",
        round(R2_total$p, 3),
        phase,
        current_variable,
        current_answer_binary
      )
    )
  
    
  #THIS IS THE PART FOR THE PDF
  file_name <-
    paste0(mainDir, file_name,
           "_animals.pdf")
  
  pdf(file = file_name, width = 10, height = 8)
  
  print(p1)
  
  dev.off()

	return(R2_total)
  
} 

