### ====================================================
###     Function for plotting Two-way SA Diagrams
### ====================================================
twsa.plot.det <- function(params, outcomes, 
                          strategyNames, 
                          outcomeName = "Outcome", 
                          mx = T){
  ## Load required packages
  library(ggplot2)
  library(reshape2)
  library(scales)
  
  parm1 <- colnames(params)[1]
  parm2 <- colnames(params)[2]
  if (mx == T){
    strategy <- factor(max.col(outcomes), labels = strategyNames)
  } else {
    strategy <- factor(max.col(-outcomes), labels = strategyNames)
  }
  
  twsa.df <- data.frame(strategy)
  #A simple trick to define my variables in my functions environment
  twsa.df$parm1 <- params[, parm1]
  twsa.df$parm2 <- params[, parm2]
  
  print(
    ggplot(twsa.df, aes(x = parm1, y = parm2))+ 
      geom_tile(aes(fill = strategy)) +
      theme_bw(base_size = 14) +
      xlab(parm1)+
      ylab(parm2)+
      ggtitle("Two-way sensitivity analysis", subtitle = outcomeName) +
      scale_fill_discrete("Strategy: ", l=50) +
      theme(legend.position = "bottom")
  )
}
