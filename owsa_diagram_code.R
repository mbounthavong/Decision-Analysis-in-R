### ====================================================
###     Function for plotting One-way SA Diagrams
### ====================================================
owsa.plot.det <- function(param, outcomes,
                          paramName,
                          strategyNames, 
                          outcomeName = "Outcome"){
  ## Load required packages
  library(ggplot2)
  library(reshape2)
  library(scales)
  
  owsa.df <- data.frame(outcomes)
  colnames(owsa.df) <- strategyNames
  owsa.df$param <- param
  
  print(
    ggplot(data = melt(owsa.df, id.vars = "param", 
                       variable.name = "Strategy"), 
           aes(x = param, y = value, color = Strategy)) +
      geom_line() +
      xlab(paramName) +
      ggtitle("One-way sensitivity analysis", subtitle = outcomeName)+
      theme_bw(base_size = 14) +
      theme(legend.position = "bottom")
    
  )
}
