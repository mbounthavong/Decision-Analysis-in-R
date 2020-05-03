####################################################################
####################################################################
getFrontier <- function(CEmat, maxWTP = Inf, plot = TRUE){
  # Name: getFrontier.R
  # Goal: Find the CEA frontier, up to a given WTP level, by 
  #       identifying strategies with the highest NMB
  # Originally written by: Sze Suen on Feb 25, 2015
  # Citation: Suen S-C, Goldhaber-Fiebert JD. An Efficient, 
  #           Noniterative Method of Identifying the Cost-Effectiveness Frontier. 
  #           Med Decis Making. 2015;2–6. 
  # Modified by: Fernando Alarid-Escudero on July 20, 2015 
  # Notes: 
  #    ~Frontier strategies are displaced on the R output screen and 
  #      plotted in red on the scatter plot.  
  #
  #    ~User needs to provide a csv file of costs and QALYs 
  #	    (CostQalyInputFile_online_supp.csv) inside the folder specified    
  #	    below (inputFolder). The CSV should have three columns (labeled 
  #     in first row) in this order: 
  #      Strategy number, costs, and QALYs.
  #
  #    ~User can specify the maximum willingness-to-pay level to 
  #      consider (maxWTP).  Can be Inf for infinity.
  #
  #    ~QALY-reducing strategies will be on the frontier if they save
  #      enough money (script assumes maximum willingness to save money 
  #      per QALY lost is equivalent to maximum willingness to pay per QALY
  #      gained). If the user does not wish to consider such policies as
  #      being on the frontier, do not include strategies with negative 
  #      QALYs in the input csv file.
  #
  #    ~Script does not use the one-line code cited in the text
  #      as the max function is slow. This implementation is
  #      faster and methodologically does the same thing.
  #
  #    ~May take a few minutes if thousands of strategies and 
  #       processing resources are low.  Please be patient.
  #
  #    Please cite article if this code is used.
  #
  # USER INPUTS:
  #inputFolder <- "CostEffectivenessFrontier_MDM/"
  #maxWTP <- Inf        # any positive value or Inf
  
  ## Clean everythng from workspace
  #rm(list=ls())
  ####################################################################
  ####################################################################
  
  # check for duplicated strategies
  dups <- CEmat[c(duplicated(CEmat[,2:3]) | duplicated(CEmat[,2:3], fromLast = TRUE)),1]
  
  # initialize some variables
  costsCol <- 2; qalyCol <- 3
  numStrat <- nrow(CEmat)
  
  # find WTP levels to test so that all strategies on frontier will be captured
  # this means testing on either side of all NMB intersections, which are just all the pairwise ICERs
  ICERmat <- matrix(1, numStrat, numStrat)
  for (i in 1:numStrat ) {
    indexStrat <- matrix(1, numStrat, 3)
    indexStrat[,costsCol] <- indexStrat[,costsCol]*CEmat[i,costsCol]
    indexStrat[,qalyCol] <- indexStrat[,qalyCol]*CEmat[i,qalyCol]
    delCostQalys <- CEmat - indexStrat
    ICERmat[,i] <- delCostQalys[,costsCol] / delCostQalys[,qalyCol]
  }  
  intersections <- sort(unique(c(ICERmat)))
  intersections <- intersections[is.finite(intersections)]
  WTPtestPoints <- c(0, intersections [intersections >= 0 & intersections <= maxWTP ], maxWTP)
  
  # Find the strategy with the max NMB at each of the WTP test points
  indiciesOfMax <- vector()
  NMBmat <- matrix(0, numStrat, length(WTPtestPoints))
  for (i in 1:length(WTPtestPoints) ) {
    NMBmat[,i] <- (WTPtestPoints[i]*CEmat[,qalyCol]) - CEmat[,costsCol]
  }
  if (is.infinite(maxWTP)) {
    #WTP of infinity means costs are not considered
    NMBmat[,length(WTPtestPoints)] = CEmat[,qalyCol] - (0*CEmat[,costsCol]); 
  }
  maxVals <- apply(NMBmat, 2, max)  #find strategy that maximizes NMB at each WTP
  for (i in 1:length(WTPtestPoints) ) {  #find all strategies that match max at each WTP
    indiciesOfMax <- c(indiciesOfMax,which( NMBmat[,i] == maxVals[i]))
  }
  frontier <- unique(indiciesOfMax)  #find strategy that maximizes NMB at each WTP
  
  if (plot == TRUE){
    # display out: make plot and print to output screen
    plot(CEmat[frontier,qalyCol], CEmat[frontier,costsCol], col = 'red', pch = 16, 
         xlab = "Effectiveness", ylab = "Cost")
    points(CEmat[,qalyCol], CEmat[,costsCol])
    lines(CEmat[frontier,qalyCol], CEmat[frontier,costsCol])
    if (length(dups)>0){
      warning("Strategies have the same costs and benefits (displayed above)")
      print(dups)
    }
  }
  sprintf("Frontier is formed by strategies: %s", paste( sort(CEmat[frontier,1]), collapse=" "))
  
  return(frontier)
}

plotFrontier <- function(CEmat, frontier, 
                         ncol = 1,
                         coord.flip = F,
                         txtsize = 12)
{
  # A function to plot CE frontier
  # USER INPUTS:
  #   CEmat: A CE matrix arranged as: Col1: Strategy; Col2: Cost; Col3: Effectiveness
  # Create a dataframe from matrix
  CEmat.df <- data.frame(CEmat)
  colnames(CEmat.df)[3] <- "Effectiveness"
  n.strategies <- nrow(CEmat.df)
  # Make Strategies as factor
  CEmat.df$Strategy <- as.factor(CEmat.df$Strategy)
  #
  if (coord.flip == T){
    ggplot(CEmat.df, aes(Effectiveness, Cost)) +
      geom_point(aes(color = Strategy, shape = Strategy), size = 4) + 
      coord_flip() +
      ggtitle("Cost-Effectiveness Frontier") +
      geom_point(data = CEmat.df[frontier,], 
                 aes(Effectiveness, Cost, shape = Strategy, color = Strategy), size = 4) +
      geom_line(data = CEmat.df[frontier,], aes(Effectiveness, Cost)) +
      scale_shape_manual(values = 0:(n.strategies-1)) +
      guides(shape = guide_legend(ncol = ncol)) +
      theme_bw() +
      theme(title = element_text(face = "bold", size = txtsize+2),
            axis.title.x = element_text(face = "bold", size = txtsize),
            axis.title.y = element_text(face = "bold", size = txtsize),
            axis.text.y = element_text(size = txtsize),
            axis.text.x = element_text(size = txtsize))
  } else {
    ggplot(CEmat.df, aes(Effectiveness, Cost)) +
      geom_point(aes(color = Strategy, shape = Strategy), size = 4) + 
      ggtitle("Cost-Effectiveness Frontier") +
      geom_point(data = CEmat.df[frontier,], 
                 aes(Effectiveness, Cost, shape = Strategy, color = Strategy), size = 4) +
      geom_line(data = CEmat.df[frontier,], aes(Effectiveness, Cost)) +
      scale_shape_manual(values = 0:(n.strategies-1)) +
      guides(shape = guide_legend(ncol = ncol)) +
      theme_bw() +
      theme(title = element_text(face = "bold", size = txtsize+2),
            axis.title.x = element_text(face = "bold", size = txtsize),
            axis.title.y = element_text(face = "bold", size = txtsize),
            axis.text.y = element_text(size = txtsize),
            axis.text.x = element_text(size = txtsize))
  }
}

#### Formatting Functions ####
number_ticks <- function(n) {function(limits) pretty(limits, n)} #Function for number of ticks in ggplot

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
