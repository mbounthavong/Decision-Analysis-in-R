
#################  Decision trees in R #######################################
## Example from Day 1 of the Decision modeling in R workshop                ##
## Child Health Evaluative Sciences, The Hospital for Sick Children, 2017   ##
## Credits for the R Code: Petros Pechlivanoglou, Fernando Alarid- Escudero ##
## Fernando Alarid Escudero, Hawre Jalal and Mohammad Kaviul Kahn           ##
## Credits for the example: A. Gray et al 2011                              ##
## Applied Methods of Cost-effectiveness Analysis in Healthcare             ##
## Cost effectiveness of follow-up strategies after colon cancer surgery    ##
## Strategies: PC(Primary Care), HC (Hospital Care), RP (Routine Practice)  ##
##############################################################################

rm(list = ls())      # clear memory (removes all the variables from the workspace)

####################### Input Model Parameters   ############################################

Strategies      <- c("Routine Practice", "Primary Care", "Hospital Care")  

p.PCed <- 0.40   # Probability of early detection PC
p.HCed <- 0.45   # Probability of early detection HC
p.RPed <- 0.35   # Probability of early detection RP 

le.ed  <- 7      # Life expectancy after early detection
le.ld  <- 1      # Life expectancy after  late detection 

c.PCed <-  3900  # Total costs after early detection with PC
c.HCed <-  6200  # Total costs after early detection with HC
c.RPed <-  3030  # Total costs after early detection with RP
c.PCld <- 12800  # Total costs after  late detection with PC
c.HCld <- 14400  # Total costs after  late detection with HC
c.RPld <- 12020  # Total costs after  late detection with RP
wtp    <- 10000  # Define WTP


#################### Estimation of the Decision  Tree:   Method 1  ########################################

# the solution of the tree is the sum of the weights (probabilities of each leaf) times the reward of every leaf.

# for costs
c.RP <- p.RPed * c.RPed + (1 - p.RPed) * c.RPld # RP cost
c.PC <- p.PCed * c.PCed + (1 - p.PCed) * c.PCld # PC cost
c.HC <- p.HCed * c.HCed + (1 - p.HCed) * c.HCld # HC cost

# ...and for effects
e.RP <- p.RPed * le.ed + (1 - p.RPed) * le.ld   # RP life expectancy
e.PC <- p.PCed * le.ed + (1 - p.PCed) * le.ld   # PC life expectancy
e.HC <- p.HCed * le.ed + (1 - p.HCed) * le.ld   # HC life expectancy

LE <- c(e.RP, e.PC, e.HC)                     # vector of life expectancies
C  <- c(c.RP, c.PC, c.HC)                     # vector of total costs
names(LE) <- names(C) <- c("RP", "PC", "HC")  # names for the elements of the two vectors



#################### Estimation of the Decision  Tree:  OpenTree method  ########################################

# for costs
c.RP <- c(prod(c(p.RPed)), prod(c(1 - p.RPed))) %*%  c(c.RPed, c.RPld) # RP cost
c.PC <- c(prod(c(p.PCed)), prod(c(1 - p.PCed))) %*%  c(c.PCed, c.PCld) # PC cost
c.HC <- c(prod(c(p.HCed)), prod(c(1 - p.HCed))) %*%  c(c.HCed, c.HCld) # HC cost

e.RP <- c(prod(c(p.RPed)), prod(c(1 - p.RPed))) %*%  c(le.ed, le.ld) # RP life expectancy
e.PC <- c(prod(c(p.PCed)), prod(c(1 - p.PCed))) %*%  c(le.ed, le.ld) # PC life expectancy
e.HC <- c(prod(c(p.HCed)), prod(c(1 - p.HCed))) %*%  c(le.ed, le.ld) # HC life expectancy

LE <- c(e.RP, e.PC, e.HC)                     # vector of life expectancies
C  <- c(c.RP, c.PC, c.HC)                     # vector of total costs
names(LE) <- names(C) <- c("RP", "PC", "HC")  # names for the elements of the two vectors


#################### Estimation of the Decision  Tree:   Vectorization method  ########################################

p.ed <- c(p.RPed, p.PCed, p.HCed)      # create vector of probabilities for early detection
c.ed <- c(c.RPed, c.PCed, c.HCed)      # create vector of costs for early detection
c.ld <- c(c.RPld, c.PCld, c.HCld)      # create vector of costs for late detection

C  <- p.ed *  c.ed + (1 - p.ed) *  c.ld
LE <- p.ed * le.ed + (1 - p.ed) * le.ld
names(LE) <- names(C) <- c("RP", "PC", "HC")  # names for the elements of the two vectors


# estimating pairwise incremental costs and effects
DC      <-  C  - C[1]      # incremental effectiveness
DE      <- LE - LE[1]      # incremental costs
ICER    <- DC / DE         # Incremental cost-effectiveness ratios
ICER[1] <- NA
NMB     <- LE * wtp - C
# create full incremental cost-effectiveness analysis table 
table           <- cbind(C, LE, DC, DE, round(ICER, 2))  # bind together the results into a table
table           <- as.data.frame(table)                  # as the table has both text and numbers, define as a data frame
colnames(table) <- c( "Costs", "LE", "Inc.Cost", "Inc.Effects", "ICER") # give column names
rownames(table) <- Strategies                                           # give row names
write.table(table, "table.txt")                                         # store the table 
table                                                                   # present the reults

# load functions needed for plotting and CEA Frontier
source("CEA_functions.R")

### Get CEA frontier 
ce.mat <- cbind(Strategy = 1:3, 
                Cost = C, 
                Effectiveness = LE)
ce.front <- getFrontier(ce.mat, plot = FALSE)

### Plot frontier
## Using basic `plot` function
plot(ce.mat[ce.front, 3], ce.mat[ce.front, 2], 
     col = 1:3, pch = 1:3,
     xlab = "Effectiveness", ylab = "Cost")
lines(ce.mat[ce.front, 3], ce.mat[ce.front, 2])
legend("bottomright", Strategies, pch = 1:3, col = 1:3, bty = "n", cex = 0.8)
## Using `ggplot2` function
library(ggplot2)
ce.df <- data.frame(Strategy = Strategies,
                    Cost = C,
                    Effectiveness = LE)
ce.df
ce.front <- getFrontier(ce.mat, plot = F)
plotFrontier(CEmat = ce.df, frontier = ce.front)

#####################################################################################################################
######################################## Deterministic Sensitivity Analysis  ########################################
#####################################################################################################################

### Vector of input parameters
input <- data.frame(
  p.PCed = 0.40,   # Probability of early detection PC
  p.HCed = 0.45,   # Probability of early detection HC
  p.RPed = 0.35,   # Probability of early detection RP 
  le.ed  = 7,      # Life expectancy after early detection
  le.ld  = 1,      # Life expectancy after  late detection 
  c.PCed =  3900,  # Total costs after early detection with PC
  c.HCed =  6200,  # Total costs after early detection with HC
  c.RPed =  3030,  # Total costs after early detection with RP
  c.PCld = 12800,  # Total costs after  late detection with PC
  c.HCld = 14400,  # Total costs after  late detection with HC
  c.RPld = 12020   # Total costs after  late detection with RP
)

#### Wrap decision tree into a function ####
dec_tree <- function(params){
  with(
    as.list(params),
    {
      # the solution of the tree is the sum of the weights (probabilities ofeach leaf) times the reward of every leaf.
      # for costs
      c.RP <- p.RPed * c.RPed + (1 - p.RPed) * c.RPld  # Routine practice cost
      c.PC <- p.PCed * c.PCed + (1 - p.PCed) * c.PCld  # Primary Care cost
      c.HC <- p.HCed * c.HCed + (1 - p.HCed) * c.HCld  # Hospital care cost
      
      # and for effects
      e.RP <- p.RPed * le.ed + (1 - p.RPed) * le.ld   # Routine practice effectiveness
      e.PC <- p.PCed * le.ed + (1 - p.PCed) * le.ld   # Primary care effectiveness
      e.HC <- p.HCed * le.ed + (1 - p.HCed) * le.ld   # Hospital care effectiveness
      
      LE <- c(e.RP, e.PC, e.HC)
      C  <- c(c.RP, c.PC, c.HC)
      nmb <- LE * wtp - C
      names(LE) <- paste("LE", c("RP", "PC", "HC"), sep = "_")
      names(C) <- paste("C", c("RP", "PC", "HC"), sep = "_")
      names(nmb) <- paste("NMB", c("RP", "PC", "HC"), sep = "_")
      
      return(c(LE, C, nmb))
    }
  )
}

#=================#
#### One-way SA ###
#=================#
p.PCed_range <- seq(0.30, 0.60, length.out = 50)
## Generate matrix of inputs for decision tree
m.owsa.input <- cbind(p.PCed = p.PCed_range, input[-1])


## Run model and capture NMB
outcomes_NMB <- t(apply(m.owsa.input, 1, dec_tree))[, 7:9]

### Using base R commands
plot(p.PCed_range, outcomes_NMB[, 1], type = "l", xlab = "p.RPed", ylab = "NMB")
lines(p.PCed_range, outcomes_NMB[, 2], col = "red")
lines(p.PCed_range, outcomes_NMB[, 3], col = "green")
legend("bottomright", Strategies, col = 1:3, lty = c(1, 1, 1), bty = "n")

### the ggplot way
source("owsa_diagram_code.R")
paramName <- "p.PCed"
outcomeName <- "Net Monetary Benefit"
owsa.plot.det(param = p.PCed_range, outcomes = outcomes_NMB, paramName = paramName, 
              strategyNames = Strategies, outcomeName = outcomeName)

#=================#
#### Two-way SA ###
#=================#
## Generate full factorial combinations between two different parameters
p.PCed_range <- seq(0.25, 0.45, length.out = 50)
p.HCed_range <- seq(0.30, 0.50, length.out = 50)
params <- expand.grid(p.PCed = p.PCed_range, 
                      p.HCed = p.HCed_range)
head(params)

## Generate matrix of inputs for decision tree
m.twsa.input <- cbind(params, input[-c(1:2)])

## Run model and capture NMB
outcomes_NMB <- t(apply(m.twsa.input, 1, dec_tree))[, 7:9]

## Find optimal strategy with highest NMB
strategy <- max.col(outcomes_NMB)

### Using base R commands
image(p.PCed_range, p.HCed_range, matrix(strategy, ncol = 50), xlab = "p.RPed", ylab = "p.PCed")
title(main = "Two-way Sensitivity Analysis \nNet Monetary Benefit", 
      font.main = 4)

### the ggplot way
source("twsa_diagram_code.R")
outcomeName <- "Net Monetary Benefit"
twsa.plot.det(params = params, outcomes = outcomes_NMB, 
              strategyNames = Strategies, outcomeName = outcomeName)

#===================#
#### Tornado plot ###
#===================#
## Define ranges
p.PCed_range <- c(BaseCase = p.PCed, low = 0.25, high = 0.45)
c.PCed_range <- c(BaseCase = c.PCed, low = 1000, high = 5000)
c.PCld_range <- c(BaseCase = c.PCld, low = 5000, high = 20000)
le.ld_range  <- c(BaseCAse = le.ld, low = 0.5, high = 5)

## Parameter names
paramNames <-  c( "p.PCed",
                  "c.PCed",
                  "c.PCld",
                  "le.ld"
)
## List of inputs

l.tor.in <- vector("list", 4)
names(l.tor.in) <- paramNames
l.tor.in$p.PCed <- cbind(p.PCed = p.PCed_range, input[-1])
l.tor.in$c.PCed <- cbind(c.PCed = c.PCed_range, input[-6])
l.tor.in$c.PCld <- cbind(c.PCld = c.PCld_range, input[-9])
l.tor.in$le.ld  <- cbind(le.ld  = le.ld_range, input[-5])

## List of outputs
l.tor.out <- vector("list", 4)
names(l.tor.out) <- paramNames

## Run model on different parameters
for (i in 1:4){
  l.tor.out[[i]] <- t(apply(l.tor.in[[i]], 1, dec_tree))[, 8]
}


## Data structure: ymean	ymin	ymax
m.tor <- matrix(unlist(l.tor.out), nrow = 4, ncol = 3, byrow = TRUE,
                dimnames =list(paramNames,c("basecase", "low", "high"	)) )
## Plot tornado
source("tornado_diagram_code.R")
TornadoPlot(Parms = paramNames, Outcomes = m.tor, 
            titleName = "Tornado Plot", outcomeName = "NMB on primary care")
