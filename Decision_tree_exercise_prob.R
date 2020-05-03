
################################################################################################################

########################### Probabilistic Analysis of the Decision Tree example #################################



nsim <- 100000   # number of iterations 


#################### Estimation of the Decision  Tree:   Method 0.0  ########################################

#initialize C and LE that will store the results from the simulations
C<- LE <- matrix(NA, nrow = nsim, ncol = length(Strategies))
p<- Sys.time()  # Start the clock 
for (i in 1:nsim){
  
  p.PCed <- rbeta(1, 320, 480)   # Probability of early detection PC
  p.HCed <- rbeta(1, 360, 440)   # Probability of early detection HC
  p.RPed <- rbeta(1, 280, 520)   # Probability of early detection RP 
  
  le.ed  <- rnorm(1, 7, 1.2)     # Life expectancy after early detection
  le.ld  <- rnorm(1, 1,0.03)     # Life expectancy after  late detection 
  
  c.PCed <- rnorm(1, 3900,  1000)   # Total costs after early detection with PC
  c.HCed <- rnorm(1, 6200,  1500)   # Total costs after early detection with HC
  c.RPed <- rnorm(1, 3030,   250)   # Total costs after early detection with RP
  c.PCld <- rnorm(1, 12800, 2050)   # Total costs after  late detection with PC
  c.HCld <- rnorm(1, 14400, 1500)   # Total costs after  late detection with HC
  c.RPld <- rnorm(1, 12020,  666)   # Total costs after  late detection with RP
  # for costs
  c.RP <- p.RPed * c.RPed + (1 - p.RPed) * c.RPld # RP cost
  c.PC <- p.PCed * c.PCed + (1 - p.PCed) * c.PCld # PC cost
  c.HC <- p.HCed * c.HCed + (1 - p.HCed) * c.HCld # HC cost
  
  # ...and for effects
  e.RP <- p.RPed * le.ed + (1 - p.RPed) * le.ld   # RP life expectancy
  e.PC <- p.PCed * le.ed + (1 - p.PCed) * le.ld   # PC life expectancy
  e.HC <- p.HCed * le.ed + (1 - p.HCed) * le.ld   # HC life expectancy
  
  LE[i,] <- c(e.RP, e.PC, e.HC)                     # vector of life expectancies
  C[i,]  <- c(c.RP, c.PC, c.HC)                     # vector of total costs
}

 Sys.time() - p  # stop the clock

 
 #################### Estimation of the Decision  Tree:   Method 0.1  ########################################
 ############### Take the RNG outside of the loop
 
 p <- Sys.time()  # Start the clock 
 
 p.PCed <- rbeta(nsim, 320, 480)   # Probability of early detection PC
 p.HCed <- rbeta(nsim, 360, 440)   # Probability of early detection HC
 p.RPed <- rbeta(nsim, 280, 520)   # Probability of early detection RP 
 
 le.ed  <- rnorm(nsim, 7, 1.2)      # Life expectancy after early detection
 le.ld  <- rnorm(nsim, 1,0.03)      # Life expectancy after  late detection 
 
 c.PCed <- rnorm(nsim, 3900,  1000)   # Total costs after early detection with PC
 c.HCed <- rnorm(nsim, 6200,  1500)   # Total costs after early detection with HC
 c.RPed <- rnorm(nsim, 3030,   250)   # Total costs after early detection with RP
 c.PCld <- rnorm(nsim, 12800, 2050)   # Total costs after  late detection with PC
 c.HCld <- rnorm(nsim, 14400, 1500)   # Total costs after  late detection with HC
 c.RPld <- rnorm(nsim, 12020,  666)   # Total costs after  late detection with RP
 
 
 
 C<- LE <- matrix(NA, nrow = nsim, ncol = length(Strategies),
                  dimnames = list( 1:nsim, c("RP", "PC", "HC"))  # names for the elements of the two vectors
)
 for (i in 1:nsim){
   
   # for costs
   c.RP <- p.RPed[i] * c.RPed[i] + (1 - p.RPed[i]) * c.RPld[i] # RP cost
   c.PC <- p.PCed[i] * c.PCed[i] + (1 - p.PCed[i]) * c.PCld[i] # PC cost
   c.HC <- p.HCed[i] * c.HCed[i] + (1 - p.HCed[i]) * c.HCld[i] # HC cost
   
   # ...and for effects
   e.RP <- p.RPed[i] * le.ed[i] + (1 - p.RPed[i]) * le.ld[i]   # RP life expectancy
   e.PC <- p.PCed[i] * le.ed[i] + (1 - p.PCed[i]) * le.ld[i]   # PC life expectancy
   e.HC <- p.HCed[i] * le.ed[i] + (1 - p.HCed[i]) * le.ld[i]   # HC life expectancy
   
   LE[i,] <- c(e.RP, e.PC, e.HC)                     # vector of life expectancies
   C[i,]  <- c(c.RP, c.PC, c.HC)                     # vector of total costs
 }
 
 Sys.time() - p  # stop the clock

#################### Estimation of the Decision  Tree:   Method 1  ########################################

# the solution of the tree is the sum of the weights (probabilities of each leaf) times the reward of every leaf.
 
p <- Sys.time()  # Start the clock 
 
# for costs
c.RP <- p.RPed * c.RPed + (1 - p.RPed) * c.RPld # RP cost
c.PC <- p.PCed * c.PCed + (1 - p.PCed) * c.PCld # PC cost
c.HC <- p.HCed * c.HCed + (1 - p.HCed) * c.HCld # HC cost

# ...and for effects
e.RP <- p.RPed * le.ed + (1 - p.RPed) * le.ld   # RP life expectancy
e.PC <- p.PCed * le.ed + (1 - p.PCed) * le.ld   # PC life expectancy
e.HC <- p.HCed * le.ed + (1 - p.HCed) * le.ld   # HC life expectancy

LE <- cbind(e.RP, e.PC, e.HC)                     # matrix of life expectancies
C  <- cbind(c.RP, c.PC, c.HC)                     # matrix of total costs

Sys.time() - p  # stop the clock

#################### Estimation of the Decision  Tree:   Vectorization method  ########################################
p <- Sys.time()  # Start the clock 

p.ed <- cbind(p.RPed, p.PCed, p.HCed)      # create matrix of probabilities for early detection
c.ed <- cbind(c.RPed, c.PCed, c.HCed)      # create matrix of costs for early detection
c.ld <- cbind(c.RPld, c.PCld, c.HCld)      # create matrix of costs for late detection

C  <- p.ed *  c.ed + (1 - p.ed) *  c.ld          # matrix of life expectancies
LE <- p.ed * le.ed + (1 - p.ed) * le.ld          # matrix of total costs
Sys.time() - p  # stop the clock


