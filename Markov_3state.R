##################################################################################
#### simple 3-state transition model with fixed probabilities over time ##########
##################################################################################

p.SD <- 0.02                                 # probability from stable to diseased
p.SP <- 0.05                                 # probability from stable to progressed
p.PD <- 0.1                                  # probability from progressed to diseased
n.t <- 60                                    # number of cycles
v.n <- c("stable", "progressed","diseased")  # state names
n.s<- length(v.n)                            # number of states
m.P <- rbind(c(1 - p.SD - p.SP, p.SP, p.SD), # Transition matrix
             c(0,            1- p.PD, p.PD),
             c(0,               0,       1))
m.TR <- matrix(NA, nrow = n.t, ncol = n.s, 
               dimnames = list(1:n.t, v.n))   # create Markov trace
m.TR[1,] <- c(1,0,0)                         # initialize Markov trace
for (t in 2:n.t)                             # throughout the number of cycles
{
  m.TR[t, ] <-m.TR[t - 1,] %*% m.P           # estimate the Markov trace for cycle t 
}


plot(1:n.t, m.TR[,"stable"],type = "l", ylim = c(0, 1), ylab = "Proportion of cohort", xlab = "Cycles" )
lines(m.TR[,"progressed"],col = 2)
lines(m.TR[,"diseased"],col = 3)
legend("right", v.n, col = 1:3, lty = rep(1, 3), bty = "n")

