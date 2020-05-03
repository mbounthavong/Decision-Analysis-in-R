
rm(list = ls()) 

strategies <- c("Control", "Intervention")

p.cancer <- .5
p.death <- .2
p.relapse <- .5
rr <- .8
le.dead <- 0
le.relapse <- 1
le.norelapse <- 8
le.healthy <- 10
c.dead <- 200
c.relapse <- 100
c.norelapse <- 50
c.healthy <- 0
c.int <- 500
wtp <- 50000

input <- data.frame (
  p.cancer <- .5,
  p.death <- .2,
  p.relapse <- .5,
  rr <- .8,
  le.dead <- 0,
  le.relapse <- 1,
  le.norelapse <- 8,
  le.healthy <- 10,
  c.dead <- 200,
  c.relapse <- 100,
  c.norelapse <- 50,
  c.healthy <- 0,
  c.int <- 50,
  wtp <- 50000
)


dec_tree <- function(params){
  with(
    as.list(params),
    {
      #the solution for the tree is the sum of the weights
      outcomes <- matrix(
        c(c.dead, c.relapse, c.norelapse, c.healthy, 
          le.dead, le.relapse, le.norelapse, le.healthy),
        nrow =4,
        ncol = 2)
      
      outcomes_int <- matrix(
        c(c.dead+c.int, c.relapse+c.int, c.norelapse + c.int, c.healthy+c.int, 
          le.dead, le.relapse, le.norelapse, le.healthy),
        nrow =4,
        ncol = 2)
      
      control <- c(prod(c(p.cancer, p.death)), prod(c(p.cancer, 1-p.death, p.relapse)), 
                   prod(c(p.cancer, 1-p.death, 1-p.relapse)), prod(c(1-p.cancer))) %*% outcomes
      
      int <- c(prod(c(p.cancer*rr, p.death*rr)), prod(c(p.cancer*rr, 1-p.death*rr, 
               p.relapse*rr)), prod(c(p.cancer*rr, 1-p.death*rr, 1-p.relapse*rr)), 
               prod(c(1-p.cancer*rr))) %*% outcomes_int
      
      table <- as.data.frame(rbind(control, int))
      colnames(table) <- (c("Costs","LYs"))
      rownames(table) <- strategies
      
      inc <- int - control  
      ICER <- inc[ ,1]/inc[ ,2]
      NMB <- ICER*wtp
      return(NMB)
    }
  )
}

dec_tree(input)


#=================#
#### One-way SA ###
#=================#
p.relpase_range <- seq(0.30, 0.70, length.out = 50)
m.owsa.input <- cbind(p.relapse = p.relpase_range, input[-1])

## Run model and capture NMB
sa_NMB <- t(apply(m.owsa.input, 1, dec_tree))

### Using base R commands
plot(p.relpase_range, sa_NMB, type = "l", xlab = "Probability of Relapse", ylab = "NMB")

### the ggplot way
      # source("owsa_diagram_code.R")
      # paramName <- "p.PCed"
      # outcomeName <- "Net Monetary Benefit"
      # owsa.plot.det(param = p.PCed_range, outcomes = outcomes_NMB, paramName = paramName, 
      #             strategyNames = Strategies, outcomeName = outcomeName)

#=================#
#### Two-way SA ###
#=================#
## Generate full factorial combinations between two different parameters
p.relpase_range <- seq(0.30, 0.70, length.out = 50)
c.int_range <- seq(1.00, 200.00, length.out = 50)
params <- expand.grid(p.relpase = p.relpase_range, 
                      c.int = c.int_range)
head(params)

m.twsa.input <- cbind(params, input[-c(1:2)])
twsa_NMB <- t(apply(m.twsa.input, 1, dec_tree))


