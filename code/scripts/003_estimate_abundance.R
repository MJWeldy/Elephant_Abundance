library(R2jags)

# mean group size
groupsize <- mean(mean_cluster_size$mean,na.rm=TRUE)

# Moments estimator
#theta  <-  0.45 #degrees
theta <- 0.785398 # radians
radius <- 0.018288
t <- 1
A <- ((2 + theta)/pi)*radius*t
denisty_REM  <- (mean(as.matrix(y[,-1]), na.rm=TRUE)/A/ V)
(N <- denisty_REM * 780 * groupsize )

#Moments estimator by week
# N <- c()
# for(i in 2:ncol(y)){
# denisty_REM  <- (mean(as.matrix(y[,i]), na.rm=TRUE)/A/ V)
# (N[i] <- denisty_REM * 780 * groupsize )
# 
# }
# mean(N,na.rm=TRUE)


# Should we consider the NB model over the Poisson?
mean(as.matrix(y[,-1]), na.rm=TRUE) < var(unlist(y[,-1]), na.rm=TRUE)


data <- list(
  y = as.matrix(y[,-1]),
  V = V,
  theta = theta, 
  t = 1,
  pie = pi,
  n_camera = nrow(y),
  #n_camera = length(y2),
  n_visit = ncol(y)-1,
  groupsize = groupsize#,
  #radius = radius
)

modelstring <- textConnection(
  "
	  model{
	  radius ~ dnorm(0.014, tau_r)
	  sd_r <- 0.001
	  tau_r <- pow(sd_r,-2)

	  density ~ dunif(0,10)
	  
		A <- radius*t*((2+theta)/pie)
	  lambda <- A*V*density
		
		for (i in 1:n_camera) {
		  for (j in 1:n_visit) {
		    y[i,j] ~ dpois(lambda)
		  }
		}
		N <- density*780*groupsize
    }
	"
)
parameters <- c("density","radius", "N")

inits <- function() {
  list()
}
ni <- 5000
nt <- 1
nb <- ni/2
nc <- 3
Model_mean <- jags(data, inits, parameters, modelstring, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

modelstring <- textConnection(
  "
	  model{
	
	  density ~ dunif(0,10)
	  
    for (i in 1:n_camera) {
      radius[i] ~ dunif(0.01,0.02)
      A[i] <- radius[i]*t*((2+theta)/pie)
      lambda[i] <- A[i]*V*density
    }
		
		for (i in 1:n_camera) {
		  for (j in 1:n_visit) {
		    y[i,j] ~ dpois(lambda[i])
		  }
		}
		N <- density*780*groupsize
    }
	"
)
parameters <- c("density","radius", "N")

inits <- function() {
  list()
}
ni <- 5000
nt <- 1
nb <- ni/2
nc <- 3
Model_RE <- jags(data, inits, parameters, modelstring, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)


modelstring <- textConnection(
  "
	  model{
    radius ~ dnorm(0.014, tau_r)
	  sd_r <- 0.001
	  tau_r <- pow(sd_r,-2)
	  density ~ dunif(0,10)
	  
	  r ~ dunif(0,50)

		A <- radius*t*((2+theta)/pie)
	  lambda <- A*V*density
	  p <- r/(r+lambda)
	
		for (i in 1:n_camera) {
		  for (j in 1:n_visit) {
		    y[i,j] ~ dnegbin(p,r)
		  }
		}
		N <- density*780*groupsize
    }
	"
)
parameters <- c("density","radius", "r","N")

inits <- function() {
  list()
}
ni <- 5000
nt <- 1
nb <- ni/2
nc <- 3
NB_Model_mean <- jags(data, inits, parameters, modelstring, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)


modelstring <- textConnection(
  "
	  model{

	  density ~ dunif(0,10)
	  
	  r ~ dunif(0,50)
	  
		for (i in 1:n_camera) {
  		radius[i] ~ dunif(0.01,0.02)
      A[i] <- radius[i]*t*((2+theta)/pie)
      lambda[i] <- A[i]*V*density
	  	p[i] <- r/(r+lambda[i])
		}
	
		for (i in 1:n_camera) {
		  for (j in 1:n_visit) {
		    y[i,j] ~ dnegbin(p[i],r)
		  }
		}
		N <- density*780*groupsize
    }
	"
)
parameters <- c("density","radius", "r","N")

inits <- function() {
  list()
}
ni <- 5000
nt <- 1
nb <- ni/2
nc <- 3
NB_Model_RE <- jags(data, inits, parameters, modelstring, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

#print(paste0("Moments: :", N, " Poisson: ", Model$BUGSoutput$summary["N","mean"]," NB: ", NB_Model$BUGSoutput$summary["N","mean"]))
