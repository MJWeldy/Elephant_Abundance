library(R2jags)

# mean group size
groupsize <- mean(mean_cluster_size$mean,na.rm=TRUE)

# Moments estimator
theta  <-  0.45
radius <- 0.012
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
  theta = 0.45, 
  t = 1,
  pie = pi,
  n_camera = nrow(y),
  #n_camera = length(y2),
  n_visit = ncol(y)-1,
  groupsize = groupsize
)


modelstring <- textConnection(
  "
	  model{
	  #radius ~ dunif(0.005,0.03) 
	  radius <- 0.018288
	  #radius <- 0.012
	  # mu_r ~ dnorm(45, 0.001)
	  # sd_r ~ dunif(0,10)
	  # tau_r <- pow(sd_r, -2)
	  density ~ dunif(0,10)
	  
    # for (i in 1:n_camera) {
      # radius[i] ~ dnorm( mu_r ,tau_r)
      # A[i] <- radius[i]*t*((2+theta)/pie)
      # lambda[i] <- A[i]*V*density
    # }
		A <- radius*t*((2+theta)/pie)
	  lambda <- A*V*density
		
		for (i in 1:n_camera) {
  		 #y[i] ~ dpois(lambda)
		  for (j in 1:n_visit) {
		    y[i,j] ~ dpois(lambda)
		  }
		}
		N <- density*780*groupsize
    }
	"
)
parameters <- c("A","density","radius", "N")

inits <- function() {
  list()
}
ni <- 5000
nt <- 1
nb <- ni/2
nc <- 3
Model <- jags(data, inits, parameters, modelstring, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

# groupsize 1.2268
# total groups 473.5898
# groups per km 0.607



View(Model$BUGSoutput$summary)
modelstring <- textConnection(
  "
	  model{
	  # mu_r ~ dnorm(45, 0.001)
	  # sd_r ~ dunif(0,10)
	  # tau_r <- pow(sd_r, -2)
	  #radius ~ dunif(0.005,0.03) 
	  radius <- 0.018288
	  # radius <- .045
	  density ~ dunif(0,10)
	  
	  r ~ dunif(0,50)
	  
# 		for (i in 1:n_camera) {
#   		radius[i] ~ dnorm( mu_r ,tau_r)
# 	  	A[i] <- radius[i]*t*((2+theta)/pi)
# 	  	lambda[i] <- A[i]*V*density #m2*m/s*C/m2
# 	  	p[i] <- r/(r+lambda[i])
# 		}

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
NB_Model <- jags(data, inits, parameters, modelstring, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

print(paste0("Moments: :", N, "Poisson: ", Model$BUGSoutput$summary["N",],"NB: ", NB_Model$BUGSoutput$summary["N",]))