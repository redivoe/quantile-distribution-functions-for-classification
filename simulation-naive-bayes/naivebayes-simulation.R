
rm(list = ls())
pkgs <- c("here",
          "mvtnorm",   # for simulated data (rmunif(), gendata())
          "klaR",      # function klaR::NaiveBayes
          "tidyverse", # for my functions
          "quadprog",  # for constrained ls in fit_*_ls
          "evd",       # for naivebayes(.. distr = "gev")
          "foreach",   # packages for parallel computing
          "doParallel",
          "doRNG"
)
sapply(pkgs, require, character.only = TRUE)

my_functions<-c("naive_bayes",
                "predict_naive_bayes",
                "invQ",
                "dQ",
                "distributions",
                "fitting-functions")
purrr::walk(my_functions,\(f) source(here("..", "functions", paste0(f,".R"))))

# Define covariance matrix of dimension n 
# NOT RANDOM
gencov <- function(n){
  sigma <- seq(n + 1, 2, length = n)^seq(1.1, 1.9, length = n)
  sigma <- sigma/max(sigma)
  
  A <- qr.Q(qr(matrix(seq(1, 2, length = n^2), n)))
  
  Sigma <- crossprod(A, A*sigma)
  A <- cov2cor(Sigma)
  cr <- range(A[upper.tri(A, diag = FALSE)])
  
  attr(Sigma, "corrange") <- cr
  # cat("Correlation ranges from", cr[1], "to", cr[2], "\n")
  
  return(Sigma)
}

# Random sampling from multivariate uniform distribution
# F(x) ~ Unif
rmunif <- function(n, sigma, R = 1){
  # replicate is wrapper for sapply
  x <- replicate(R, rmvnorm(n, sigma = sigma))
  pnorm(x)
}

# n1 and n2 are the number of observations for the 2 classes of the response
gendata <- function(distr, n1, n2 = NULL, sigma, R = 1, rescale = FALSE){
  p <- nrow(sigma)
  if(is.null(n2)) n2 <- n1
  U <- rmunif(n1 + n2, sigma = sigma, R = R)
  X <- switch(
    distr,
    norm = qnorm(U),
    t = qt(U, df = 3),
    logabst = log(abs(qt(U, df = 3))),
    ilogabst = log(abs(qt(U, df = 3))),
    # exp = qexp(U, rate = 0.25),
    exp = qexp(U, rate = 0.5)
  )
  
  dimnames(X) <- list(label = rep(c("a","b"),c(n1,n2)),
                      var = paste0("x",1:p),
                      rep = 1:R)
  
  # if(distr == "ilogabst"){
  #   X <- sweep(X, MARGIN = "label", STATS = rep(c(1, -1), c(n1, n2)), FUN = "*")
  # }
  if(distr == "ilogabst"){
    X <- sweep(X, MARGIN = "label", STATS = rep(c(1, 0.8), c(n1, n2)), FUN = "*")
  } else {
    X[(n1+1):(n1+n2),,] <- sweep(X[(n1+1):(n1+n2),,],
                                 MARGIN = "var",
                                 STATS = 0.3*(-1)^(1:p), FUN = "+")
  }
  
  if(rescale){
    sda <- apply(X[1:n1,,,drop=FALSE], 3, function(x) apply(x, 2, sd))
    sdb <- apply(X[(n1+1):(n1+n2),,,drop=FALSE], 3, function(x) apply(x, 2, sd))
    sdm <- (sda+sdb)/2
    X <- sweep(X, c("var","rep"), STATS = sdm, FUN = "/")
  }
  
  return(X)
}


model_names <- c("normal", "kde", "discrete", "fgld", "quad", "gev")

classify <- function(x, z){
  # gendata() returns class labels on the rownames of the data.frame
  clx <- rownames(x)
  clz <- rownames(z)
  # labs <- c("a","b")
  
  # Vector of test misclassification rates for each model
  res <- array(NA,
               dim = length(model_names),
               dimnames = list(model_names))
  
  time <- array(NA,
                dim = length(model_names),
                dimnames = list(model_names))
  

  time["normal"] <- system.time({
  
    # Naive Bayes Normal
    fit_normal <- klaR::NaiveBayes(x, as.factor(clx), usekernel = FALSE)
    res["normal"] <- class_err(predict(fit_normal, z)$class, clz)

  })["elapsed"]
  
  
  
  time["kde"] <- system.time({
    
  # Naive Bayes KDE
  fit_kde <- klaR::NaiveBayes(x, as.factor(clx), usekernel = TRUE)
  res["kde"] <- class_err(predict(fit_kde, z)$class, clz)
  
  })["elapsed"]
  
  
  
  command_fit <- purrr::partial(naive_bayes,
                                y = as.factor(clx),
                                X = data.frame(x),
                                fitted = FALSE)
  command_predict <- purrr::partial(predict_naive_bayes, X = data.frame(z))
  
  time["discrete"] <- system.time({
  
  fit_discrete <- command_fit(distr = "discrete")
  res["discrete"] <- class_err(command_predict(fit_discrete)$class, clz)
  
  })["elapsed"]
  
  
  time["fgld"] <- system.time({
    
  fit_fgld <- command_fit(distr = "fgld")
  res["fgld"] <- class_err(command_predict(fit_fgld)$class, clz)
  
  })["elapsed"]

  time["quad"] <- system.time({
    
    fit_quad <- command_fit(distr = "quad")
    res["quad"] <- class_err(command_predict(fit_quad)$class, clz)
    
  })["elapsed"]
  
  
  time["gev"] <- system.time({
    
  fit_gev <- command_fit(distr = "gev")
  res["gev"] <- class_err(command_predict(fit_gev)$class, clz)
  
  })["elapsed"]
  
  
  return(list("res" = res, "time" = time))
}

class_err <- function(x1, x2){
  z <- xtabs(~x1+x2) # equivalent to table(x1,x2) but better
  1-sum(diag(z))/sum(z)
}


replicate_fit <- function(id){
  cat(id, "\n")
  
  # misclassification rate for each model for each replication (R)
  err <- array(NA,
                dim = c(R, length(models)),
                dimnames = list(rep = 1:R, model = models))
  time <- err
  
  if(cases[id, "cor"] == "yes"){
    sigma <- gencov(n = cases[id, "nvars"])
  } else {
    sigma <- diag(diag(gencov(cases[id, "nvars"])))
  }
  
  # training dataset
  x <- gendata(distr = cases[id, "scenario"], n1 = cases[id, "sample_size"]/2, n2 = NULL,
               sigma = sigma, R = R, rescale = FALSE)
  
  # test dataset
  z <- gendata(distr = cases[id, "scenario"], n1 = cases[id, "sample_size"]/2, n2 = NULL,
               sigma = sigma, R = R, rescale = FALSE)
  
  # classify
  for(j in 1:R){
    outi <- classify(x = x[,,j], z = z[,,j])
    err[j,] <- outi$res
    time[j,] <- outi$time
  }
  
  return(list(err, time))
}


# scenarios
scenario <- c("norm", "t", "logabst", "ilogabst","exp")
nscenario <- length(scenario)

# models
models <- model_names
nmods <- length(models)

# sample size (total)
N <- c(100, 500, 1000)
nn <- length(N)

# dimensionality
P <- c(10, 50, 100)
np <- length(P)

# define simulation cases
cases <- expand.grid(scenario = scenario,
                     nvars = P,
                     cor = c("no", "yes"),
                     sample_size = N)
cases$scenario <- as.character(cases$scenario)
cases$cor <- as.character(cases$cor)

nc <- nrow(cases)

# replications
R <- 100


##------------------------------
##  setting up parallelazation  
##------------------------------

parallel_type <- "psock"
# parallel_type <- "fork"

cores_used <- 7

if(parallel_type=="psock"){
  cl <- makeCluster(spec = cores_used, type = "PSOCK")
  registerDoParallel(cl = cl)
}else{
  registerDoParallel(cores = cores_used)
}

registerDoRNG(123)

if(parallel_type=="psock"){
  out <- foreach(id = 1:nc,
                 .packages = pkgs,
                 .export = ls()
  ) %dopar% replicate_fit(id)
}else{
  out <- foreach(id = 1:nc) %dopar% replicate_fit(id)
}


save.image(file = here("output", "naivebayes-simulation.RData"))

