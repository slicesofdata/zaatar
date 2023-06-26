Rfiles = list.files(".", pattern = ".R") # list R files

# for DMC (Dynamic Models of Choice, https://osf.io/pbwx8/)
dmc.pkgs <- c("truncdist" # truncated t etc.
,"msm"  # For truncated normal priors 
,"loo" # For WAIC and looaic calculation
,"hypergeo" # For population plausible values
,"statmod" # Wald model
,"rtdists" # For standard model distribution 
,"pracma"  # For gng and stop signal robust integration
,"snowfall" # Parallel processing
,"rlecuyer" # Parallel processing
,"numDeriv" # Prior transformations
,"vioplot" # Stop signal graphs
,"ggplot2" # For fancy graphs
,"gridExtra" # For fancy graphs
,"mvtnorm" # For Bayes Factors
,"Matrix" # For Bayes Factors
,"Brobdingnag" # For Bayes Factors
,"stringr" # For Bayes Factors
,"LaplacesDemon" # For multivariate Cauchy
)

