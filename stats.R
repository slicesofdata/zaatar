PyPath = "G:/DropBox/Progs/Python/Dist/python-2.7.10/;G:/DropBox/Progs/Python/Dist/python-2.7.10/lib/site-packages/;G:/DropBox/Progs/Python/Dist/python-2.7.10/Scripts/;"

mdn.ci.boot <- function(x, conf = 0.95) {
    if (!require(boot)) {install.packages("boot")}
    m = na.omit(median(x))
    b = boot::boot(x,function(x,i) median(x[i]), R = 10000)
    bci = boot::boot.ci(b, conf = conf, type = c("norm", "basic" ,"perc", "bca"))
    hist(b$t[,1], col = "darkgray", main = "Histogram of Bootstrapped Medians", xlab = "")
    return(list(Median = m, CI = bci))
}

mean.ci.boot <- function(x, conf = 0.95) {
    if (!require(boot)) { install.packages("boot")}
    m = na.omit(mean(x))
    b = boot::boot(x,function(x,i) mean(x[i]), R = 10000)
    bci <- boot::boot.ci(b, conf = conf, type = c("norm", "basic" ,"perc", "bca"))
    hist(b$t[,1], col = "darkgray", main = "Histogram of Bootstrapped Means", xlab = "")
    return(list(Mean = m, CI = bci))
}

biserial.cor.new = function (x, y, use = c("all.obs", "complete.obs"), level = 1) {
    if (!is.numeric(x))
        stop("'x' must be a numeric variable.\n")
    y <- as.factor(y)
    if (length(levs <- levels(y)) > 2)
        stop("'y' must be a dichotomous variable.\n")
    if (length(x) != length(y))
        stop("'x' and 'y' do not have the same length")
    use <- match.arg(use)
    if (use == "complete.obs") {
        cc.ind <- complete.cases(x, y)
        x <- x[cc.ind]
        y <- y[cc.ind]
    }
    ind <- y == levs[level]
    diff.mu <- mean(x[ind]) - mean(x[!ind])
    prob <- mean(ind)
    diff.mu * sqrt(prob * (1 - prob))/sd.pop(x)
}

st.err <- function(x) {
    sd(x)/sqrt(length(x))
     }

# probability and area demo
prob.iq <- function() {
    if (rstudio_is_available() & require(manipulate)) {
        manipulate(xpnorm(score, 100, 15, verbose=verbose),
                   score = slider(50,170),
                   verbose = checkbox(TRUE, label="Verbose Output"))}}
prob.z <- function() {
    if (rstudio_is_available() & require(manipulate)) {
        manipulate(xpnorm(score, 0, 1, verbose=verbose),
                   score = slider(0,1),
                   verbose = checkbox(TRUE, label="Verbose Output"))}}

# z.test
z.test <- function(data = NULL, xbar = NULL, null.mu = NULL, sigma = NULL, n = NULL, two.tailed = T) {
    # if (missing(data)) | (missing(null.mu)) | (missing(sigma)) {
    #         return(message("object '", substitute(data), "' not previously defined") ) }
    if (is.null(null.mu)) {
        return(message("Error: missing null.mu argument") ) }
    if (is.null(sigma)) {
        return(message("Error: missing sigma argument") ) }
    if (!is.null(data)) {
        if (!is.null(n)) {
            return(message("Error: n should not be included when using data argument")) }
        else {
            data = na.omit(data); xbar = mean(data); n = length(data)
        }}

    z.val = (xbar - null.mu)/(sigma/sqrt(n)); z.val = abs(z.val)

    z.p = 2*pnorm(z.val, lower.tail = F) ; if (two.tailed == F){z.p = z.p/2}
    options(digits = 4)
    report.df = as.data.frame(list(method = "One sample z-test (two-tailed)", mean = round(xbar, 3),
                                   mu = null.mu, diff = round(xbar - null.mu, 3), se = round(sigma/sqrt(n), 3),
                                   z = round(z.val, 4), p.value = z.p, n = n  ))
    #options(digits = 4)
    return(report.df)
}
# z.test(data = c(4,5,6,7,8,9), null.mu = 5, sigma = 15)

t2r <- function(t.object) {
    # take t-value and convert into r and r2
    t.val = round(t.object$statistic[[1]], 3) ; t.p = round(t.object$p.value, 3)
    #if(t.p < .05) { t.p = paste0(t.p, " *") }
    t.df = round(t.object$parameter[[1]], 3)
    t.r = round((sqrt(t.val^2/(t.val^2 + t.df))),3) ; t.r2 = round(t.r^2, 3)
    #        x = cat(t.object$method, "/n")
    #        print(paste0(x, "t = ", t.val, "   df = ", t.df,
    #                     "   p.value = ", t.p, "   r = ", t.r, "   r2 = ", t.r2))
    #print(x)
    report.df = as.data.frame(list(t = t.val, df = t.df, p.value = t.p, r = t.r, r2 = t.r2))
    #        report.df = as.data.frame(report)
    return(report.df)
}

#    fix
t.broom <- function(dataframe, var1, var2 = NULL, type = "one")  {
    library(broom)
    if(type == "one")
        t <- broom::tidy(with(dataframe, t.test(var1, mu = 0)))
    if(type == "i")
        t <- broom::tidy(with(dataframe, t.test(var1 ~ var2, paired = FALSE)))
    if(type == "p")
        t <- broom::tidy(with(dataframe, t.test(var1, var2, paired = TRUE)))
    return(t)
}

cohens.d.one.sample <- function(x, mu, sigma = NULL){
    if (is.null(sigma))  {  # if sigma is not provided
        sigma = sd(x) } # calculate estimate
    d = abs( (mean(x) - mu) / sigma ) # cohen's d
    return(d) # return the value of d
}

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}