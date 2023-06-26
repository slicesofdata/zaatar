# negates %in%
`%ni%` <- Negate(`%in%`)

is.recalled <- function(studydf, recalldf, word = "Word",
                         recalledlist = "Recalled", drop = c("")) {
    df = merge(studydf, recalldf)
    matchFunc <- function(needle, haystack) grepl(needle,haystack)
    for (i in 1:nrow(df)) {
        df[i,"Acc"] <- as.integer(as.logical(
            matchFunc(df[i,word],df[i,recalledlist]))) }
    df <- df[ , !(names(df) %in% drop)] }

dollar.to.numeric <- function(x) {  as.numeric(gsub("\\$", "", x))  } 
numeric.to.dollar <- function(x) {  as.character(paste0("$",x))  }


decimal <-         function(x, dec = 2) {format(round(x, dec), nsmall = dec)}
#specify_decimal <- function(x, dec) trimws(format(round(x, dec), nsmall = dec))

char.match <- function(a, b) {    as.numeric(adist(a,b)/nchar(a))  }

toupper.all <- function(df) {data.frame(lapply(df, function(x) {
    ifelse(is.numeric(x), return(x), return(toupper(x))) }))}

rtcheck <- function(df, rt = "RT", longrt = 5000 , shortrt = 1000,
                    bins = c(5000,4500,4000,3500,3000,2750,2500,2250,2000,1500,1000)) {
    bins = unique(c(longrt,bins,shortrt))
    df = as.data.frame(df); x = df[[rt]]
    for (time in bins) {
        rt = length(which(x > time)) / length(df[[rt]]) #dim(df)[1]
    message(paste0("RTs over ",time," ms: ", round(rt,3)*100,"%"))
    }
}

# move back n parent folders.
backdir <- function(bw = 1, reset = NULL) {
    # changes the working directory by moving up
    init <- getwd(); print(init)
    # go back n number of times
    if (!is.null(bw)) {
        for (n in 1:bw) {
            setwd('..') # set get the new dir
            newdir <- getwd() # get the new dir
        }
        #print(newdir)
    }
    if (reset == TRUE) {
        setwd(init)
    }
    return(list(newdir = newdir, init = init))
} # backdir(2, TRUE)
