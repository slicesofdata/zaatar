create.pack <- function(name, setwd = NULL) {
    library(devtools); library(roxygen2)
    oldwd = getwd()
    if (!is.null(setwd)) {setwd(setwd)}

    create_package(name)
#    setwd(paste0("./",name))
    setwd(oldwd)
}

create.pack("zzz", setwd = "X:/Progs/R/R-Portable")

rebuild <- function(name = "zaatar", setwd = NULL) {
    #install.packages("devtools"); #devtools::install_github("klutometis/roxygen")

    library(devtools); library(roxygen2)
    oldwd = getwd()

    if (!is.null(setwd)) {setwd(setwd)} # if not null, set it
    setwd(setwd) #"C:/Users/gcook/Sync/Progs/R/R-Portable")

#    create_package(name)
    setwd(paste0("./",name))
    document() # build it

    setwd("..")
    install("zaatar")
    setwd(oldwd)
}