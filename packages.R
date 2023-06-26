# install if not installed, package checking
package.check <- function(package) {
  
  lapply(
    package,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        #library(x, character.only = TRUE)
      }
    }
  )
# suppressPackageStartupMessages(package.check(pkgs))
}

dl.pak <- function(pkg, destDir = sourceDir, load = FALSE, force = FALSE) {
    # download if not there
    if(!file.exists(sourceDir)){
        print("skipping") #download.packages(pkg, destDir = sourceDir, type="source")
    }
}

ipak <- function(pkg, load = FALSE, force = FALSE) {
    # force install without checking if they exist
    if (force) { install.packages(pkg, dependencies = TRUE)  }
    # install if not installed, and load
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) {
	install.packages(new.pkg, dependencies = TRUE)
            # versions::install.dates(new.pkg, checkpoint.date, dependencies = TRUE) # version by date
	download.packages(new.pkg, destDir = sourceDir, type="source")
	}
    if(load) {
        #message("Loading ", new.pkg, "...")
        sapply(pkg, require, character.only = TRUE) # loads all libraries if set to TRUE
    } else { print("Not set to load packages.") }
}

ipak.first <- function(pkg) {
    # install if not installed, and load
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
}

ipak.gits <- function() {
    # install if not installed, and load
    #	if(Sys.info()[[8]] %in% c("GCook","cookprojects")) {}
    if (!"papaja" %in% installed.packages()[, "Package"])
        devtools::install_github("crsh/papaja")
    if (!"multidplyr" %in% installed.packages()[, "Package"])
        devtools::install_github("hadley/multidplyr")
    if (!"rio" %in% installed.packages()[, "Package"])
        devtools::install_github("leeper/rio")
}

load.pkgs <- function(pkg = c()) {sapply(pkg, require, character.only = TRUE) } # if problematic, use require


install.if <- function(packages = NULL) {
    `%notin%` <- Negate(`%in%`)
    pkgs = as.data.frame(installed.packages()[,c(1,3:4)])
    pkgs = pkgs[is.na(pkgs$Priority),1:2, drop = F]
    pkgs = as.vector(as.character(pkgs$Package))
    
    if (!is.null(packages)) {
        for (pkg in packages) {
            if (pkg %notin% pkgs) { 
                install.packages(pkg) 
                } else { message(paste0(pkg, " already installed")) }}}}

reinstall.packages <- function() {
    `%notin%` <- Negate(`%in%`)
    pkgs = as.data.frame(installed.packages()[,c(1,3:4)])
    pkgs = pkgs[is.na(pkgs$Priority),1:2, drop = F]
    pkgs = as.vector(as.character(pkgs$Package))
    
    for (pkg in pkgs) { install.packages(pkg) } }

clean.temp.dir <- function(temp.dir = 'c:/Users/gcook/AppData/Local/Temp/') {
	if (dir.exists(temp.dir)) {
		list.of.dirs <- list.files(temp.dir, pattern = 'Rtmp', full.names = T)
		for (dir in list.of.dirs) {
	    		message(paste0("Deleting: ", dir))
	    		unlink(dir, recursive = T)
		} 
	} else { message('temp.dir does not exst!') }}

