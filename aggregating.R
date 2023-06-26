dpath <- function(x = "C:/Users/gcook/Sync/Data/"){
    x = x ; setwd(x); cat("Working Directory is:",getwd()); return(x) }

dirs <- function(projdir, setwd = F, getwd = F) {
    projdir <- paste0(projdir); dir.create(projdir, showWarnings = FALSE)
    proj <- basename(projdir); 
    datadir <- paste0(projdir,"/",basename(projdir),"_Analyses/"); dir.create(datadir, showWarnings = FALSE)
    analysisdir <- datadir; dir.create(analysisdir, showWarnings = FALSE)
    batchdir <- paste0(projdir,"/",basename(projdir),"_RawData/"); dir.create(batchdir, showWarnings = FALSE)
    message("projdir     = ", projdir);
    message("datadir     = ", datadir)
    message("analysisdir = ", analysisdir);
    message("batchdir    = ", batchdir)
    if (setwd) {setwd(projdir)} ; if (getwd) { message("Working directory: ",getwd())  } 
    return(list(projdir = projdir, proj = proj, datadir = datadir, analysisdir = analysisdir, batchdir = batchdir))
}

aggy.raw <- function(pattern = "", names = "", batchdir = batchdir, drop = NULL,
                 analysisdir = analysisdir, ext = ".raw", print = T, rm.pattern = "9999") {
    if (print) {
        message("batchdir: ",batchdir); message("analysisdir: ",analysisdir) 
        message("******************************")
    }
    if (pattern != "") { #& !is.null(analysisdir) ) {
        l = list.files(batchdir, pattern = pattern, full.names = T);
#        l = l[lapply(l,function(x) file.size(x) > 0) == T ] # if file size > 0
#	l = l[lapply(l, function(x) file.info(x)[["size"]] > 0]
	l = l[ file.info(l)[["size"]] > 0]
        l = l[lapply(l,function(x) length(grep(ext, x, value = T))) == 1] # get only matches 
	for (pat in rm.pattern) { 
	    l <- l[lapply(l,function(x) length(grep(pat,x,value = FALSE))) == 0]	
	} 
        l <- do.call(rbind, lapply(l, read.csv, header = F))
	print(str(l))
	if (!is.null(drop)) {
		l <- l[as.numeric(as.character(l[,1])) < drop, ]
	}
        if (length(names) == length(names(l))) {
		names(l) = names 
        	message("Writing new agg data file..."); Sys.sleep(1)
		write.csv(l, paste0(analysisdir,"0.",pattern,".csv"), row.names = F)
        	message(paste0("Saved: ",analysisdir,"0.",pattern,".csv"))
        } else { 
		message("Column names are not the same length.")  
	}
        #print(head(l))
    } else {   message("No file pattern or 'analysisdir' not specified.")    }
	message("Done!") 	
    return(l)}


