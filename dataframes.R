list.as.data.frame <- function(list) {do.call(rbind.data.frame, list)}



split.csv.to.files <- function(list, sep = ",", ext = ".csv", dir = "", row.names = F, col.names= F) {
    # split data frame into multiple files based on variable (breaks out into list)
    if (!is.null(dir)) {
	    dir.create(dir);
		lapply(1:length(list), function(i) write.table(list[[i]], sep = sep,
                                                   file = paste(dir,paste0(names(list[i]),ext), sep = "/"),
											 row.names = row.names, col.names = col.names))  
	}
}