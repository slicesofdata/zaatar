mkdir <- function(path = NULL, dir = getwd(), ...) {
  if (!is.null(path)) {
    
    # add dir to path, defaults to working dir
    path = paste(dir, path, sep = "/")
    
    # apply to each path
    lapply(X = path, #paste(dir, path, sep = "/"), 
           FUN = function(x) 
           suppressWarnings(dir.create(x, showWarnings = F, ...))
         )
  } else { message("Directory names missing from 'path' ") }
}

# mkdir(path = c("0dat1"))