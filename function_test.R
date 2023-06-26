message("Loading custom functions...")



back2forward <- function(path) {
  gsub("\\\\", "/", path)
} #a =  "c:/asf/asf/asf"

#############################################################################################################
## Some helpful functions 
#############################################################################################################
# install a library if it is not already installed; arguments to load and suppress warnings and error messages
# This is a helper function often used for other functions
get_Packages <- function(pkglist, load = F, suppressall = T) {
    for (p in pkglist) {                             # for each package
      if (!is.element(p, installed.packages()[,1]))  # check
            install.packages(p, dep = TRUE)
      # if wanting to load
      if (load) {                                   
        if (suppressall) {                           # to suppress warnings and messages
          suppressMessages(suppressWarnings( require(p, character.only = TRUE))) }
          else { require(p, character.only = TRUE)   # without suppression
          }
      }
    }
}  # get_Packages(c("gganimate", "car", "boot", "dendextend"), load = F, suppressall = F)


#load.pkgs <- function(pkg = c()) {sapply(pkg, require, character.only = TRUE) } # if problematic, use require

#############################################################################################################
# get the active Rmd file path
get_ActivePath <- function() { 
    get_Packages(c("rstudioapi"))
    rstudioapi::getActiveDocumentContext()$path 
}

#############################################################################################################
# an alternative to base R's View(); uses DT library
# a better view function
view <- function(object, 
                 rows = T, 
                 show = 100,
                 filter = "top", #c("none", "top", "bottom")
                 ...) { 
    get_Packages(c("DT", "tibble"), load = F) # check if library is installed
    # check appropriateness of data structure
  if (tibble::is_tibble(object)) {
    object = as.data.frame(object)
  }  
  if (is.null(dim(object)) & class(object) == "list") {
        message("Object is a list. Viewer displays last list element. Consider passing each element to view().")
        lapply(object, function(x) { 
          DT::datatable(x, 
                        rownames = rows, 
                        options = list(pageLength = show),
                        filter = filter) }) 
    } else {
        DT::datatable(object, 
                      rownames = rows, 
                      options = list(pageLength = show),
                      filter = filter) 
    }
}

#############################################################################################################

usr <- (tolower(paste0(Sys.Date(), ":", Sys.info()[8],"")))
usr_time <- (tolower(paste0(Sys.info()[8],": ", format(Sys.time(), "%a %b %d %X %Y"), "")))
               
#############################################################################################################

#############################################################################################################
# an alternative to negating %in%
`%ni%` <- Negate(`%in%`) # use as %ni%
#############################################################################################################

#############################################################################################################
# function to extract parameters from model
get_model_parameters <- function(model) {
  
  get_Packages(c("magrittr", "parameters", "dplyr"))
  
   model %>%
    parameters::select_parameters() %>%   # select 
    parameters::model_parameters() %>%    # model
    as.data.frame() %>%                   # convert into data frame
    dplyr::mutate_if(is.numeric, round, digits = 2) %>% # round
    dplyr::mutate(p, round(p, 3))          #round
}

#############################################################################################################
means <- function(data, 
                  x = NULL, by = NULL, filter = NULL, trim = .1) {
  # summarizes a data frame
  # dependent packages
  dep = c("dplyr", "DescTools", "magrittr"); pkgs = dep[!(dep %in% installed.packages()[,"Package"])]
  if(length(pkgs)) install.packages(pkgs, dep = T)
  
  #dplyr::select_(data, as.name(x))
  # manipulate the data frame
  if (is.data.frame(data)) {
    
    data = data %>% 
      dplyr::group_by_at(by) %>%
      dplyr::summarise(
        n = dplyr::n(),
        mean = mean(.data[[x]], na.rm = T),
        mean.trim = mean(DescTools::Trim(.data[[x]], trim = trim)),
        mdn = median(.data[[x]], na.rm = T),
        sum = sum(.data[[x]], na.rm = T), 
        sd = sd(.data[[x]], na.rm = T),
        se = sd(.data[[x]], na.rm = T) / sqrt(length(na.omit(.data[[x]]))),
        skew = moments::skewness(.data[[x]], na.rm = T),
        kurt = moments::kurtosis(.data[[x]], na.rm = T),
        min = min(.data[[x]], na.rm = T),
        max = max(.data[[x]], na.rm = T),
        mad = mad(.data[[x]], na.rm = T)
      )
  }
  if (is.vector(data)) {
    data = data.frame(x = data, var = gsub(".*\\$", "", deparse(substitute(D$V1))))
    
    data = data %>% 
      dplyr::group_by(var) %>%
      dplyr::summarise(
        n = dplyr::n(),
        mean = mean(x, na.rm = T),
        mean.trim = mean(DescTools::Trim(x, trim = trim)),
        mdn = median(x, na.rm = T),
        sum = sum(x, na.rm = T), 
        sd = sd(x, na.rm = T),
        se = sd(x, na.rm = T) / sqrt(length(na.omit(x) ) ),
        skew = moments::skewness(x, na.rm = T),
        kurt = moments::kurtosis(x, na.rm = T),
        min = min(x, na.rm = T),
        max = max(x, na.rm = T),
        mad = mad(x, na.rm = T)
      )
  }
  return(data)
}


mean.na <- function(x) {
    # calculate the mean by exluding NAs 
    mean(x, na.rm = TRUE)
}

length.na <- function(x, na.rm = FALSE) {
# as length() substitute that calculates length with and without NAs
  if (na.rm) {
    x = length(na.omit(x))
  } else {
    x = length(x)
  }
  return(x)
}
#############################################################################################################
# combining data from pavlovia
pavlovia_Merge <- function(
    dir_path = NULL,
    pattern_select = ".csv",
    pattern_filter = "participant|test"
    ) {
  # 
  if (!is.null(dir_path)) {
  # select names containing patterns
  fileList = list.files(path = dir_path, pattern = pattern_select, full.names = T)
  
  # remove names containing patterns
  fileList = fileList[grepl(pattern_filter, fileList) == FALSE]

  ## read each
  #dat = fileList %>%
  #  tryCatch(
  #    purrr::map_dfr(., readr::read_csv),
  #    error = function(e) NULL
  #    )
#
  # iterate reading csv and apply to list
  dat = lapply(fileList, function(x) {
    tryCatch(
      read.table(x, 
                 header = TRUE, 
                 sep = ','), 
      error = function(e) NULL)
  })
      
  # bind list items to rows of data frame
  dat = dat %>% dplyr::bind_rows() 
  return(dat)
  
  } else { message("Warning: dir_path is NULL ")}
} 
      
#############################################################################################################
# function searches for a file name and extension and returns 
# a vector of full file names matching patters

find_file <- function(pattern = NULL,
                      start_dir = R.home(), # default starting directory
                      recursive = TRUE,
                      full.names = TRUE,
                      return_first = FALSE
                      ) {
  
  if (!is.null(pattern)) {
  
    l = list.files(
      path      = start_dir,
      pattern   = pattern,
      recursive = recursive, 
      full.names = full.names)
    
    if (return_first) l = l[1] # return only first element
    return(l)
    
    } else {
    message("Error: missing pattern argument")
    }
}
#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################
message("Done")