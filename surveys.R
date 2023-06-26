

# survey functions
clean_rename <- function(x, pattern = "|_|//.", replace = "_") {
  return(gsub(pattern = pattern, replacement = replace, x))
}

get_alpha <- function(df) {
  return(c(psych::alpha(df)$total[,1],psych::alpha(df)$total[,2])) 
}


get_alpha_total <- function(df) {
  return(psych::alpha(df)$total) 
}

# for plotting
gplot_likert <- function(df, title = "", 
                         points.no.neutral = NULL, 
                         neutral.point = NULL, 
                         reverse.colors = T, 
                         reverse.scale = T, ...)  {
  if(!require("sjPlot")) { install.packages("sjPlot", dependencies = T) }
  sjPlot::plot_likert(df, 
                      title = title, 
                      catcount = points.no.neutral, 
                      cat.neutral = neutral.point, 
                      reverse.colors = reverse.colors, 
                      reverse.scale = reverse.scale)
}


scale.opt = c("Strongly Like", "Like", "Indifferent", "Dislike", "Strongly Dislike")


#agrep("agree", "agree strongly")


recode.Scale <- function(scale.opt = NULL, values = NULL) {
  if (!is.null(scale.opt) && !is.null(values)) {
    if (length(unique(scale.opt)) == length(unique(values)))
    {
      message('same length')
      opt = sort(c("strongly like", "like", "indifferent", "dislike", "strongly dislike"))
      if (T == unique(sort(unique(scale.opt)) == opt)) {
        car::recode(var, "   "   )
        message("yes")
      }
    }
  }
}

opt = sort(c("strongly like", "like", "indifferent", "dislike", "strongly dislike"))
recode.Scale(scale.opt, c(1,2,3,4,5))



recode.Scale <- function(x, scale.opt = NULL, values = NULL) {
  x = sort(unique(tolower(x)))
  
  value.length = length(unique(values))
  scale.opt.length = length(unique(scale.opt))
  
  if (value.length == scale.opt.length) {
    #  print(x)
    if (!is.null(scale.opt) && !is.null(values)) { 
      
      if ( unique(x == sort(unique(scale.opt))) == T) { # the same as the scale option
        message("yes")
      }
    }
    return(list(options = sort(unique(x)), bins = length(x)))
  } else { message("There is an error in value length")}
}

#sort(unique(scale.opt)) == sort(unique(scale.opt))

recode.Scale(scale.opt, scale.opt, c(1,2,3,4,5))

unique(scale.opt == scale.opt)


#car::recode(x, "'strongly agree
#              ", )
#}

################################################################################
#### search for surveys and return the question count in a data frame
grep_surveys <- function(df, survey.names = c("prmq")) {
  if (!is.null(survey.names)) {
    d = c() 
    for (s in survey.names) {
      survey.cols = names(df)[grep(toupper(s), names(df), grep(tolower(s), names(df)))]
      survey.len  = length(survey.cols) 
      survey.name = unique(gsub(paste0("_|[0-9].*"), "",  survey.cols))
      msg = paste0(survey.name, ": contains ", survey.len, " columns.")
      message(msg)
      
      d = data.frame(rbind(d, c(survey.name, survey.len)))
    }
    names(d) = c("survey", "questions"); return(d) 
  } else {message("Error: No survey abbreviation supplied.")}
}
#grep_surveys(DF)

################################################################################
#### Reverse code a variable or multiple variables
reverse_code <- function(df, scale.range = NULL) {
  if (!is.null(scale.range)) {
    if (is.null(dim(df))) { # pass a data frame, return all reverse coded
      if (length(unique(df)) == length(unique(scale.range)))  { 
        sapply(df, FUN = function(x) rev(scale.range)[x] )
      } else {
        message("Warning: Range and unique values of variable not equal.")
        #sapply(df, FUN = function(x) rev(scale.range)[x] )
      }
    } else { # pass a data frame, return all reverse coded
      apply(df, 2, FUN = function(x) rev(scale.range)[x] )
    }
  } else {
    message("Error: Specify the scale.range values as a vector, e.g., c(1,2,3,4,5).")
  }
} #reverse_code(BIS11[, c("BIS1", "BIS2")], scale.range = c(1,2,3,4))
################################################################################

#names(DF)
# clean up qualtrics
#id.col <- names(DF)[grep("id", names(DF))]
#if (length(DF$id) > 0) {
#  DF$id <- as.numeric(DF$id)
#}


# make a list of surveys to keep
#keep_surveys = c()
