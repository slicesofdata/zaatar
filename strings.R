countSpaces <- function(s) { sapply(gregexpr(" ", s), function(p) { sum(p >= 0) } ) }

count_char <- function(char = " ", string) { sapply(gregexpr(char, string), function(p) { sum(p >= 0) } ) }

grep.vec <- function(x, needle = ".csv", exists = 1) {
    # removes items from vector containing a string
    if (is.vector(x)) {
    x[ lapply( tolower(x),
        function(x)length(grep(needle,x,value = F))) == exists]
    } else { message(paste0("is not a list"))}}

# strings
inStr  <- function(haystack, needle,...) { grepl(needle, haystack,...) }

inStrs <- function(haystack, needle,...) { 
  for (n in needle) { is(grepl(n, haystack,...)) }}

inStrPos <- function(needle, strings, FUN = regexpr,...) {
  if (length(strings) == 1) { 
    x = as.numeric(sapply(needle, FUN, strings, ignore.case=T,...))
    } else {
      x = sapply(needle, FUN, strings, ignore.case=T,...)[,1]
      x = ifelse(x > 0, x, 0) 
    }
  return(x)
}
#inStrPos(c("z"), c("az","tt"), regexpr)

strReplace <- function(haystack, needle, replace,...) {
  gsub(needle, replace, haystack,...)}

forStrInStrs  <- function(haystack, needles, repstr,...) {
    for (needle in needles) { 
      haystack = ifelse(inStr(haystack, needle), repstr, haystack) }
  return(haystack)
}


dollar.to.numeric <- function(x) {  as.numeric(gsub("\\$", "", x))  }

numeric.to.dollar <- function(x) {  as.character(paste0("$",x))  }

