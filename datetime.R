months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

month.abb <- function(x) {
	# converts a vector of numeric values for months (e.g., 1- 12) to an abbreviated vector
	months <- c("Jan","Feb","Mar", "Apr","May","Jun", "Jul","Aug","Sep", "Oct","Nov","Dec")
	return (months[x]) }


diff.days <- function(start, end) {
    x = as.numeric(as.Date(start) - as.Date(end))
    #message(paste0("Time difference of ",x, " days"))
    return(x)
}

# some simple date format functions
dates <- function(d = 3) {
        #use Sys.Date to obtain the current date year, month, or day
        a = stringr::str_split_fixed(Sys.Date(),"-",3)[,1]
        b = paste0(stringr::str_split_fixed(Sys.Date(),"-",3)[,1],"-",
                 stringr::str_split_fixed(Sys.Date(),"-",3)[,2])
        c =  as.character(Sys.Date())
        x <- ifelse(d == 1, a, ifelse(d == 2, b, c)); return(x)
}

yyyy = function(
    # make a list of years from x to y repeating how many. Default
    # returns a character vector of each year repeated 12 times
    d = 3,
    from = as.numeric(stringr::str_split_fixed(Sys.Date(),"-",3)[,1]),
    to = 2000, times = 1, each = 12) {
    yyyy <- as.character(rep(seq(from=from, to=to),
                             times=times, length.out=NA, each=each))
    mm <- rep(seq(from=1,to=12),times=length(yyyy)/12,length.out=NA,each=1)
    yyyy.mm <- paste0(yyyy,"-",ifelse(mm < 10,paste0("0",mm),mm))
    yyyy.mm.dd <- paste0(yyyy.mm,"-01")
    ifelse(d==1, return(yyyy), ifelse(d==2,return(yyyy.mm),
                 ifelse(d==3,return(yyyy.mm.dd), NA)))
}