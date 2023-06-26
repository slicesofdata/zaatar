#graphs
histy <- function(x,...) {
  hist(x, main = paste0( "M = ", round(mean(x, na.rm = T),1),
                         " SD = ",round(sd(x, na.rm = T),2),
                         "\nn = ",length(x),
                         "\nmin = ",round(min(x, na.rm = T),2),
                         " max = ", round(max(x, na.rm = T),2)), 
                         ...)
}

my.panel <- function(..., box.ratio) {
 panel.violin(..., col = 'grey', varwidth = FALSE, box.ratio = box.ratio)
 panel.bwplot(..., col = 'grey', cex = 0.9, pch = '|', fill = 'cyan', box.ratio = .25)
}

flattenCorrMatrix <- function(cormat, pmat) {  # borrowed from:  
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor = (cormat)[ut],
    p = pmat[ut]
    )
}