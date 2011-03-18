##
##  s t d . R
##


# std(a, flag=0, dim=2)  # dim = 1: along cols; = 2: along rows
std <- function(x, flag=0) {
	n <- if (flag == 0) length(x) - 1 else length(x) 
	sqrt(sum((x-mean(x))*(x-mean(x)))/n)
}
