##
##  s t r i n g s . R
##


strcat <- function(s1, s2 = NULL, collapse = "") {
	stopifnot(is.character(collapse))
	if (!is.vector(s1, mode = "character"))
	    stop("Argument 's1' must be a character vector.")

	if (is.null(s2)) {
	    paste(s1, collapse=collapse)
	} else {
	    if (!is.vector(s2, mode = "character"))
	        stop("Argument 's2' must be a character vector.")
	    else
	        paste(rep(s1, each = length(s2)), s2, sep = collapse)
    }
}

strcmp <- function(s1, s2) {
	if (!is.vector(s1, mode="character") || !is.vector(s1, mode="character"))
	    stop("Arguments 's1' and 's2' must be character vectors.")

    if (length(s1) == length(s2))
        all(s1 == s2)
    else
	    FALSE
}

strcmpi <- function(s1, s2) {
	if (!is.vector(s1, mode="character") || !is.vector(s1, mode="character"))
	    stop("Arguments 's1' and 's2' must be character vectors.")

    strcmp(tolower(s1), tolower(s2))
}
