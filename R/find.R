##
##  f i n d . R  Finds indices of nonzero elements
##


find <- function(v) 
    which( if (is.logical(v)) v else v != 0 )
