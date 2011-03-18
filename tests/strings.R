##
##  s t r i n g s . R  tests
##

strcat <- pracma::strcat
strcmp <- pracma::strcmp
strcmpi <- pracma::strcmpi

strcmp(" empty", " empty")               # TRUE
!strcmp("empty ", "empty")               # FALSE
!strcmp("foobar", "barfoo")              # FALSE
!strcmp("string", "String")              # FALSE
!strcmp(c("yes", "no"), c("yes", "on"))  # FALSE
!strcmp(c("abc", "abc"), c("abc"))       # FALSE
strcmp(c("yes", "no"), c("yes", "no"))   # TRUE

strcmpi("string", "String")              # TRUE
strcmp(c("yes", "no"), c("Yes", "No"))   # TRUE
