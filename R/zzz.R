##
##  z z z . R
##


# Define 'ans' as in Matlab
# But: "Package namespaces are locked when loaded!"
# makeActiveBinding("ans", function() .Last.value, .GlobalEnv)

.pracmaEnv <- new.env()
assign("elapsedTime", 0, envir = .pracmaEnv)

.onLoad <- function(libname, pkgname) {
    # require(some_packages)

    # Load dynamic libraries
    # library.dynam(pkg, pkg, lib)

    environment(.pracmaEnv) <- asNamespace("pracma")

    # packageStartupMessage("Package 'pracma 1.0.5' loaded.")
}