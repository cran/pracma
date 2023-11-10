##
##  f r a c t a l c u r v e . R  Fractal curves
##


fractalcurve <- function(n, which = c("hilbert", "sierpinski", "snowflake",
                "dragon", "triangle", "arrowhead", "flowsnake", "molecule"))
{
     curve <- match.arg(which)
     if (curve == "hilbert") {  # Hilbert curve
         a <- 1 + 1i
         b <- 1 - 1i
         z <- 0
         for (k in 1:n) {
             w <- 1i * Conj(z)
             z <- c(w-a, z-b, z+a, b-w) / 2.0
         }

     } else if (curve == "sierpinski") {  # Sierpinski Cross curve
         a <- 1 + 1i
         b <- 1 - 1i
         c <- 2 - sqrt(2)
         z <- c
         for (k in 1:n) {
             w <- 1i * z
             z <- c(z+b, w+b, a-w, z+a) / 2.0
         }
         z <- c(z, 1i*z, -z, -1i*z, z[1])

     } else if (curve == "snowflake") {  # Koch snowflake
         a <- 1/2 + sqrt(-3+0i)/6; b <- 1 - a
         c <- 1/2 + sqrt(-3+0i)/2; d <- 1 - c
         z <- 1
         for (k in 1:n) {
             z <- Conj(z)
             z <- c(a*z, b*z+a)
         }
         z <- c(0, z, 1-c*z, 1-c-d*z)

     } else if (curve == "dragon") {  # Dragon curve
         a <- (1 + 1i)/2
         b <- (1 - 1i)/2
         c <- sqrt(1/2 + 0i)
         z <- c(1-c, c)
         for (k in 1:n) {
             w <- rev(z)  # z(end:-1:1);
             z <- c(a*z, 1-b*w)
         }

     } else if (curve == "triangle") {  # Sierpinski Triangle curve
         a <- (1 + sqrt(-3+0i))/2
         z <- c(0, 1)
         for (k in 1:n) {
             z <- c(z, z+a, z+1)/2
         }
         z <- c(z, a, 0)

     } else if (curve == "arrowhead") {  # Sierpinski Arrowhead curve
         a <- (1 + sqrt(-3+0i))/2
         b <- (1 - sqrt(-3+0i))/2
         z <- 0
         for (k in 1:n) {
             w <- Conj(z)
             z <- c(a*w, z+a, b*w+a+1)/2
         }
         z <- c(z, 1)

     } else if (curve == "flowsnake") {  # Gosper Flowsnake curve
         a <- (1 + sqrt(-3+0i))/2
         b <- (1 - sqrt(-3+0i))/2
         c <- c(1, a, -b, -1, -a, b)
         u = 0;
         for (k in 1:n) {
             v <- rev(u)
             u <- c(u, v+1, v+3, u+2, u, u, v-1)
         }
         u <- mod(u, 6)
         z <- cumsum(c[u+1])
         z <- c(0, z/7^(n/2))

     } else if (curve == "molecule") {  # Hexagon Molecule curve
         a <- (1 + sqrt(-3+0i))/2
         b <- (1 - sqrt(-3+0i))/2
         c <- c(1, a, -b, -1, -a, b)
         u <- 0
         for (k in 1:n) {
             u <- c(u+1, -u, u-1)
         }
         u <- c(u, 1-u, 2+u, 3-u, 4+u, 5-u)
         u <- mod(u, 6)
         z <- cumsum(c[u+1])
         z <- c(0, z/2^n)

     } else {
         stop("Unknown fractal curve name ...")
     }
     return(list(x = Re(z), y = Im(z)))
}
