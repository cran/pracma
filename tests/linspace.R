##
##  l i n s p a c e . R
##

linspace <- pracma::linspace
logspace <- pracma::logspace
logseq   <- pracma::logseq

identical(linspace(1, 100), as.numeric(1:100))
identical(linspace(0, 25), c(0, 6.25, 12.50, 18.75, 25))
identical(linspace(1, 25, 1.5), 25)
identical(all.equal(linspace(1, pi, n=5),
                    c(10.0000, 7.4866, 5.6050, 4.1963, 3.1416),
                    tolerance=0.0001),
          TRUE)
