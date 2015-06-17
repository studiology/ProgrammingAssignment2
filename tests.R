###############################
## Matrix solution test functions sourced from Steven Okamoto in this post:
## https://class.coursera.org/rprog-015/forum/thread?thread_id=442#post-2918

## Creates a square matrix whose elements are independently
## sampled from a uniform distribution.
##
## Returns the randomly-generated matrix.
##
## n    The number of rows and columns
## rng  The range of values to sample each element from
rand.mtx <- function(n, rng = 1:10) {
        matrix(sample(rng, n^2, replace=TRUE), c(n, n))
}

# Checks if an (uncached) matrix is the identity matrix,
# up to a noise tolerance.
#
# Returns TRUE if the matrix is the identity matrix, and
# FALSE otherwise.
#
# Arguments:
# x          The matrix to test.
# tolerance  The maximum amount of allowable noise on each
#            element.
is.identity <- function(x, tolerance = 0.001) {
        if (class(x) != "matrix") {
                # it's not a matrix
                return(FALSE);
        }
        n <- nrow(x)
        if (n != ncol(x)) {
                # it's not square
                return(FALSE);
        }
        return(all(abs(x - diag(n) < tolerance)))
}

# Tests if two (cached or uncached) matrices are
# inverses, up to a tolerance.  This is determined
# by multiplying the matrices and checking if the
# result is the identity matrix.
#
# Returns TRUE if the matrices are inverses, and
# FALSE otherwise.
#
# Arguments:
# x1         The first matrix.
# x2         The second matrix.
# tolerance  The tolerance for testing identity.
are.inv <- function(x1, x2, tolerance=0.001) {
        if (class(x1) == "matrix") {
                if (class(x2) == "matrix") {
                        return(is.identity(x1 %*% x2))
                } else {
                        return(is.identity(x1 %*% x2$get()))
                }
        } else {
                if (class(x2) == "matrix") {
                        return(is.identity(x1$get() %*% x2))
                } else {
                        return(is.identity(x1$get() %*% x2$get()))
                }
        }
}

# Tests the matrix caching and inversion on multiple randomly-generated
# matrices.
# Each test result is stored in a list with the following elements:
#    matrix    The randomly-generated matrix.
#    cache     The cached matrix as created by makeCacheMatrix(matrix)
#    inverse   The (uncached) inverse of the cached matrix, as
#              calculated by cacheSolve(cache)
#    correct   Logical indicator whether matrix and inverse are really
#              inverses.
#    ident     Logical indicator whether matrix generated the same inverse
#              on a subsequent call to cacheSolve().
#
# Returns a list of test results.
#
# Arguments:
# n    A vector of the number of rows/columns in each test.
# rng  The range of values for each element in the randomly-generated
#      matrices.
run.test <- function(n, rng=1:10) {
        results <- list()
        for (i in n) {
                mtx <- rand.mtx(i, rng)
                cache.mtx <- makeCacheMatrix(mtx)
                inv.mtx <- cacheSolve(cache.mtx)
                inv.mtx2 <- cacheSolve(cache.mtx)
                correct <- are.inv(inv.mtx, cache.mtx)
                ident <- identical(inv.mtx, inv.mtx2)
                results <- c(results, list(list(matrix=mtx, cache=cache.mtx,
                                                inverse=inv.mtx, correct=correct,
                                                ident=ident)))
        }
        results
}

###############################################################################

## These functions are used for testing makeCacheMatrix and cacheSolve only
## hilbert : generate a solveable n*n matrix (only useful up to 11x11)
## checkit : verifies whether two matrices are inverse of eachother
## multicheck : creates a new n*n matrix, then prints, caches, solves, prints the solution, then verifies it

hilbert <- function(n=4L) {
        ## generates an n*n (4x4 by default) Hilbert matrix

        ## Note that 12x12 matrices (and larger) are not solveable on most
        ## machines because the fractions get too small for the computer to be
        ## able to tell them apart: see .Machine$double.eps
        i <- 1:n
        1 / outer(i - 1, i, "+")
}


checkit <- function(mat1, mat2) {
        ## multiply mat1 by mat2 and compare with identity matrix of the same size
        ## returns TRUE if mat2 is the inverse of mat1
        ## mat1 can be a matrix or a list created by makeCacheMatrix

        ## If mat1 is a list, then mat2 is optional and mat1$get() and
        ## mat1$getsol() will be used for verification
        if (class(mat1) == "list") {
                m1 <- mat1$get()
                m2 <- mat1$getsol()
        } else {
                m1 <- mat1
                m2 <- mat2
        }
        n <- dim(m1)[1]
        ident <- diag(n)
        identical(round(m1 %*% m2),ident)
}

testCacheSolve <- function(n) {
        a <- hilbert(n)
        #print(round(a,2))

        cm <- makeCacheMatrix(a)
        cs <- cacheSolve(cm)

        #print(round(cs,2))
        ident <- diag(dim(a)[1])
        is_inverse <- checkit(a,cs)
        list(size = n, matrix1 = a, inverse = cs, identity = ident, is_inverse = is_inverse)
}
