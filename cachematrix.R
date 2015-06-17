## Finds the inverse of a given matrix and caches the result. Subsequent calls
## will retrieve the cached result if present

## returns a list of functions which operate on a given matrix.
# get        returns the original matrix
# set        sets the original matrix and clears the cached inverse
# setInverse solves the matrix and caches the inverse matrix
# getInverse returns the cached inverse matrix

makeCacheMatrix <- function(mat = matrix()) {
        inv <- NULL
        set <- function(y) {
                mat <<- y
                inv <<- NULL
        }
        get <- function() mat
        setsol <- function(soln) inv <<- soln
        getsol <- function() inv
        list(set = set, get = get,
             setsol = setsol,
             getsol = getsol)

}


##

cacheSolve <- function(fList, mat, ...) {
        ## Return a matrix that is the inverse of the one stored when creating
        ## the function list in makeCacheMatrix()

        ## If the optional 'mat' argument is passed, it will be compared to the
        ## one cached in makeCacheMatrix(), and if it is different, the new
        ## matrix will be cached, a new solution will be calculated and cached,
        ## and finally the solution will be returned
        if (!missing(mat)) {
                if (!identical(mat , fList$get())) {
                        message("this is a different matrix, resetting cache")
                        fList$set(mat)
                }
        }
        sol <- fList$getsol()
        if(!is.null(sol)) {
                message("getting cached solution")
                return(sol)
        }
        cMat <- fList$get()
        sol <- solve(cMat, ...)
        fList$setsol(sol)
        sol
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
