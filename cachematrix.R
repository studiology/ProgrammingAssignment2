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

