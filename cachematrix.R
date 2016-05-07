## Cache the solve matrix to avoid heavy computing
## Should be a square matrix

## The first function makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
##
##    set the data of the matrix
##    get the data of the matrix
##    set the data of the solve
##    get the data of the solve

makeCacheMatrix <- function(m = matrix()) {
        solv <- NULL
        set <- function(mprim = matrix()) {
                m <<- mprim
                solv <<- NULL
        }
        get <- function() m
        setsolve <- function(solve) solv <<- solve
        getsolve <- function() solv
        list(set = set,
			 get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Return the solve matrix cache; Compute it if needeed

cacheSolve <- function(m, ...) {
        solv <- m$getsolve()
        if(!is.null(solv)) {
                message("getting cached data")
                return(solv)
        }
        data <- m$get()
        solv <- solve(data)
        m$setsolve(solv)
        solv
}
