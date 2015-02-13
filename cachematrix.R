## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object than can cache its inverse
## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated then the inverse 
## matrix is retrieved from cache

## Write a short comment describing this function
## makeCacheMatrix is a list object containing functions that
## store a matrix (x) and can cache its inverse (m)

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
			x <<- y
			m <<- NULL
	}
	get <- function() x
	setInverseMatrix <- function(solve) m <<- solve
	getInverseMatrix <- function() m
	list(set = set,
		 get = get,
		 setInverseMatrix = setInverseMatrix,
		 getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function
## cacheSolve will check the special matrix object for its cached invert, if it
## is not found, the inverted matrix will be computed using solve and stored to
## cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseMatrix()
        if(!is.null(m)) {
        	message("getting cached data")
        	return(m)
        }
        mat <- x$get()
        m <- solve(mat,...)
        x$setInverseMatrix(m)
        m
}
