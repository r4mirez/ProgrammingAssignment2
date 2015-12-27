## The following functions can be used to make a kind of
## matrix which is able to cache its inverse and avoid
## solving it again if this value is requested again.

## makeCacheMatrix receives a matrix and returns a list with
## several functions used to get its value, set it again,
## set the inverse and get it back

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (m) {
		x <<- m
		inv <- NULL
	}
	get <- function () x
	setinv <- function (i) inv <<- i
	getinv <- function () inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve receives a list obtained using the previous function and
## checks if the inverse has already been calculated. If it has not,
## cacheSolve does it, sets its value and returns the inverse matrix.
## If it has already been solved, it just report it and returns the
## inverse matrix.

cacheSolve <- function(x, ...) {
	inverse <- x$getinv()
	if (!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data)
	x$setinv(inverse)
	inverse
}