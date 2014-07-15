## these function calculate the inverse of a matrix and store the solution
## so that it can be extracted from memory later if needed

## this function returns a list of functions that get and set the matrix and
## get and set the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(solve) m <<- solve
	getinv <- function() m
	list(set = set, get = get,
	setinv = setinv,
	getinv = getinv)
}

## this function returns the inverse matrix, either through calculation or
## if available from memory

cacheSolve <- function(x=matrix(), ...) {
	matrix <- x$get()

	## stops eveluation if matrix is not invertible
	if(det(matrix) == 0){
		stop("this matrix is not invertible")
	}

	## if variable m (the inverse matrix) is already cached it will
	## get the cached data, otherwise m will be calculated and then
	## cached
	m <- x$getinv()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	matrix <- x$get()
	m <- solve(matrix, ...)
	x$setinv(m)
	m
}

## the functions are used as follows:
## my_matrix <- makeCacheMatrix()
## my_matrix$set("Your matrix here")
## cacheSolve(my_matrix)
