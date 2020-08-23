## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())
{
	m <- NULL
	set <- function(y)
	{
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(i) m <<- solve(x)
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...)
{
	m <- x$getInverse()
	if(!is.null(m))
	{
		return(m)
	}
	m <- solve(x$get())
	x$setInverse(m)
	m
}
