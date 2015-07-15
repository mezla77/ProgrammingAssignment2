## Functions to allow the programmer to avoid the time-consuming
## re-calculation of the inverse of a matrix

## Return a list of functions to get, set, & cache a matrix
## and it's inverse.
makeCacheMatrix <- function(x = matrix())
{
    mInverse <- NULL
    
    set <- function(y)
    {
            x <<- y
            mInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(aInverse) mInverse <<- aInverse
    getInverse <- function() mInverse

    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Returns a matrix that is the inverse of 'x', using the stored
## result if it exists, otherwise storing the new result
cacheSolve <- function(x, ...)
{
    pInverse <- x$getInverse()
    if(!is.null(pInverse))
    {
        message("getting cached data")
        return(pInverse)
    }
    
    data <- x$get()
    pInverse <- solve(data)
    x$setInverse(pInverse)
    pInverse
}
