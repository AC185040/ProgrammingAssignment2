## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
    invrs <- NULL
    set <- function(y)
    {
        x   <<- y
        invrs <<- NULL
    }
    get <- function() x
    setinvers <- function(inverse) invrs <<- inverse
    getinvers <- function() invrs
    list (set = set, get = get, setinvers = setinvers, getinvers = getinvers)
}    


##cacheSolve: This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...)
{
    inv <- x$getinvers()
    if (!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinvers(inv)
    inv
}


## Execution:
x = rbind(c(2, -3/5), c(-3/5, 2))
> m = makeCacheMatrix(x)
> m$get()
[,1] [,2]
[1,]  2.0 -0.6
[2,] -0.6  2.0

## No cache in the first run

 > cacheSolve(m)
           [,1]      [,2]
 [1,] 1.0666667 0.2666667
 [2,] 0.2666667 1.0666667

cacheSolve(m)
[,1]      [,2]
[1,] 0.5494505 0.1648352
[2,] 0.1648352 0.5494505

# Retrieved Cache data

> cacheSolve(m)
getting cached data
[,1]      [,2]
[1,] 0.5494505 0.1648352
[2,] 0.1648352 0.5494505
