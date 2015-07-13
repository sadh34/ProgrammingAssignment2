## Functions below compute inverse of matrix (if matrix is square and invertible). Inverse of matrix is not compute if was computed before. In this case the inverse of matrix is cached.

## makeCacheMatrix function creates a special "matrix", which is really a list containing a functions. Here you can change a matrix to inverse.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
}
        get <- function() x
        setodw <- function(odw) m <<- odw
        getodw <- function() m
        list(set = set, get = get, setodw = setodw, getodw = getodw)
}


## The cachSolve function calculates the inverse of matrix created with the above function. However, it first checks to see if the inverse of matrix has already been calculated. If so, it gets the result from the cache and skips the computation. Otherwise, it calculates the inverse of matrix of the data and sets the value of the inverse matrix in the cache via the setodw function.

cacheSolve <- function(x, ...) {
        m <- x$getodw()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setodw(m)
        m
        }


b <- makeCacheMatrix(matrix(c(3,5,6,7),nrow = 2, ncol = 2, byrow=TRUE))

cacheSolve(b)
cacheSolve(b)

b <- makeCacheMatrix(matrix(c(3,5,6,3,4,9,6,9,0), nrow = 3, ncol = 3, byrow=TRUE))

cacheSolve(b)
cacheSolve(b)


d <- matrix(runif(2000*2000, min = 1, max = 1000), nrow = 2000, ncol= 2000)

b <- makeCacheMatrix(d)

cacheSolve(b)
cacheSolve(b)
