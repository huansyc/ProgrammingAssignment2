## The two functions compute and cache the inverse of the matrix.

## Create a 'Vector', which is really a list containing the functions to
## 1. set the value of the matrix (set)
## 2. get the value of the matrix (get)
## 3. set the value of the inverse (settrans)
## 4. get the value of the inverse (gettrans)

makeCacheMatrix <- function(x = matrix()) {
        t <- NULL
        set <- function(y) {
                x <<- y
                t <<- NULL
        }
        get <- function() x
        settrans <- function(trans) t <<- trans
        gettrans <- function() t
        list(set = set, 
             get = get,
             settrans = settrans,
             gettrans = gettrans)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        t <- x$gettrans()
        if(!is.null(t)) {
                message("getting cached data")
                return(t)
        }
        data <- x$get()
        t <- t(data)
        x$settrans(t)
        t
}
