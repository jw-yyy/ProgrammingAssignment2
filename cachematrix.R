## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to 
## 1. set the value of a matrix 
## 2. get the value of a matrix
## 3. set the inverse of a matrix
## 4. get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(m) {
  	            x <<- m
  	            i <<- NULL
        }
        get <- function() x
        getinverse <- function() i
        setinverse <- function(inverse) i <<- inverse
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with the above function 
## However, it first checks to see if the mean has already been calculated. If so, it 
## [getinverse]s from the cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the inverse of the matrix in the cache via the setinverse function  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse of the matrix")
                return(i)
        }
        m <- x$get()
        inverse <- solve(m)
        x$setinverse(inverse)
        inverse
}
