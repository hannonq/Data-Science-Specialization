## makeCacheMatrix(): This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


##creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function(){
        x    
    }
    setInverse <- function(inverse){
        i <<- inverse
    }
    getInverse <- function(){
        i
    }
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}

## computes  or retrieves the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting chached data")
        return(i)
    }
    message("calculating new inverse matrix")
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i ## Return a matrix that is the inverse of 'x'
}

##Test code. Uncomment to run
# x <- matrix(c(-7,-6,-12,5,5,7,1,0,4), nrow = 3, ncol = 3, byrow = TRUE)
# c_matrix <- makeCacheMatrix(x)
# print(cacheSolve(c_matrix))
# print(cacheSolve(c_matrix))
