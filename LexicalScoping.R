## This following function takes a square invertible matrix
## and return a list of functions to:
## 1.- Set the matrix.
## 2.- Get the matrix.
## 3.- Set the inverse of matrix.
## 4.- Get the inverse of matrix.
## Finally, this is used as input in cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## Set the matrix
    set <- function(y){ 
        x <<- y
        m <<- NULL
    }
    ## Get the matrix
    get <- function() x
    ## Set the inverse of matrix.
    setinverse <- function(solve) m <<- solve
    ## Get the inverse of matrix.
    getinverse <- function() m
    ## Return the list of functions.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## In cacheSolve function, we take the matrix and
## calculate the inverse.
## Finally we return it.
cacheSolve <- function(x, ...) {
    ## Get 
    m <- x$getinverse()
    if(!is.null(m)) {
        ## If the value already exists, returns m.
        message("getting cached data")
        return(m)
    }
    ## Else
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    return(m)
}

## Running Example
# > matriz <- matrix(runif(9,1,100),3,3)

#  Show "matriz"
# > matriz

# [,1]      [,2]     [,3]
# [1,] 90.15773 26.488523 72.41762
# [2,] 60.19124  9.766337 91.25337
# [3,] 96.55614 47.140896 67.54101

# Generating the cache matrix
# > matrixExample <- makeCacheMatrix(matriz)

# Finally calculate or retrieve the value of
# Inverted matrix using cacheSolve:

# > cacheSolve(matrixExample)

# [,1]        [,2]        [,3]
# [1,] -0.162284059 -0.12840965  0.22513611
# [2,]  0.194560788 -0.05779459 -0.08093354
# [3,]  0.004915707  0.20579781 -0.04886617


