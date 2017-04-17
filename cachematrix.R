## Use 'makeCacheMatrix' to create a matrix and assign to a variable.
## Use 'cacheSolve' method to solve for inverse of the matrix variable created using 'makeCacheMatrix' method. Once this method is run,
## it stores the inverse in memory and if requested again, the inverse is returned from memroy instead of being computed agan.

## This function lets user assign a matrix to a variable. Once this function is called,
## it exposes 4 methods which do the following:
## 1. set: Sets the value for the matrix.
## 2. get: Gets the existing matrix.
## 3. setMatrixInverse: Sets the inverse of the matrix.
## 4. getMatrixInverse: Gets the inverse of the matrix. (from cache if it already exists)
makeCacheMatrix <- function(x = matrix()) {
        
        matrixInverse <- NULL
        
        # Following line of code creates a method for this object
        # which sets the matrix data
        set <- function(y) {
                x <<- y
                matrixInverse <<- NULL
        }
        
        # Following line of code creates a method for this object
        # which retreives/gets the matrix data
        get <- function() x
        
        # Following line of code creates a method for this object
        # which sets the inverse of the matrix
        setMatrixInverse <- function(inverse) matrixInverse <<- inverse
        
        # Following line of code creates a method for this object
        # which gets the inverse of the matrix
        getMatrixInverse <- function() matrixInverse
        
        # Create a list of all functions available for this object
        list(set = set, get = get, setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
}


## This function checks if the inverse has already been caculated... if it is not caculated, it calculates the inverse and caches it.
cacheSolve <- function(x, ...) {

        # Get the inverse of matrix from memory
        inverse <- x$getMatrixInverse()
        
        # If the inverse is not null, let the user know that the inverse existed in cache and return it.
        if(!is.null(inverse)) {
                message("Inverse already exists in memory.")
                return(inverse)
        }
        
        # Get the matrix using 'get' method which was created in the other function
        matrix <- x$get()
        
        # Solve for inverse of the matrix
        inverse <- solve(matrix)
        
        # Save the inverse in memory using 'setMatrixInverse' method which was created in other function
        x$setMatrixInverse(inverse)
        
        # Return inverse of the matrix
        inverse
}

