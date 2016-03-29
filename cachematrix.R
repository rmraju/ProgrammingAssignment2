#
# Coursera R progtramming Assignment 2
#
# Following functions call can be used to test functions
# 
#1 makeCacheMatrix function 
#This function will created a special "matrix" object that can cache its  #inverse and rturns a #list of get and set functions
#
#2 cacheSolve function 
# cacheSolve function will set and return value of inverse matrix
#
#3 testProgrammingAssignment2 function
#This function will create a special matrix and then cache inversion of the 
#matrix and check to see that check if it rerure  cached inversion
#
#4 Running assignment 
#At the console just excute testProgrammingAssignment2() to see results
#


#1
makeCacheMatrix <- function(m = matrix()) {
     
    inv <- NULL # local inverse of matrix
    
    #initalize global env parameters
    set <- function(y){
         inv <<- NULL # set invserse of matrix in GlobalEnv
         m <<- y  # set matrix in GlobalEnv 
    }
    
    get <- function() m  #get GlobalEnv matrix
    #print( paste("get=",  m, ";" , sep =  ""))
    
    
    #set and get for global env parameters for matrix m
    setInverse <- function(inverse) inv <<- solve 
    getInverse <- function() inv
    
    #print( paste("getInverse=",  inv, ";", sep =  ""))
    
    #list of functions is retured to set and get values
    list( get = get, set = set, setInverse = setInverse, getInverse = getInverse)
}

#2
cacheSolve <- function(m, ...) {

    #get inversion of matrix

   inv <- m$getInverse()
   if(!is.null(inv)) {
       message("getting cached inverse of special matrix")
       inv
   }else {
       message("calculating and caching inverse of special matrix")
   }
   
   #local data
   m_data <- m$get()

   #local inverse
   inv <- solve(m_data, ...)
   #print( paste("inverse of matrix =",  inv , ";" , sep =  ""))
   
   #set inverse to global environment parameter matrix
   m$setInverse(inv)
   
   #print(inv)
 
   #return inverse
   inv
   
}

#3 
testProgrammingAssignment2 <- function () {

    # create a local invertible matrix
    temp_matrix <- matrix( c(1,0,5, 2,1,6, 3,4,0), nrow = 3, ncol = 3)

    #call makeCacheMatrox
    my_matrix <- makeCacheMatrix(temp_matrix)
    print("Special Matrix")
    print(my_matrix$get())

    #cache inverse
    test_cache1 <- cacheSolve(my_matrix)
    print(test_cache1)

    #return cached inversion
    test_cache2 <- cacheSolve(my_matrix)
    print(test_cache2)
    
    #see if inverse_of_inverse gives original matrix
    inverse_of_inverse <- solve(test_cache2)
    print("See if inverse_of_inverse Matrix is same as original")
    print(inverse_of_inverse)
}


