## Write the following functions:
      
      ## 1. makeCacheMatrix: This function creates a special "matrix" object
      ## that can cache its inverse.

      ## Put comments here that give an overall description of what your
      ## functions do

      ## Write a short comment describing this function

      #########################
      ## ASSIGNMENT 2 ANSWER ##
      #########################
      
      ## We'll need 2 functions to cache the inverse of a matrix:
      
      ##############################################################
      ## 1) makeCacheMatrix, which lists the following functions: ##
      ##############################################################
      
            ## a. set the values of the matrix as 'set'
            ## b. get the values of the matrix as 'get'
            ## c. set the values of inverse of the matrix as 'setinverse'
            ## d. get the values of inverse of the matrix as 'getinverse'

      ##########################################################
      ## 2) cacheSolve, which returns the inverse of a matrix ##
      ##########################################################
      
            ## a. set 'i' with 'getinverse', if previously computed
            ## b. return 'i', if previously set
            ## c. compute the inverse 'i', if not previously set
            ## d. set the matrix inverse values 'i' in a cache
            ## e. return newly calculated 'i'
            
            #################################################
            ## 1) Script for 1st funtion 'makeCacheMatrix' ##
            #################################################
      
            makeCacheMatrix <- function(x = matrix()) {
      
            # (a. 'set' the value of the matrix)
                  
                  # (delete any existing value for 'i' (inverse))
                       
                  i<- NULL
      
                  # (assign vector 'set' with a defined function of y) 
                  
                  set <- function(y) {
      
                        # (assign 'y' value to 'x' and after the function is 
                        # called, sets 'i' as NULL, from any environment)
                        
                        x <<- y
                        i <<- NULL
      
                  }
      
             # (b. "get" the value of the matrix)
             
                  # (assign vector 'get' with the x local environment within
                  # the user defined matrix (x) function)
      
                  get <- function() x
      
            # (c. set the inverse of the matrix)
      
                  # (clear any previous value of i, assigns vector 'setinverse'
                  # with user-defined inverse function of the matrix within the
                  # i local environment)
                  
                  setinverse <- function(solve) i <<- solve
      
            # (d. get the inverse of the matrix)
            
                  # (assign vector 'getinverse' with inverse of the matrix
                  # through the user-defined function in the i environment)
                  
                  getinverse <- function() i
            
                  # (create list of user-defined functions)
      
                  list(set = set, get = get, setinverse = setinverse,
                  getinverse = getinverse)

            }

      ##################################################
      ## 2) Script for 2nd function 'makeCacheMatrix' ##
      ##################################################
            
            cacheSolve <- function(x, ...) {
      
                  # (set 'i' with 'getinverse', if previously computed...
                  # assign vector 'i' the currently calculated inverse of
                  # the matrix)
                  
                  i <- x$getinverse()
      
                  # (return 'i', if previously set..             
                  # ...if 'i' isn't null, and therefore previously calculated,
                  # then create message informing user and...)
      
                  if(!is.null(i)) {
                        message("Cached data found...
                                getting inverse matrix data.")
                        
                        # (...return the inverse values of the matrix )
                        
                        return(i)
                  }
                  
                  # (else, compute the inverse 'i', if not previously set...
                  # ...if 'i' is null, create a message informing
                  # the user and...)
                  
                  message("No cached data found...
                          calculating inverse matrix...")
                  
                  # (set the matrix inverse values 'i' in a cache...
                  # ...retrieve matrix data and calculate the matrix inverse)
                  
                  data <- x$get()
                  i <- solve(data, ...)
      
                  # (set the calculated inverse of the matrix, 
                  # communicate completion to user)
      
                  x$setinverse(i)
                  message("...done.")
                  
                  # (# return newly calculated mtrix inverse values'i')                  
                  
                  return(i)
                                    
            }
            
# Sample Run

      # create matrix, run first function without the cache
            
      x = rbind(c(1.5, -2.0), c(-2.0, 1.5))
      m = makeCacheMatrix(x)
      m$get()
            
      # With cache, (second function)
            
      cacheSolve(m)

      # With cache, (second function, second run)
      
      cacheSolve(m)