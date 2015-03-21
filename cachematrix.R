## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object
## that can cache its inverse.  Execution Scripts are at the bottom
## used the code from the Readme.md file and changed to matrix special types
## Also added the solver function into the code to do the inversion of 
## valid square matrixes passed to the makeCacheMatrix function


        makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                ## sets the value of the matrix(x)
                set <-  function(y){
                        ## This sets the Global Varaibles using the <<- operator
                        x  <<- y
                        m <<- NULL
                }
                ##retrieve the function value of matrix(x)
                get <- function() x
                ##Sets the value of solve function
                setmatrix <- function(solve) m <<- solve
                ## retrieves the value of solve
                getmatrix <- function() m
                list(set = set, get = get
                     ,setmatrix = setmatrix
                     ,getmatrix = getmatrix)
        }



## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
## Note:  If the value passed to the objects is not a square matrix 
## you should recieve an appropriate system error.  This is expected operation...


        cacheSolve <- function(x = matrix(), ...) {
                ## fills m with the list (x) value of getmatrix in cache.
                ## if = NULL 
                ## it runs function matrix(x) from console
                ## otherwise it retrieves cached data
                m <- x$getmatrix()
                if(!is.null(m)){
                        message("getting cached data")
                        return(m)
                }
                ## fills data passed from console.
                data <- x$get()
                ## using the first position of solve for a square matrix inversion as b is empty
                m <- solve(data, ...)
                ##sets value of list(x) value of setmatrix with results of solver function.
                x$setmatrix(m)
                m
        }
        
        
        ##sample set inversion should be -5, 4, 4.5, -3.5        
        ## a <- matrix(c(7,8,9,10), 2)
        ## b <- makeCacheMatrix(a)
        ## cacheSolve(b)
        

        
        ## creating a regular normal distribution matrix 100x100
        ## set.seed(125)
        ## a <- matrix(rnorm(100), 10,10)
        ## b <- makeCacheMatrix(a)
        ## cacheSolve(b)
        
        ##A group took a trip on a bus, at $3 per child and $3.20 per adult for a total of $118.40. 
        ##They took the train back at $3.50 per child and $3.60 per adult for a total of $135.20.
        ##How many children, and how many adults?
        ## a <- matrix(c(3, 3.2, 3.5, 3.6), 2,2)
        ## b <- makeCacheMatrix(a)
        ## cacheSolve(b)
        
        ## solver equation (NOTE: OUT OF SCOPE FOR ASSIGNMENT)
        ## inverse category 1 "Children" = (-9,8)
        ## (118.4 x -9 + 135.2 x 8) = 16 Children
        
        ## inverse Category 2 "Adults" = (8.75, -7.50)
        ## (118.4 x 8.75 + 135.2 x -7.50) = 22 Adults
        

