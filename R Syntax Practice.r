cars <- mtcars #create a dataset
#print(colnames(cars)) #print column names
#print(rownames(cars))
#print(str(cars)) # examine data type
#print(dim(cars)) # rows x cols
#print(mean(cars[["mpg"]])) # calculate mean of a column 
#print(sd(cars[["mpg"]])) # calculate std of a column
#print(summarise(cars, my_mean = mean(mpg)))  # Apply summarise function within package dplyr


"a function"
fahr_to_celsius <- function(temp) {
  celsius <- 5/9*(temp -32)
  return(celsius)
}
print(fahr_to_celsius(80))