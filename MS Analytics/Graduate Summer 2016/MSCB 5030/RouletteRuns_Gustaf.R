
## A useful tip: If you plan on performing a set of calculations over and over again, it is sometimes
##               useful to define a funcion that will do these for you, and allow you to set options.
##               That is what I do here.


## Some simple examples of functions:
# A function is a routine that can take arguments and return data, both optional.

f1 <- function() {
  print("This is a useless function")
}
# You run a function by putting a parenthesis behind (arguments goes inside)
f1()


# A trivial example with both input and output
addition <- function(a,b) {
  return(a+b)
}
addition(5,6)

# Inline functions (advanced) work a little bit differently. A built in example is %in%.
# Let's create another one: 
"%P%" <- function(a,b) { # notice how the function name is quoted
  return(paste(a,b, sep = ""))
}
"This is a nice convinience function for pasting strings of text," %P% " like this."


# Sometimes, you will want to return multiple things
f2 <- function(a, b) {
  aa = a^2
  ba = b^2
  ab = a^(1/2)
  bb = b^(1/2)
  return(list(square = c(aa, ba), root = c(ab, bb)))
}
# You can always save the output of a function
output = f2(9,16)

 

## Loops
# The simplest loop is a for-loop, here's an easy one:
for(i in 1:4) {
  print(i)
}


r = 0
for(i in 1:10) {
  r = r + i
}
r

## ----------------------- ##
## Code to simulate the    ##
## probability of getting  ## 
## a run of five reds in   ##
## a row in 100 roulette   ##
## spins                   ##
## ----------------------- ##

count_the_runs = function(Num_spins, type = "equal"){
  spin = c("red", "green", "black")
  
  spinsample = sample(spin, size = Num_spins, replace = TRUE, prob = c(18/37, 1/37, 18/37))
  isitred = (spinsample == "red")
  
  ## Loop through and count how long each run is
  
  ## Logic: the loop goes through all the spins, if a spin is red, it count how long the
  ##        run of reds is. When the run ends, it is stored in the 'runs' variable.
  ##        After that, the loop looks for the next red not in the prior run and does the same thing.
  
  runs = c()
  k = 0
  for(i in 1:Num_spins) {
    # how long is the current run
    # count forward:
    if(isitred[i] == TRUE & i > k) {
      run = 1
      run.going = TRUE
      k = i + 1 # k is index of next spin
      while(run.going ) {
        if( k <= Num_spins & isitred[k] == TRUE) {
          run = run+1
          k = k+1
        } else {
          runs = c(runs, run)
          run.going = FALSE
        }
        
      }
    } 
  }
  
  
  if(type %in% "equal"){
    counted_runs = sum(runs == 5)
  }
  if(type %in% "greater"){
    counted_runs = sum(runs >= 5)
  }
  return(counted_runs)
}


Num_reps = 10000                 ## Again, preallocating, will store outcomes from count_the_runs
out_store = rep(NA, Num_reps)    ## here

for(j in 1:Num_reps){
  out_store[j] = count_the_runs(Num_spins = 100)  ## Stores the number of runs in the jth element
}

table(out_store)/Num_reps



## The R-way of doing the same thing (loops are evil)
count_the_runs.R = function(Num_spins, type = "equal"){
  spinsample = sample(spin, size = Num_spins, replace = TRUE, prob = c(18/37, 1/37, 18/37))
  
  # Get run length using the run length function
  run.lengths = rle(spinsample)
  idx = run.lengths$values == "red"
  red.runs = run.lengths$lengths[idx]
  
  if(type %in% "equal"){
    counted_runs = sum(red.runs == 5)
  }
  if(type %in% "greater"){
    counted_runs = sum(red.runs >= 5)
  }
  return(counted_runs)
}


Num_reps = 10000                 ## Again, preallocating, will store outcomes from count_the_runs
out_store = rep(NA, Num_reps)    ## here


for(j in 1:Num_reps){
  out_store[j] = count_the_runs.R(Num_spins = 100)  ## Stores the number of runs in the jth element
}

table(out_store)/Num_reps
barplot(table(out_store)/Num_reps)







## Can also look at greater than or equal to 5 reds in a row.
## 
out_store = rep(NA, Num_reps)
for(j in 1:Num_reps){
  out_store[j] = count_the_runs(Num_spins = 100, type = "greater")
}

table(out_store)/Num_reps

## Look at 20 spins  (unlikely)
## 
out_store = rep(NA, Num_reps)
for(j in 1:Num_reps){
  out_store[j] = count_the_runs(Num_spins = 20, type = "greater")
}

table(out_store)/Num_reps


## Look at 200 spins  (very likely)
## 
out_store = rep(NA, Num_reps)
for(j in 1:Num_reps){
  out_store[j] = count_the_runs(Num_spins = 200, type = "greater")
}

table(out_store)/Num_reps