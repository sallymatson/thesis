tensor_als <- function(X,Z,legs,N=100){
  '
  This function runs N iterations of the Alternating Least Squares algorithm, 
  for a single rank matrix. It assumes that the tensor has 3 dimensions.
  '
  a.it = legs$a
  b.it = legs$b
  c.it = legs$c
  for (j in 1:N){
    a.numerator = X*Z*outer(rep(1,length(a.it)),outer(b.it,c.it))
    a.denominator = Z*outer(rep(1,length(a.it)),outer(b.it*b.it, c.it*c.it))
    a.it = apply(a.numerator, 1, sum)/apply(a.denominator, 1, sum)
    
    b.numerator = X*Z*outer(a.it,outer(rep(1,length(b.it)),c.it))
    b.denominator = Z*outer(a.it*a.it,outer(rep(1,length(b.it)), c.it*c.it))
    b.it = apply(b.numerator, 2, sum)/apply(b.denominator, 2, sum)
    
    c.numerator = X*Z*outer(a.it,outer(b.it,rep(1,length(c.it))))
    c.denominator = Z*outer(a.it*a.it,outer(b.it*b.it,rep(1,length(c.it))))
    c.it = apply(c.numerator, 3, sum)/apply(c.denominator, 3, sum)
    }
  return(list(a = a.it, b = b.it,c = c.it))
}

initialize_als <- function(xch4, rank, seed=2020){
  '
  This function initializes all of the random iterators for Alternating 
  Least Squares for a three dimensional tensor, with any rank.
  '
  len_a = dim(xch4)[1]
  len_b = dim(xch4)[2]
  len_c = dim(xch4)[3]
  als_list = list()
  for (r in seq(1, rank)){
    set.seed(seed)
    a0 = rnorm(len_a)
    set.seed(seed)
    b0 = rnorm(len_b)
    set.seed(seed)
    c0 = rnorm(len_c)
    als_list[[paste("rank", r)]] = list(a = a0, b = b0, c = c0)
  }
  return(als_list)
}

run_als <- function(xch4, Z, rank, max.iterations=1000, seed=2020){
  
  # Initialize the a, b, and c values for each rank:
  als_list = initialize_als(xch4, rank, seed=seed)
  
  # Create the initial xch4 approximation based on just the FIRST legs.
  curr.xch4.approx = outer(outer(rep(0, 72), rep(0, 34)), rep(0, 144))

  error.current = sqrt(sum((xch4-curr.xch4.approx)^2 * Z)/ sum(!is.na(xch4)))
  errors.list = list()

  # This outer for loop represents one iteration of the ALS algorithm.
  # One iteration means going over each rank and adjusting the approximation.
  for (i in seq(1, max.iterations)){
    
    if (i == 1){
      for (r in seq(1, rank)){
        curr.legs = als_list[[paste("rank", r)]]
        to.approximate = (xch4 - curr.xch4.approx) * Z
        new.legs = tensor_als(to.approximate,Z,curr.legs,10)
        new.legs.approx = outer(new.legs$a, outer(new.legs$b, new.legs$c))
        curr.xch4.approx = curr.xch4.approx + new.legs.approx
        als_list[[paste("rank", r)]] = new.legs
      }
      
    }
    else {
      for (r in seq(1, rank)){
        curr.legs = als_list[[paste("rank", r)]]
        curr.legs.approx = outer(curr.legs$a, outer(curr.legs$b, curr.legs$c))
        
        # This matrix represents what the "target" approximation is for the 
        # current set of legs. Start with the whole matrix (xch4) and subtract
        # off the sum over all ranks; add back the current rank's approximation. 
        to.approximate = (xch4 - curr.xch4.approx + curr.legs.approx) * Z
        
        # Run the ALS and get the new approximation for this rank:
        new.legs = tensor_als(to.approximate,Z,curr.legs,1)
        new.legs.approx = outer(new.legs$a, outer(new.legs$b, new.legs$c))
        
        # Adjust the curr.xch4.approx to take into acocunt the new legs:
        curr.xch4.approx = curr.xch4.approx - curr.legs.approx + new.legs.approx
        
        # Finally, replace the old legs with the new ones
        als_list[[paste("rank", r)]] = new.legs
      }
    }
    
    # Update the error calculations and keep one past for the stopping condition:
    error.previous = error.current
    error.current = sum((xch4-curr.xch4.approx)^2 * Z)
    error.current = sqrt(error.current / sum(!is.na(xch4)))
    error.average = error.current

    # Every 10 iterations print an update:
    if (i %% 10 == 0){
      #print(paste("Iteration", i, "| Overall error:", error.current, "| Average error", error.average))
    }
    errors.list[i] = error.current
    
    # Check the stopping condition:
    if (error.previous - error.current < 0.01 || i == max.iterations){
      #print(paste("ALS has stopped after", i, "iterations."))
      #print(paste("The previous error was", error.previous, "and the current error is", error.current, "."))
      #print(paste("The final average error is", error.average))
      break
    }
    
  }
  return(list(als_list = als_list, errors = errors.list, iterations = i))
}


calculate_approximation <- function(als_list){
  approximation = outer(outer(rep(0, 34), rep(0, 72)), rep(0, 144))
  for (rank in als_list){
    approximation = approximation + outer(rev(rank$b), outer(rank$a, rank$c))
  }
  return(approximation)
}


