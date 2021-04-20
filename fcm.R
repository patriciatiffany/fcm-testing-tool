
fcm.run <- function (mx, rel_type_mx, rel_group_mx, cn, iter, 
                     set.concepts=NULL, set.values=NULL, 
                     infer_type = "sigmoid-exp",
                     init = 1, k = 0, h = 0, lambda = 1, plot=FALSE) 
  #' @param mx m x m Data frame which stores the weights assigned to the pairs of concepts. The weights are usually normalized to the interval [0,1 ] or [-1, +1].
  #' @param rel_type_mx m x m Data frame which stores the type of relationships: "AND", "OR"
  #' @param rel_group_mx m x m Data frame which stores the groupings/ sets of relationships that go together (should have the same type)
  #' @param cn A character vector of length m with names of concepts (in order - should match matrix)
  #' @param iter Number of iterations to run 
  #' @param set.concepts A character vector of the concepts the user wishes to fix/ hold at a certain value
  #' @param set.values A numeric vector containing the desired values in the same order
  #' @param infer_type A 1 x m character vector specifying the inference or squashing function desired. If a single entry is provided, sets all concepts to use that inference type.
  #' @param init A 1 x m numeric vector containing the initial concept values. Defaults to setting initial values at 1. If a single number is provided, it sets all initial concept values to that number.
  #' @param k A 1 x m numeric vector, or a single numeric. A value of 1, which corresponds to a modified Kosko algorithm, where causal concepts influence the *change* in the receiving concept. Set this to 0 if you would like the causal concepts to influence the *value* of the receiving concept directly.
  #' @param h A numeric. Represents the threshold (steepest part of the sigmoid function). See Penn 2013, Knight 2014 for example of h=0.5. This should probably be 0 if "sigmoid-tanh" is used
  #' @param lambda A numeric. Parameter that determines the steepness of the sigmoid function used
  #' @return A data frame of the results of the simulation, with rows corresponding to timestep (n = iter) and columns corresponding to concept (m = number of concepts)
  #'
{
  # The structure of this code is adapted from a function in the fcm R package, but highly modified. Notably, it supports the use of "relationship types" with logical conjunctions.
  
  ## Step 1: Convert the dataframes into matrices: weights in mx and type of relationship in rel_type_mx
  mx <- as.matrix(mx)
  rel_type_mx <- as.matrix(rel_type_mx)
  rel_type_mx[rel_type_mx==0] <- NA # Make sure any non-links are NAs
  
  # If there are more than one set of relationships (aka influences to one concept that has more than one type), throw an error
  if (any(rel_group_mx > 1, na.rm = TRUE)){
    stop("More than one set of causal concepts (with corresponding relation type) detected for >1 concepts. Currently not supported") # ***
    # Note: we can write a catch here so we can support different type of relation branches (later, loop over rel_group) 
  }
  
  ## This is an alternative approach to addressing the k=0 + no incoming influences case. Is used in conjunction with another group of lines commented out ~line 130
  # if (infer_type!="sigmoid-exp" && h==0 && k!=1){ # cases where f(0) = 0
  #   print("For concepts for which there is no input, let them equal to starting values throughout the simulation.")
  #   fix_initial_values <- TRUE
  # } else {
  #   fix_initial_values <- FALSE
  # }
  
  ## Step 2: Initialize Vectors
  
  # Create vector to store results
  act_vector = matrix(0, nrow = iter, ncol = length(mx[1,])) # columns contain concept values
  
  # Set initial values
  if (length(init) == 1){
    act_vector[1, ] = rep(init, length(mx[1, ]))
  } else if (length(init)==length(mx[1,])){
    act_vector[1, ] = init
  } else{
    stop("Please check the initial value(s) provided. It must be a vector of length equal 
         to the number of concepts, OR a single number.")
  }
  # For scenarios with set values, overwrite initial conditions
  if (!is.null(set.values)){
    act_vector[1, which(cn %in% set.concepts == TRUE)] = set.values
  }
  
  ## Run the simulation! (Loop over number of iterations)
  for (i in 2:iter) { # can we make this traceable for at least 1 round?
    
    ## Step 3: Adjust k values (Note: moved this up from in the infer_type loop 2021/02/25)
    # For concepts for which there is no input, let them influence themselves (see Penn et al 2013 - note that in their use case, they didn't do this and assumed everything, not just the ones without input, were drivers)
    if (length(k) == 1){ # If a single k value is given, create a vector, then make adjustments if k=0
      if (k == 0){ 
        k_ <- rep(0, length(cn))
        k_[apply(mx, 2, function(x) all(x==0))] <- 1
      } else{
        k_ <- rep(k, length(cn))
      }
    } else{ # If a vector of k's is given, make adjustments directly
      k_ <- k
      k_[is.na(k_)] <- 1 
    }

    ## Step 4: Add together all influences, using the appropriate aggregation function. Include self influence.
    infl_vector <- rep(0, length = length(cn))
    # Loop over relation type (add/ or/ etc.) 
    for (rel_type in na.exclude(unique(c(rel_type_mx)))){
      # print(rel_type)
      links <- mx
      links[rel_type_mx != rel_type|is.na(rel_type_mx)] <- NA # take only the links that correspond to this type
      contrb = matrix(act_vector[i - 1,], nrow = length(act_vector[i - 1, ]), ncol = length(mx[ , 1])) * links
      #diag(contrb) <- NA # to exclude self from comparison - but should already be excluded
      #print(contrb)
      
      self_infl <- (k_ * act_vector[i - 1, ]) # influence from previous timestep
      num_influences <- apply(act_vector, 2, function(x) sum(!is.na(x))) # count the non NAs 
      
      if (num_influences > 0){
        if (tolower(rel_type) == "req" || tolower(rel_type) == "and"){ 
          infl = (self_infl/(num_influences)) + apply(contrb, 2, function(x) ifelse(all(is.na(x)), 0, min(x,na.rm=TRUE))) # doesn't work if there are negative links
        } else if (tolower(rel_type) == "add" || tolower(rel_type)=="lookup"){ # temporarily lump "lookup" in this category 2019/08/20
          infl = (self_infl + apply(contrb, 2, function(x) ifelse(all(is.na(x)), 0, sum(x,na.rm=TRUE)))) / (num_influences) # switched to sum, and divide manually by number of influences (2021/02/25) # previously, used mean for these additive relationships 2019/08/20
        } else if (tolower(rel_type) == "or"){
          infl = (self_infl/(num_influences)) + apply(contrb, 2, function(x) ifelse(all(is.na(x)), 0, max(x,na.rm=TRUE))) # find maximum of element-wise product of the weight matrix and the values of the influencing variables
        } else {
          # Currently corresponds to what it was previously (in the fcm package), where it would have been simply below: infl_vector = (act_vector[i - 1, ] %*% mx) 
          infl <- self_infl + apply(contrb, 2, function(x) sum(x,na.rm=TRUE)) 
        }
      } else {
        infl <- self_infl
      }
      
      infl_vector <- infl_vector + infl # <- If more than one set of causal concepts exists for a single concept, may need to rethink this approach.
    } 
    #print(infl_vector)

  ## Step 5: Calculate values at this timestep according to the inference method
  # Here, loop over each type of inference (sigmoid-exp, etc) given (can be a single type, or a vector of types - for each concept) so we can do the calculations all at once.
  for (f in unique(infer_type)){
    
    # Figure out which concepts this inference type should be applied to
    if (length(infer_type)==1){ # If there is a single inference type provided, assume that this applies to all the concepts
      to_compute <- 1:length(cn)
    } else{ # If there are multiple inference types provided, figure out which ones match
      if (length(infer_type)==length(cn)){
        to_compute <- (infer_type == f)
      } else{
        stop("Length of inference types differs from number of concepts. Provide either a vector with entries corresponding to each concept, or a single inference type.")
      }
    }
    
    # Calculate the values
    if (f == "sigmoid-exp"){
      act_vector[i, to_compute] = 1/(1 + exp(-lambda * (infl_vector - h))) # without any influence, a concept starting at 0 tends to 0.659 if k=1, lambda=1
    } else if(f == "sigmoid-tanh"){
      act_vector[i, to_compute] = tanh(lambda * (infl_vector - h)) # h should be 0 in this case
    } else{ # note: may need to fix linear mapping (should it plateau past a certain point?)
      act_vector[i, to_compute] = infl_vector - h # linear mapping. See Kok 2009 for an example. A comparison of linear vs sigmoidal mapping is provided in Knight et al 2014 and Penn et al 2013.
    }
  }

    # # For concepts for which there is no input, let them equal to the previous value (should correspond to starting values) -- see commented out lines ~30
    # # downside: not consistent with treatment of other concepts - probably can comment out if h=0.5, BUT should definitely implement if f(0)=0 (sigmoid-tanh or linear)
    # if (fix_initial_values){
    #   act_vector[i,apply(mx,2,function(x) all(x==0))] <- act_vector[i - 1,which(apply(mx,2,function(x) all(x==0)))]
    # }
    
    ## Step 6: Are there any concepts that should remain fixed at a certain value?
    # Set clamp values here (override any numbers calculated above)
    if (!is.null(set.values)){
      act_vector[i, which(cn %in% set.concepts == TRUE)] = set.values
    }
  }
  if (all.equal(act_vector[iter, ], act_vector[iter - 1, ]) != TRUE) {
    warning("WARNING: Convergence not reached. Try increasing the number of iterations.")
  }
  
  # Step 7: Save the results, and generate plots if specified above (not saved)
  results = as.data.frame(act_vector)
  colnames(results) = cn
  if (plot){  # Plot progression
    ylims <- if (infer_type=="sigmoid-tanh"){c(-1,1)} else {c(0,1)}
    graphics::plot(act_vector[, 1] ~ seq(1, iter, 1), type = "n", 
                   ylim = ylims, xlab = "Iteration", ylab = "Value")
    for (n in 1:length(mx[1, ])) {
      graphics::points(act_vector[, n] ~ seq(1, iter, 1), type = "l", 
                       col = n)
    }
    graphics::legend("topright", legend = cn, col = seq(1, n, 1), lty = 1)
  }
  return(results)
}