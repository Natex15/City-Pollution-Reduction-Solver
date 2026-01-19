source("ProjectData.R")

# Solves the Linear Programming Problem using the Simplex method
# Returns a list containing the status, optimal Z value, solution matrix, and iteration history
# Same with my previous Exercise but added some features like Error Checking
# I used -1e-9 to reduce floating point error
SimplexMethod <- function(tableau){
  
  # Check if valid tableau
  if(any(is.na(tableau))){
    return(list(Status = "Error", Message = "Input Data contains NA"))
  }
  
  # Check size
  finalTableau = tableau$tableau
  rowNum = nrow(tableau$tableau)
  colNum = ncol(tableau$tableau)
  
  # Initialize list
  TableauIteration <- list()
  BasicSolIteration <- list()
  InitialTableau <- finalTableau
  hasNegative = TRUE
  iter = 0
  status = "Pending"
  
  # Loops until no negative
  while(hasNegative == TRUE){
    PC = 1
    min_val <- -1e-9 
    for(i in 1:colNum){
      if(finalTableau[rowNum, i] < min_val){
        PC = i
        min_val <- finalTableau[rowNum, i]
      }
    }
    
    # If no negative, we found the solution
    if(min_val >= -1e-9){
      hasNegative = FALSE
      status = "Optimal"
      break
    }
    
    PR <- NA
    minRatio <- Inf
    
    # Find PR, Copied from exer except changed 0 to 1e-9
    for (i in 1:(rowNum - 1)) {
      denominator <- finalTableau[i, PC]
      numerator <- finalTableau[i, colNum]
      
      if (is.na(denominator) || denominator <= 1e-9) next
      
      ratio <- numerator / denominator
      if (!is.na(ratio) && ratio >= 0 && ratio < minRatio) {
        minRatio <- ratio
        PR <- i
      }
    }
    
    # If no valid pivot row exists, Infeasible, Break the loop
    if (is.na(PR)) {
      status = "Infeasible" 
      break 
    }
    
    # Finding Pivot element
    PE = finalTableau[PR,PC]
    PivotRow = finalTableau[PR,]
    NormalizedRow = PivotRow/PE
    finalTableau[PR,] = NormalizedRow
    
    # Zero out the rest of the pivot column
    for(i in 1:(rowNum)){
      if(i == PR){next}
      finalTableau[i,] = finalTableau[i,] - (NormalizedRow*finalTableau[i,PC])
    }
    
    # Logs the tableau iteration
    iter = iter+1
    TableauIteration[[paste("Iteration ",iter)]] <- finalTableau
    
    # Finds the basic sol, copied from my maximization exer
    current_col_names <- colnames(finalTableau)[1:(colNum-1)]
    if(is.null(current_col_names)) current_col_names <- paste0("Col", 1:(colNum-1))
    basicSolutionVector = matrix(data = 0, ncol = colNum-1, nrow = 1, dimnames = list(NULL, current_col_names))
    for(i in 1:(colNum-1)){
      numofData = 0
      ind = 0
      for(j in 1:rowNum){
        if(abs(finalTableau[j,i]) > 1e-9){ 
          numofData = numofData + 1
          ind = j
        }
      }
      if(numofData == 1 && ind > 0){
        basicSolutionVector[1, i] = finalTableau[ind,colNum]
      }
    }
    # Logs the Basic Sol Iteration
    BasicSolIteration[[paste("Iteration ",iter)]] <- basicSolutionVector
  }
  
  # Initialize Z and Print, value also for infeasible
  Z <- NA
  forPrintSol <- NULL
  
  # Basic solution
  basicSolution <- finalTableau[rowNum,] 
  
  # Check if optimal
  if(status == "Optimal") {
    
    # Gets Z
    Z = unname(finalTableau[rowNum, colNum], force = FALSE)
    
    # Ensure names exist for mapping
    if(is.null(names(basicSolution))) {
      names(basicSolution) <- colnames(finalTableau)
    }
    
    # Makes last basic sol be the last row
    BasicSolIteration[[paste("Iteration ", iter)]] <- matrix(basicSolution, nrow = 1, dimnames = list(NULL, names(basicSolution)))
    
    # Formats the final tableau
    valid_names <- tableau$names[tableau$names %in% names(basicSolution)]
    quantities <- basicSolution[valid_names]
    unit_costs <- tableau$costs[1:length(valid_names)]
    total_costs <- quantities * unit_costs
    
    forPrintSol <- matrix(c(valid_names, quantities, total_costs), nrow = length(valid_names), byrow = FALSE)
    
    # Filter out variables with near-zero values
    forDelete <- c()
    for(i in 1:nrow(forPrintSol)){
      val <- as.numeric(forPrintSol[i, 2])
      if(abs(val) <= 1e-9){ 
        forDelete <- c(forDelete, i)
      }
    }
    if(length(forDelete) > 0){
      forPrintSol <- forPrintSol[-forDelete, , drop=FALSE]
    }
  }
  
  #output
  Output <- list(
    Status = status,
    basicSolution = basicSolution, 
    Z = Z, 
    forPrintSol = forPrintSol, 
    TableauIteration = TableauIteration, 
    InitialTableau = InitialTableau, 
    BasicSolIteration = BasicSolIteration
  )
  return (Output)
}

# Transpose Function
MinimizationTranspose <- function(matrices,targets){
  n <- nrow(matrices) + 1
  if(nrow(matrices) <2) return(NA)
  
  company_Names <- rownames(matrices)
  
  # Slack Variables for each company
  companies <- diag(n)
  dimnames(companies) <- list(NULL,c(company_Names,"Z"))
  
  # Constraints/Targets
  matrix_pollutants_data <- cbind(t(matrices)[-1,],targets)
  costs <- c(t(matrices)[1,],c(0))
  
  # Transpose
  initial_transposed <- t(rbind(matrix_pollutants_data,costs))
  dimnames(initial_transposed) = list(NULL,NULL)
  
  # Make the Objective negative
  initial_transposed[n,] <- initial_transposed[n,]*-1
  
  # Add slack variables for max projects per company which is 20
  max_Companies <- diag(rep(-1,times = (n-1)))
  max_Companies <- rbind(max_Companies,rep(20,times = (n-1)))
  
  Solution <- initial_transposed[,11]
  
  # Return list
  tableau <- list(
    tableau = cbind(initial_transposed[,-11],max_Companies,companies,Solution),
    names = company_Names,
    costs = costs
  )
  return(tableau)
}

# Used in app to convert selected to matrix
namesToMatrix <- function(names){
  newMatrix <- projects_matrix[names,]
  return(newMatrix)
}