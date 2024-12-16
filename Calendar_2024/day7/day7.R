evaluate_expr <- function(nums, operationish) {
  result <- nums[1]
  for (i in 1:length(operationish)) {
    if (operationish[i] == '+') {
      result <- result + nums[i + 1]
    } else if (operationish[i] == '*') {
      result <- result * nums[i + 1]
    }
  }
  return(result)
}

generate_combinations <- function(num_count) {
  operator_combinations <- expand.grid(rep(list(c('+', '*')), num_count - 1))
  return(as.matrix(operator_combinations))
}

check_valid_equation <- function(target, nums) {
  num_count <- length(nums)
  print(paste("Line being processed:", target, "and numbers:", paste(nums, collapse = " ")))
  if (num_count == 1) {
    return(nums[1] == target)
  }
  
  operator_combinations <- generate_combinations(num_count)
  
  for (i in 1:nrow(operator_combinations)) {
    operationish <- as.character(operator_combinations[i,])
    result <- evaluate_expr(nums, operationish)
    if (result == target) {
      return(TRUE)
    }
  }
  return(FALSE)
}

solve <- function(input) {
  total_calibration_result <- 0
  for (line in input) {
    parts <- strsplit(line, ":")[[1]]
    target <- as.numeric(parts[1])
    nums <- as.numeric(strsplit(trimws(parts[2]), " ")[[1]])
    if (check_valid_equation(target, nums)) {
      total_calibration_result <- total_calibration_result + target
    }
  }
  return(total_calibration_result)
}

file_path <- "data.txt"
input <- readLines(file_path)
result <- solve(input)
print(result)
