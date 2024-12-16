# slide slide slide boyyyyy, a in center!
check_for_x_mas <- function(matrix) {
  nrows <- nrow(matrix)
  ncols <- ncol(matrix)
  ans <- 0
  for (row in 2:(nrows-1)) {
    for (col in 2:(ncols-1)) {
      if (mat[row, col] == "A") {
        diag1 <- c(matrix[row-1, col-1], matrix[row+1, col+1])
        diag2 <- c(matrix[row-1, col+1], matrix[row+1, col-1])
        if ((all(diag1 == c("M", "S")) || all(diag1 == c("S", "M"))) &&
            (all(diag2 == c("S", "M")) || all(diag2 == c("M", "S")))) {
          cat(sprintf("Winner in (%d, %d)\n", row, col))
          ans <- ans + 1
        }
      }
    }
  }
  return(ans)
}
mattt <- readLines("data.txt")
splitti <- strsplit(mattt, "")
matriksi <- do.call(rbind, splitti)
check_for_x_mas(matriksi)
