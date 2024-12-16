# into the the matrix it goes
xmas <- function(matrix, target, directions) {
  nrows <- nrow(matrix)
  ncols <- ncol(matrix)
  count <- 0
  for (row in 1:nrows) {
    for (col in 1:ncols) {
      for (dir_name in names(directions)) {
        dir <- directions[[dir_name]]
        positions <- lapply(0:3, function(step) { list(row = row + step * dir[1], col = col + step * dir[2]) })
        if (all(sapply(positions, function(pos) pos$row >= 1 && pos$row <= nrows && pos$col >= 1 && pos$col <= ncols))) {
          chars <- sapply(positions, function(pos) matrix[pos$row, pos$col])
          if (all(chars == target)) {
            cat(sprintf("Winner in (%d, %d)\n", row, col))
            count <- count + 1
          }
        }
      }
    }
  }
  return(count)
}
mattt <- readLines("data.txt")
splitti <- strsplit(mattt, "")
matriksi <- do.call(rbind, splitti)
target <- strsplit("XMAS", "")[[1]]
directions <- list(
  horizontal_1 = c(0, 1),
  horizontal_2 = c(0, -1),
  vertical_1 = c(1, 0),
  vertical_2 = c(-1, 0),
  diagonal_1 = c(1, 1),
  diagonal_2 = c(-1, -1),
  diagonal_1 = c(-1, 1),
  diagonal_2 = c(1, -1)
)
xmas(matriksi, target, directions)
