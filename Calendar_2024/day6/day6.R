# uncomment for test inp
# data <- readLines("data2.txt")
data <- readLines("data.txt")
data_lines <- strsplit(data, "")
grid_matrix <- do.call(rbind, data_lines)

# statics
directions <- list("up" = c(-1, 0),
                   "right" = c(0, 1),
                   "down" = c(1, 0),
                   "left" = c(0, -1))
object_coords <- which(grid_matrix == "^", arr.ind = TRUE)
direction <- "up"
coords_visited <- list(paste(object_coords[1], object_coords[2], sep = ","))

# logic
turn <- function(current_direction) {
  direction_order <- c("up", "right", "down", "left")
  idx <- match(current_direction, direction_order)
  return(direction_order[(idx %% 4) + 1])
}

# solve
while (TRUE) {
  next_coords <- object_coords + directions[[direction]]
  if (next_coords[1] < 1 || next_coords[1] > nrow(grid_matrix) || 
      next_coords[2] < 1 || next_coords[2] > ncol(grid_matrix)) {
    break
  }
  if (grid_matrix[next_coords[1], next_coords[2]] == "#") {
    direction <- turn(direction)
  } else {
    object_coords <- next_coords
    coords_visited <- c(coords_visited, paste(object_coords[1], object_coords[2], sep = ","))
  }
}

# res
unique_visited <- unique(coords_visited)
cat("Unique coordinates count:", length(unique_visited), "\n")
cat("Route:\n", paste(unique_visited, collapse = " -> "), "\n")
