# gotta love the brute time with R :D python fast what XDXDXD
library(dplyr)

directions <- list("up" = c(-1, 0),
                   "right" = c(0, 1),
                   "down" = c(1, 0),
                   "left" = c(0, -1)
                   )

turn_right <- function(current_direction) {
  direction_order <- c("up", "right", "down", "left")
  idx <- match(current_direction, direction_order)
  return(direction_order[(idx %% 4) + 1])
}

simulate_path <- function(grid_matrix, start_coords, start_direction) {
  object_coords <- start_coords
  direction <- start_direction
  visited_states <- c()
  
  while (TRUE) {
    state <- paste(object_coords[1], object_coords[2], direction, sep = ",")
    if (state %in% visited_states) {
      return(list(loop = TRUE, path = visited_states))
    }
    visited_states <- c(visited_states, state)
    next_coords <- object_coords + directions[[direction]]
    if (next_coords[1] < 1 || next_coords[1] > nrow(grid_matrix) || 
        next_coords[2] < 1 || next_coords[2] > ncol(grid_matrix)) {
      return(list(loop = FALSE, path = visited_states))
    }
    if (grid_matrix[next_coords[1], next_coords[2]] == "#") {
      direction <- turn_right(direction)
      next_coords <- object_coords + directions[[direction]]
      if (grid_matrix[next_coords[1], next_coords[2]] == "#") {
        direction <- turn_right(direction)
      }
    } else {
      object_coords <- next_coords
    }
  }
}

find_critical_positions <- function(grid_matrix, path) {
  critical_positions <- c()
  for (state in path) {
    parts <- unlist(strsplit(state, ","))
    row <- as.numeric(parts[1])
    col <- as.numeric(parts[2])
    if (grid_matrix[row, col] == ".") {
      new_position <- paste(row, col, sep = ",")
      if (!(new_position %in% critical_positions)) {
        critical_positions <- c(critical_positions, new_position)
      }
    }
  }
  starting_position <- paste(object_coords[1], object_coords[2], sep = ",")
  critical_positions <- setdiff(critical_positions, starting_position)
  return(critical_positions)
}

count_infinite_loop_positions <- function(grid_matrix, start_coords, start_direction) {
  result <- simulate_path(grid_matrix, start_coords, start_direction)
  path <- result$path
  critical_positions <- find_critical_positions(grid_matrix, path)
  loop_count <- 0
  for (position in critical_positions) {
    parts <- unlist(strsplit(position, ","))
    row <- as.numeric(parts[1])
    col <- as.numeric(parts[2])
    grid_matrix[row, col] <- "#"
    result <- simulate_path(grid_matrix, start_coords, start_direction)
    if (result$loop) {
      loop_count <- loop_count + 1
    }
    grid_matrix[row, col] <- "."
  }
  return(loop_count)
}

data <- readLines("data2.txt")
data_lines <- strsplit(data, "")
grid_matrix <- do.call(rbind, data_lines)

object_coords <- which(grid_matrix == "^", arr.ind = TRUE)
start_direction <- "up"

num_wall_solutions <- count_infinite_loop_positions(grid_matrix, object_coords, start_direction)
cat(num_wall_solutions)
