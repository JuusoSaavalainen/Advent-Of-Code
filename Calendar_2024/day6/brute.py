import numpy as np

# it so funny that python is seen as fast in comp to R here XD

directions = {
    "up": (-1, 0),
    "right": (0, 1),
    "down": (1, 0),
    "left": (0, -1)
}


def turn(current_direction):
    direction_order = ["up", "right", "down", "left"]
    idx = direction_order.index(current_direction)
    return direction_order[(idx + 1) % 4]


def simulate_path(grid_matrix, start_coords, start_direction):
    object_coords = start_coords
    direction = start_direction
    visited_states = set()
    path = []

    while True:
        state = (object_coords[0], object_coords[1], direction)
        if state in visited_states:
            return True, path
        visited_states.add(state)
        path.append(state)
        next_coords = (object_coords[0] + directions[direction][0],
                       object_coords[1] + directions[direction][1])
        if next_coords[0] < 0 or next_coords[0] >= grid_matrix.shape[0] or \
           next_coords[1] < 0 or next_coords[1] >= grid_matrix.shape[1]:
            return False, path
        if grid_matrix[next_coords[0], next_coords[1]] == "#":
            direction = turn(direction)
            next_coords = (object_coords[0] + directions[direction][0],
                           object_coords[1] + directions[direction][1])
            if grid_matrix[next_coords[0], next_coords[1]] == "#":
                direction = turn(direction)
        else:
            object_coords = next_coords


def find_critical_positions(grid_matrix, path):
    critical_positions = set()
    for row, col, direction in path:
        if grid_matrix[row, col] == ".":
            critical_positions.add((row, col))
    return critical_positions


def count_infinite_loop_positions(grid_matrix, start_coords, start_direction):
    loop_count = 0
    loop, path = simulate_path(grid_matrix, start_coords, start_direction)
    critical_positions = find_critical_positions(grid_matrix, path)
    for row, col in critical_positions:
        new_grid = grid_matrix.copy()
        new_grid[row, col] = "#"
        loop, _ = simulate_path(new_grid, start_coords, start_direction)
        if loop:
            loop_count += 1
    return loop_count


with open("data.txt", "r") as f:
    data = f.read().strip().splitlines()
grid_matrix = np.array([list(line) for line in data])
start_coords = tuple(np.argwhere(grid_matrix == "^")[0])
start_direction = "up"
num_wall_solutions = count_infinite_loop_positions(
    grid_matrix, start_coords, start_direction)
print(num_wall_solutions)
