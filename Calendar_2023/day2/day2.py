def extract_color_counts(colors):
    color_counts = {}
    for color in colors:
        color_parts = color.strip().split(' ', 1)
        color_name = color_parts[-1]
        if len(color_parts) == 2 and color_parts[0].isdigit():
            count = int(color_parts[0])
            color_counts[color_name] = count
    return color_counts


def evaluate_game(game_data):
    sets = game_data.split(';')
    for game_set in sets:
        colors = game_set.split(',')
        color_counts = extract_color_counts(colors)
        if check_limits(color_counts):
            return True
    return False


def check_limits(color_counts):
    red_limit = 12
    green_limit = 13
    blue_limit = 14
    return (
        color_counts.get('red', 0) > red_limit
        or color_counts.get('green', 0) > green_limit
        or color_counts.get('blue', 0) > blue_limit
    )


def process_games(file_path):
    legit_games = []
    with open(file_path, 'r') as file:
        data = file.read()
    games = data.split('Game ')[1:]
    for game in games:
        game_number, game_data = game.split(':', 1)
        if not evaluate_game(game_data):
            legit_games.append(int(game_number))
    print(sum(legit_games))


if __name__ == "__main__":
    process_games('Calendar_2023/day2/data.txt')
