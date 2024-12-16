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
    max_counts = {}

    for game_set in sets:
        colors = game_set.split(',')
        color_counts = extract_color_counts(colors)

        for color, count in color_counts.items():
            print(color, count, max_counts)

            if color not in max_counts or count > max_counts[color]:
                max_counts[color] = count
    return count_score_multiplier(max_counts)


def count_score_multiplier(max_counts):
    product = 1
    for count in max_counts.values():
        product *= count
    return product


def process_games(file_path):
    score_list = []
    with open(file_path, 'r') as file:
        data = file.read()
    games = data.split('Game ')[1:]
    for game in games:
        game_data = game.split(':', 1)
        game_score = evaluate_game(game_data[1])
        score_list.append(game_score)
    print(sum(score_list))


if __name__ == "__main__":
    process_games('Calendar_2023/day2/data.txt')
