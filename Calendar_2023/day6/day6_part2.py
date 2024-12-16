def all_distances(time, distance):
    wins = 0
    for i in range(1, time):
        if (i * (time-i)) > distance:
            wins += 1
    return wins


def main(path):
    with open(path, 'r') as file:
        lines = file.readlines()

    time_values = ''.join(lines[0].split()[1:])
    distance_values = ''.join(lines[1].split()[1:])
    result = (int(time_values), int(distance_values))

    print(all_distances(*result))


if __name__ == "__main__":
    main('Calendar_2023/day6/data.txt')
