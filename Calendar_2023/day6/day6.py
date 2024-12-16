def all_distances(time, distance):
    wins = 0
    for i in range(1, time):
        if (i * (time-i)) > distance:
            wins += 1
    return wins


def main(path):
    with open(path, 'r') as file:
        lines = file.readlines()

    times = [int(value) for value in lines[0].split()[1:]]
    distances = [int(value) for value in lines[1].split()[1:]]

    pairs = list(zip(times, distances))

    end_result = 1
    for time, distance in pairs:
        print(time, distance)
        end_result *= all_distances(time, distance)

    print(end_result)


if __name__ == "__main__":
    main('Calendar_2023/day6/data.txt')
