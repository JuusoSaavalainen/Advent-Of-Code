def find_substrings_with_integers(string):
    substring_mapping = {
        "one": 1, "two": 2, "three": 3, "four": 4, "five": 5,
        "six": 6, "seven": 7, "eight": 8, "nine": 9,
        "1": 1, "2": 2, "3": 3, "4": 4, "5": 5,
        "6": 6, "7": 7, "8": 8, "9": 9
    }
    substrings = ["one", "two", "three", "four", "five", "six",
                  "seven", "eight", "nine", "1", "2", "3", "4",
                  "5", "6", "7", "8", "9"]
    positions = []
    for substring in substrings:
        start = 0
        while start != -1:
            start = string.find(substring, start)
            if start != -1:
                positions.append((start, substring_mapping[substring]))
                start += 1

    positions.sort()
    result = ''.join(str(integer) for _, integer in positions)
    return result


def main(path):
    result = []
    with open(path, "r") as file:
        data_lines = file.readlines()

    for line in data_lines:
        line = line.strip()
        found_integers = find_substrings_with_integers(line)
        result.append(found_integers[0] + found_integers[-1])

    print(sum(int(integer) for integer in result))


if __name__ == "__main__":
    main('Calendar_2023/day1/data.txt')


# was tought for me...
