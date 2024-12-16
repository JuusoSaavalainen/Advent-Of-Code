def main(path):
    cards_data = []
    res = 0
    with open(path, 'r') as file:
        data = file.read()
    cards = data.splitlines()

    for card in cards:
        card_info, values_str = card.split(':')
        left_values = set(map(int, values_str.split('|')[0].split()))
        right_values = set(map(int, values_str.split('|')[1].split()))
        common_elements = left_values & right_values
        num_common_elements = len(common_elements)
        if num_common_elements == 0:
            continue
        res += calc(num_common_elements)
    print(res)


def calc(n):
    result = 1
    for _ in range(n-1):
        result *= 2
    return result


if __name__ == "__main__":
    main('Calendar_2023/day4/data.txt')
