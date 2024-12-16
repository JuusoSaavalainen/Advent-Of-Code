def main(path):
    f_res = 0
    res = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    with open(path, 'r') as file:
        data = file.read()
    cards = data.splitlines()

    for card in cards:
        amount = res.pop(0)
        f_res += amount
        res.append(1)
        card_info, values_str = card.split(':')
        left_values = set(map(int, values_str.split('|')[0].split()))
        right_values = set(map(int, values_str.split('|')[1].split()))
        common_elements = left_values & right_values
        num_common_elements = len(common_elements)
        if num_common_elements == 0:
            continue
        for i in range(num_common_elements):
            res[i] = res[i] + (1 * amount)
    print(f_res)


if __name__ == "__main__":
    main('Calendar_2023/day4/data.txt')
    # res = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    # common = 3
    # for i in range(common):
    #    res[i] = res[i] + 1
    # print(res)

# Kiitos kivasta päivästä
