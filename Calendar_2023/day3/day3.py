def main(path):
    with open(path, 'r') as file:
        data = file.read()
    datagrid = data.splitlines()
    positions = set()
    for r, row in enumerate(datagrid):
        for c, char in enumerate(row):
            if char == "." or char.isdigit():
                continue
            # 3x3 search kinda reminds me of sliding window XD
            for cur_row in [r - 1, r, r + 1]:
                for cur_col in [c - 1, c, c + 1]:
                    if not datagrid[cur_row][cur_col].isdigit():
                        continue
                    if cur_row < 0 or cur_col >= len(datagrid):
                        continue
                    if cur_col < 0 or cur_col >= len(datagrid[cur_row]):
                        continue

                    while cur_col > 0 and datagrid[cur_row][cur_col - 1].isdigit():
                        cur_col -= 1
                    positions.add((cur_row, cur_col))
    res = []
    for row, col in positions:
        numbah = ""
        while col < len(datagrid[row]) and datagrid[row][col].isdigit():
            numbah += datagrid[row][col]
            col += 1
        res.append(int(numbah))
    print(sum(res))


if __name__ == "__main__":
    main('Calendar_2023/day3/data.txt')
