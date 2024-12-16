def main(path):
    with open(path, 'r') as file:
        data = file.read()
    datagrid = data.splitlines()
    solution = 0
    for r, row in enumerate(datagrid):
        for c, char in enumerate(row):
            if char != "*":
                continue
            positions = set()
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
            if len(positions) != 2:
                continue
            res = []
            for row_, col_ in positions:
                numbah = ""
                while col_ < len(datagrid[row_]) and datagrid[row_][col_].isdigit():
                    numbah += datagrid[row_][col_]
                    col_ += 1
                res.append(int(numbah))
            solution += res[1] * res[0]
    print(solution)


if __name__ == "__main__":
    main('Calendar_2023/day3/data.txt')

# Total time in day around 2 hours :D "tira" flashback, not so bad but i tought ill get it faster
# I guess there is some rustiness iterating this kinda data. First try was with regex second with
# searching the numbers but only when i changed to search for the odd elements it became doable
# its definalty possible with searching the nums first and should be with regex too...
# tomorrow shall be better !
