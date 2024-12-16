def sum_calibration(path):
    lines = []
    sum = []
    nums = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
    with open(path) as file:
        for line in file:
            line = line.rstrip('\n')
            lines.append(line)

    for line in lines:
        first = None
        last = None
        for element in line:
            if first == None and element in nums:
                first = element
                continue
            if first != None and element in nums:
                last = element
                continue
        if last == None and first == None:
            print(line)

        if last == None:
            last = first

        helper = str(first) + str(last)
        sum.append(helper)

    answer = 0
    for i in sum:
        answer += int(i)
    print(answer)


if __name__ == "__main__":
    sum_calibration('Calendar_2023/day1/data.txt')
