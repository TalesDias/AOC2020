def part1():
    with open("input.txt",'r') as fl:
        lines = [int(line) for line in fl.readlines()]

        for candidate in lines:
            for match in lines:
                if (match + candidate == 2020):
                    print(match * candidate)
                    return

def part2():

    with open("input.txt",'r') as fl:
        lines = [int(line) for line in fl.readlines()]

        for candidate in lines:
            for match1 in lines:
                for match2 in lines:
                    if (match1 + match2 + candidate == 2020):
                        print(match1 * match2 * candidate)
                        return

print("Part1:")
part1()
print("\nPart2:")
part2()