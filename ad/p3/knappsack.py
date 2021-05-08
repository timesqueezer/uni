def knapSack(value, volume, capacity):
    assert len(value) == len(volume)

    for x in volume:
        assert x == round(x)

    cases = [[]]
    for i in range(capacity + 1):
        cases[0].append(0)

    for i in range(len(value) + 1):
        list = []
        for i in range(capacity + 1):
            list.append(0)
        cases.append(list)

    for i in range(1, len(value) + 1):
        for c in range(1, capacity + 1):
            max_value_without_item = cases[i - 1][c]
            max_value_with_item = 0

            item_volume = volume[i - 1]
            if (c >= item_volume):
                max_value_with_item = value[i - 1]

                remaining_capacity = c - item_volume
                max_value_with_item += cases[i - 1][remaining_capacity]

            cases[i][c] = max(max_value_with_item, max_value_without_item)

    return cases[len(value)][capacity]


if __name__ == '__main__':
    print(knapSack([0.5, 1.3],[2, 5],3), 0.5)
    print(knapSack([6, 8],[6, 8],0), 0)
    print(knapSack([6, 8],[6, 8],10), 8)
    print(knapSack([1, 1, 99],[6, 4, 11],10), 2)
    print(knapSack([6, 8],[6, 8],17), 14)

    print(knapSack([2, 2, 2, 2, 2, 2],[1, 2, 3, 4, 5, 6], 9), 6)
