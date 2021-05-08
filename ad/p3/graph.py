def calc(N, a):
    max_path_lengths = []
    for node in range(N):
        if node == 0:
            max_path_lengths.append([0])

        else:
            previous_edges = list(filter(lambda n: n[1] == node, a))
            if len(previous_edges) == 0:
                max_path_lengths.append([0])

            lengths = []
            for previous_edge in previous_edges:
                previous_node = previous_edge[0]

                if not max_path_lengths[previous_node]:
                    lengths.append(0)

                else:
                    max_previous_length = max(max_path_lengths[previous_node])
                    lengths.append(max_previous_length + 1)

            max_path_lengths.append(lengths)

    from pprint import pprint
    pprint(max_path_lengths)

if __name__ == '__main__':
    print(calc(3,[[1,2],[2,3]]), 2)
    print(calc(5, [[3, 4], [1, 5], [1, 4], [4, 5], [2, 5]]), 2)
    # print(calc(7, [[1, 2], [3, 4], [1, 5], [3, 7], [4, 6], [5, 7], [2, 3], [6, 7], [2, 6], [5, 6], [1, 6], [2, 5], [4, 7]]), 5)
    # print(calc(6, [[2, 4], [4, 6], [2, 3], [5, 6], [3, 6], [1, 6], [1, 3], [3, 5]]), 3)


  1 2 3 4      5
  0 0 0 [1, 1] [1, 2]
