class Node:
    def __init__(self):
        self.key = 0
        self.left = None
        self.right = None
        self.parent = None

    def __str__(self):
        s = "Node<{}>\n".format(self.key)
        if self.left:
            s += '    l: ' + str(self.left)

        if self.right:
            s += '    r: ' + str(self.right)

        return s

def tree_minimun(node):
    while node.left is not None:
        node = node.left

    return node

# Node in parameter represents root of tree
def delete(root, target):
    if root is target:
        return None

    current_node = root
    while current_node is not None and current_node.key != target.key:
        if target.key < current_node.key:
            current_node = current_node.left
        else:
            current_node = current_node.right

    if not current_node:
        return root

    if not current_node.left and not current_node.right:
        if current_node is current_node.parent.left:
            current_node.parent.left = None
        else:
            current_node.parent.right = None

    elif current_node.left and not current_node.right:
        if current_node is current_node.parent.left:
            current_node.parent.left = current_node.left
            current_node.left.parent = current_node.parent
        else:
            current_node.parent.right = current_node.left
            current_node.left.parent = current_node.parent

    elif current_node.right and not current_node.left:
        if current_node is current_node.parent.left:
            current_node.parent.left = current_node.right
            current_node.right.parent = current_node.parent
        else:
            current_node.parent.right = current_node.right
            current_node.right.parent = current_node.parent

    else:
        # successor = tree_successor(current_node)
        y = tree_minimun(current_node.right)
        if y.left is not None:
            x = y.left
        else:
            x = y.right

        if x is not None:
            x.parent = y.parent

        if y.parent is not None:
            if y is y.parent.left:
                y.parent.left = x
            else:
                y.parent.right = x

        if y is not current_node:
            current_node.key = y.key


        """if current_node is current_node.parent.left:
            current_node.parent.left = successor
            successor.parent = current_node.parent

        else:
            current_node.parent.right = successor
            successor.parent = current_node.parent"""

    return root


if __name__ == '__main__':
    # build tree
    n1 = Node()
    n1.key = 37
    n1.left = Node()
    n1.left.key = 32
    n1.left.parent = n1
    n1.right = Node()
    n1.right.key = 78
    n1.right.parent = n1

    target = Node()
    target.key = 21
    target.right = n1
    n1.parent = target
    target.left = Node()
    target.left.key = 13
    target.left.parent = target

    root = Node()
    root.key = 85
    root.left = target
    target.parent = root
    root.right = Node()
    root.right.key = 942
    root.right.parent = root
    root.right.left = Node()
    root.right.left.key = 136
    root.right.left.parent = root.right

    # Call delete
    root = delete(root, target)
    print(root)

    # Compare
    n2 = Node()
    n2.key = 37
    n2.right = Node()
    n2.right.key = 78
    n2.right.parent = n2

    replaced = Node()
    replaced.key = 32
    replaced.right = n2
    n2.parent = replaced
    replaced.left = Node()
    replaced.left.key = 13
    replaced.left.parent = replaced

    newRoot = Node()
    newRoot.key = 85
    newRoot.left = replaced
    replaced.parent = newRoot
    newRoot.right = Node()
    newRoot.right.key = 942
    newRoot.right.parent = newRoot
    newRoot.right.left = Node()
    newRoot.right.left.key = 136
    newRoot.right.left.parent = newRoot.right

    print(newRoot)
    """root = Node()
    root.key = 2
    root.left = Node()
    root.left.key = 1
    root.left.parent = root
    root.right = Node()
    root.right.key = 3
    root.right.parent = root
    root.right.right = Node()
    root.right.right.key = 99
    root.right.right.parent = root.right

    # Call delete
    root = delete(root, root.right)
    print(root)

    # Compare
    NewRoot = Node()
    NewRoot.key = 2
    NewRoot.left = Node()
    NewRoot.left.key = 1
    NewRoot.left.parent = NewRoot
    NewRoot.right = Node()
    NewRoot.right.key = 99
    NewRoot.right.parent = NewRoot

    print(NewRoot)

    # build tree
    root = Node()
    root.key = 2
    root.left = Node()
    root.left.key = 1
    root.left.parent = root
    root.right = Node()
    root.right.key = 50
    root.right.parent = root
    root.right.right = Node()
    root.right.right.key = 99
    root.right.right.parent = root.right
    root.right.left = Node()
    root.right.left.key = 25
    root.right.left.parent = root.right

    # Call delete
    print(root)
    root = delete(root, root.right)
    print(root)

    # Compare
    newRoot = Node()
    newRoot.key = 2
    newRoot.left = Node()
    newRoot.left.key = 1
    newRoot.left.parent = newRoot
    newRoot.right = Node()
    newRoot.right.key = 99
    newRoot.right.parent = newRoot
    newRoot.right.left = Node()
    newRoot.right.left.key = 25
    newRoot.right.left.parent = newRoot.right

    print(newRoot)"""
