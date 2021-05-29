import numpy as np
import matplotlib.pyplot as plt


x = np.array([[1,1,1], [1,1,0], [1,0,1], [1,0,0]])

t = np.array([1,0,0,0])

def inner_activation(weights, input_activations):
    h = 0
    for j, _x in enumerate(input_activations):
        h += _x * weights[j]

    return h

def g(h):
    y = 1 if h >= 0 else 0
    return y

def forward_pass(weights, inputs):
    return g(inner_activation(weights, inputs))

def update(weights, l, inputs, y):
    eta = 0.01
    updated_weights = []
    # for i in range(len(y)):
    for j, w in enumerate(weights):
        delta_w = eta * (l - y) * inputs[j]
        updated_weights.append(w + delta_w)

    return updated_weights

def error(preds, labels):
    res = 0
    for i in range(len(preds)):
        s = (labels[i] - preds[i]) ** 2
        res += s
    return 0.5 * res

def AND(a,b):
    return g(inner_activation([1,a,b],w))

if __name__ == '__main__':
    w = np.array([3.0, -3.0, -2.0]) # weights

    for iteration in range(1000):
        for i, d in enumerate(x):
            y = forward_pass(w, d)
            w = update(w, t[i], d, y)

        if iteration % 50 == 0:
            print(w)

    preds = [forward_pass(w, d) for d in x]
    print('preds', preds)
    print('error', error(t, preds))
