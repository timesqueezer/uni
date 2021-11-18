#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt


SAMPLE_PERIOD = 1


def partial_sync(t, ks):
    v = 0
    for k in ks:
        v += np.sinc((t - k * SAMPLE_PERIOD) / SAMPLE_PERIOD)

    return v


if __name__ == '__main__':
    plot_range = 5

    plot_ks = [
        list(range(-1, 2)),
        list(range(-2, 3)),
        list(range(-3, 4)),
    ]

    plot_points = np.arange(-5.0, 5.0, 0.1)

    plt.figure(figsize=(10, 12), dpi=96)
    plt.subplot(411)
    plt.axis([-plot_range, plot_range, 0, 2])
    plt.plot(plot_points, [1 for t in plot_points])

    for i in range(len(plot_ks)):
        plt.subplot(412 + i)
        plt.axis([-plot_range, plot_range, -2, 2])
        plt.plot(plot_points, [1 for t in plot_points], 'r--')
        plt.plot(plot_points, [partial_sync(t, plot_ks[i]) for t in plot_points])

    plt.show()
