# Kapitel_10_Variante_2.py
#
# Mit Hilfe dieses Skripts koennen Sie die Dynamik von zwei gravitativ 
# miteinander wechselwirkenden Punktteilchen im dreidimensionalen Raum 
# visualisieren. Dabei ist die Annahme, dass Sie die erforderlichen Daten
# bereits mit Kapitel_10_Variante_1.py erzeugt haben.

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.animation import FuncAnimation

data = np.load('Kapitel_10_Positionen.npy')
num_points = data.shape[0]

# 3D-Animation der Ortsvektoren der Teilchen
fig = plt.figure("Zweiteilchenbewegung")
ax = Axes3D(fig)
points, = ax.plot([], [], [], 'ro')
def init():
    ax.set_xlim(-50, 50)                   # Zu zeichnendes x-Intervall
    ax.set_ylim(-50, 50)                   # Zu zeichnendes y-Intervall
    ax.set_zlim(-50, 50)                   # Zu zeichnendes z-Intervall
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.set_zlabel("z")
    return points,
def update(n, data):
    points.set_data(data[n, :, 0], data[n, :, 1])
    points.set_3d_properties(data[n, :, 2], 'z')
    return points,
ani = FuncAnimation(fig, update, init_func = init, frames = num_points, 
                     interval = 2, fargs = (data,))

plt.show()