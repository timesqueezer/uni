# Kapitel_10_Variante_4.py
#
# Mit Hilfe dieses Skripts koennen Sie die Dynamik von zwei gravitativ 
# miteinander wechselwirkenden Punktteilchen im dreidimensionalen Raum 
# visualisieren. Dabei ist die Annahme, dass Sie die erforderlichen Daten
# bereits mit Kapitel_10_Variante_1.py erzeugt haben. Bei dieser Variante 
# haben Sie die Gelegenheit, sich die Relativbewegung der beiden Teilchen 
# anzuschauen.

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.animation import FuncAnimation

data = np.load('Kapitel_10_Positionen.npy')
num_points = data.shape[0]

# Berechnung der Relativbewegung
pos_rel = np.zeros((num_points, 3))
pos_rel[:, :] = data[:, 0, :] - data[:, 1, :]

# 3D-Animation der Relativbewegung
fig = plt.figure("Relativbewegung")
ax = Axes3D(fig)
points, = ax.plot([], [], [], 'ro')
def init():
    ax.set_xlim(-20, 20)                   # Zu zeichnendes x-Intervall
    ax.set_ylim(-20, 20)                   # Zu zeichnendes y-Intervall
    ax.set_zlim(-20, 20)                   # Zu zeichnendes z-Intervall
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.set_zlabel("z")
    return points,
def update(n, pos_rel):
    points.set_data(pos_rel[0:n, 0], pos_rel[0:n, 1])
    points.set_3d_properties(pos_rel[0:n, 2], 'z')
    return points,
ani = FuncAnimation(fig, update, init_func = init, frames = num_points, 
                     interval = 2, fargs = (pos_rel,))

plt.show()
