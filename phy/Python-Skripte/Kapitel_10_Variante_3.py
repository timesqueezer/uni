# Kapitel_10_Variante_3.py
#
# Mit Hilfe dieses Skripts koennen Sie die Dynamik von zwei gravitativ 
# miteinander wechselwirkenden Punktteilchen im dreidimensionalen Raum 
# visualisieren. Dabei ist die Annahme, dass Sie die erforderlichen Daten
# bereits mit Kapitel_10_Variante_1.py erzeugt haben. Bei dieser Variante 
# haben Sie die Gelegenheit, sich die Bewegung des Schwerpunkts der beiden 
# Teilchen anzuschauen.

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.animation import FuncAnimation

data = np.load('Kapitel_10_Positionen.npy')
mass = np.load('Kapitel_10_Massen.npy')
num_points = data.shape[0]
num_part = data.shape[1]

# Berechnung der Schwerpunktsbewegung
mass_total = 0
pos_CM = np.zeros((num_points, 3))
for i in range(num_part):
    mass_total += mass[i]  
    pos_CM[:, :] += mass[i]*data[:, i, :]
pos_CM = pos_CM/mass_total

# 3D-Animation der Schwerpunktsbewegung
fig = plt.figure("Schwerpunktsbewegung")
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
def update(n, pos_CM):
    points.set_data(pos_CM[0:n, 0], pos_CM[0:n, 1])
    points.set_3d_properties(pos_CM[0:n, 2], 'z')
    return points,
ani = FuncAnimation(fig, update, init_func = init, frames = num_points, 
                     interval = 2, fargs = (pos_CM,))

plt.show()