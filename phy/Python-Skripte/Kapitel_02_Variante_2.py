# Kapitel_02_Variante_2.py
#
# Dieses Python-Skript dient Ihnen dazu, die mathematischen Beispiele aus 
# Kapitel 2 fuer die Kinematik eines Punktteilchens im dreidimensionalen Raum 
# zu visualisieren. In diesem Beispiel wird die Gl. 2.42 umgesetzt.

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.animation import FuncAnimation

num_points = 501

t_min = 0
t_max = 10

t_points = np.linspace(t_min, t_max, num_points)

def get_trajectory(num_points, t_points):
    pos = np.zeros((3, num_points))   
# Eindimensionale Bewegung entlang der x-Achse
# Parameter
    A = 50
    omega = 2*np.pi/2.5
    pos[0,:] = A*np.cos(omega * t_points)
    return pos

pos = get_trajectory(num_points, t_points)

fig = plt.figure()
ax = Axes3D(fig)
point, = ax.plot([], [], [], 'ro')
def init():
    ax.set_xlim(-100, 100)                   
    ax.set_ylim(-100, 100)                   
    ax.set_zlim(-100, 100)
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.set_zlabel("z")
    return point,
def update(n):
    point.set_data(pos[0,n], pos[1,n])       
    point.set_3d_properties(pos[2,n], 'z')   
    return point,
ani = FuncAnimation(fig, update, init_func = init, 
                    frames = num_points, interval = 20)

plt.show()
