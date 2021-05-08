# Kapitel_02_Variante_4.py
#
# Dieses Python-Skript dient Ihnen dazu, die mathematischen Beispiele aus 
# Kapitel 2 fuer die Kinematik eines Punktteilchens im dreidimensionalen Raum 
# zu visualisieren. In diesem Beispiel modifizieren wir die Gl. 2.47 etwas,
# um weitere zweidimensionale Bewegungstypen zu erhalten.

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
# Bewegung in der xy-Ebene
# Parameter
    R = 50
    omega = 2*np.pi/2.5
    alpha = np.sqrt(2)
    phi = 0
    pos[0,:] = R*np.cos(omega * t_points)
    pos[1,:] = R*np.sin(alpha * omega * t_points + phi)
    return pos

pos = get_trajectory(num_points, t_points)

fig = plt.figure()
ax = Axes3D(fig)
line, = ax.plot([], [], [], 'ro')
def init():
    ax.set_xlim(-100, 100)                   
    ax.set_ylim(-100, 100)                   
    ax.set_zlim(-100, 100)
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.set_zlabel("z")
    return line,
def update(n):
    line.set_data(pos[0,0:n], pos[1,0:n])       
    line.set_3d_properties(pos[2,0:n], 'z')   
    return line,
ani = FuncAnimation(fig, update, init_func = init, 
                    frames = num_points, interval = 20)

plt.show()
