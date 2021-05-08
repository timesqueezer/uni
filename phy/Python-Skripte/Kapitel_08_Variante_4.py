# Kapitel_08_Variante_4.py
#
# Dieses Python-Skript ist identisch zu Kapitel_08_Variante_2.py, nur dass
# hier die Animation bzgl. eines stationaeren Punkts zentriert ist.

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# In der folgenden Python-Funktion wird die Funktion V(x, y, z)festgelegt. 
def V_function(x, y, z):
    return np.exp(np.sin(x + 2*y + 3*z) + np.cos(3*x + 2*y + z))

# Erzeugung eines fuer die Visualisierung gewuenschten Gitters in der xy-Ebene
num_points = 100
x_start = 1+3*np.pi/4 - 0.2
x_stop =  1+3*np.pi/4 + 0.2
y_start = -2+3*np.pi/8 - 0.2
y_stop =  -2+3*np.pi/8 + 0.2
x = np.linspace(x_start, x_stop, num_points)
y = np.linspace(y_start, y_stop, num_points)
X, Y = np.meshgrid(x, y)

# Erzeugung des Gitters fuer die z-Koordinate
z_start = 1 - 0.2
z_stop = 1 + 0.2
z = np.linspace(z_start, z_stop, num_points)

V_start = 0
V_stop = np.exp(2)

# Erzeugung der Animation
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.set_xlim(x_start, x_stop)
ax.set_ylim(y_start, y_stop)
ax.set_zlim(V_start, V_stop)
ax.set_xlabel("x")
ax.set_ylabel("y")
ax.set_zlabel("V")

wframe = None
for n in range(num_points):
    if wframe:
        ax.collections.remove(wframe)
    V = V_function(X, Y, z[n])    
    wframe = ax.plot_wireframe(X, Y, V, color='blue') 
    plt.pause(0.2)

plt.show()