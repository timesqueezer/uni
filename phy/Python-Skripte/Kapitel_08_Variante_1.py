# Kapitel_08_Variante_1.py
#
# Dieses Python-Skript dient der Visualisierung von Funktionen vom
# Typ V(x, y, z) fuer ein frei gewaehltes, festes z. Auf diese Weise
# erhaelt man als Darstellung der Funktion eine zweidimensionale Flaeche in 
# einem dreidimensionalen Raum mit Koordinatenachsen x, y und V. (Wuerden wir 
# auch z zu den Koordinatenachsen explizit hinzunehmen wollen, dann erhielten 
# wir fuer die betrachtete Funktion ein dreidimensionales Volumen in einem 
# vierdimensionalen Raum mit Koordinatenachsen x, y, z und V.) 

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# In der folgenden Python-Funktion wird die Funktion V(x, y, z)festgelegt. 
def V_function(x, y, z):
    return np.exp(np.sin(x + 2*y + 3*z) + np.cos(3*x + 2*y + z))

# Erzeugung eines fuer die Visualisierung gewuenschten Gitters in der xy-Ebene
num_points = 100
x = np.linspace(-2, 2, num_points)
y = np.linspace(-2, 2, num_points)
X, Y = np.meshgrid(x, y)

# Auswertung der Funktion auf dem xy-Gitter, bei festem z:
z = 1
V = V_function(X, Y, z)

# Erzeugung der Abbildung
fig = plt.figure()
ax = plt.axes(projection="3d")
ax.plot_wireframe(X, Y, V, color='blue')
ax.set_xlabel('x')
ax.set_ylabel('y')
ax.set_zlabel('V')

plt.show()
