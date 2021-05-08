# Kapitel_08_Variante_3.py
#
# Dieses Python-Skript dient der Visualisierung von Funktionen vom
# Typ V(x, y, z) fuer ein frei gewaehltes, festes x und ein freigewaehltes,
# festes z. Auf diese Weise erhaelt man als Darstellung der Funktion eine 
# eindimensionale Kurve in einer zweidimensionalen Ebene mit Koordinatenachsen 
# y und V. 

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# In der folgenden Python-Funktion wird die Funktion V(x, y, z)festgelegt. 
def V_function(x, y, z):
    return np.exp(np.sin(x + 2*y + 3*z) + np.cos(3*x + 2*y + z))
#    return np.exp(np.sin(x + 2*y + 3*z) + np.cos(3*x*y + 2*x*z + y*z))

# Erzeugung eines fuer die Visualisierung gewuenschten Gitters entlang der 
# y-Achse
num_points = 10000
y = np.linspace(-2, 2, num_points)

# Auswertung der Funktion auf dem y-Gitter, bei festen x und z:
x = 0
z = 0
V = V_function(x, y, z)

# Erzeugung der Abbildung
plt.figure()
plt.plot(y, V)
plt.xlabel('y')
plt.ylabel('V')

plt.show()
