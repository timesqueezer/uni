# Kapitel_02_Taylor.py
#
# Dieses Python-Skript dient der Motivation der Taylorreihe. Dabei wird 
# der Sinus in der Umgebung der Stelle x0 = 1 als Beispiel verwendet.

import numpy as np
import matplotlib.pyplot as plt

x0 = 1
x_min = x0 - 1
x_max = x0 + 1
num_points = 1000
x = np.linspace(x_min, x_max, num_points)

# Die zu betrachtende Funktion:
y1 = np.sin(x)
# Taylorentwicklung dieser Funktion bis zur ersten Ordnung (lineares Polynom)
# bezueglich des Bezugspunktes x0:
y2 = np.sin(x0) + np.cos(x0) * (x-x0)
# Taylorentwicklung bis zur zweiten Ordnung (quadratisches Polynom):
y3 = y2 - 0.5 * np.sin(x0) * (x-x0)**2

plt.figure()
plt.plot(x, y1, 'r', ms = 2, label = '$y = sin(x)$')
plt.plot(x, y2, 'b', label = 'Taylorentwicklung erster Ordnung')
plt.plot(x, y3, 'g', label = 'Taylorentwicklung zweiter Ordnung')
plt.xlabel('$x$')
plt.ylabel('$y$')

plt.legend() 
plt.show()

