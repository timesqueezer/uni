# Kapitel_02_Variante_1.py
#
# Dieses Python-Skript dient Ihnen dazu, die mathematischen Beispiele aus 
# Kapitel 2 fuer die Kinematik eines Punktteilchens im dreidimensionalen Raum 
# zu visualisieren. In diesem Beispiel wird die Gl. 2.39 umgesetzt.

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.animation import FuncAnimation

# num_points: Anzahl der zu betrachtenden Zeitpunkte insgesamt 
# (einschliesslich dem Anfangszeitpunkt)
num_points = 501

# t_min: Anfangszeitpunkt
t_min = 0

#t_max: Endzeitpunkt
t_max = 10

# Erzeugung einer Liste der zu betrachtenden Zeitpunkte. Dabei werden die 
# Punkte mit gleichfoermigem Abstand verteilt.
t_points = np.linspace(t_min, t_max, num_points)

# Definition der Funktion zur Berechnung des Ortsvektors zu den verschiedenen 
# Zeitpunkten bei gegebenen mathematischen Ausdruecken fuer diese Groessen. 
# (Der Nutzer muss diese also vorgeben.)
def get_trajectory(num_points, t_points):
# Initialisierung (Erzeugung entsprechender Listen)
    pos = np.zeros((3, num_points))   # Ortsvektor mit drei Komponenten
# Eindimensionale Bewegung in z-Richtung
# Parameter
    z_0 = 5
    v_0 = 40
    g = 9.81
# Im Folgenden wird das maechtige Werkzeug der "vektorisierten" Berechnung,
# die fuer "numpy arrays" zur Verfuegung steht, ausgenutzt. Auf diese
# Weise erscheint keine explizite Schleife ueber die verschiedenen Zeitpunkte.
# Diese Art der Berechnung ist zeitlich schneller als die Verwendung einer
# Schleife und kann hier daher ausgenutzt werden, weil die Berechnungen zu
# den einzelnen Zeitpunkten voneinander unabhaengig sind (anders als in 
# Kapitel 1).
# Index 0 = x-Komponente, Index 1 = y-Komponente, Index 2 = z-Komponente
    pos[2,:] = z_0 + v_0 * t_points - 0.5 * g * t_points**2  
    return pos

# Tatsaechliche Berechnung der x-, y- und z-Komponenten des Orts-,
# Geschwindigkeits- und Beschleunigungsvektors des Teilchens
# zu den verschiedenen Zeitpunkten.
pos = get_trajectory(num_points, t_points)

# 3D-Animation des Ortsvektors
fig = plt.figure()
ax = Axes3D(fig)
point, = ax.plot([], [], [], 'ro')
def init():
    ax.set_xlim(-100, 100)                   # Zu zeichnendes x-Intervall
    ax.set_ylim(-100, 100)                   # Zu zeichnendes y-Intervall
    ax.set_zlim(-100, 100)                   # Zu zeichnendes z-Intervall
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.set_zlabel("z")
    return point,
def update(n):
    point.set_data(pos[0,n], pos[1,n])       # x- und y-Komponenten des Ortsvektors
    point.set_3d_properties(pos[2,n], 'z')   # z-Komponente des Ortsvektors
    return point,
ani = FuncAnimation(fig, update, init_func = init, 
                    frames = num_points, interval = 20)

plt.show()
