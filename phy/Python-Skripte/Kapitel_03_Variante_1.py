# Kapitel_03_Variante_1.py
#
# Mit Hilfe dieses Skripts koennen Sie die Dynamik eines Punktteilchens
# im dreidimensionalen Raum gemaess der Newton'schen Bewegungsgleichung 
# berechnen und visualisieren. Die Berechnung wird mit Hilfe der 
# Euler-Methode durchgefuehrt, dem einfachsten Verfahren zur numerischen 
# Loesung von (gewoehnlichen) Differentialgleichungen. Zu beachten ist, dass 
# fuer das numerische Verfahren im Allgemeinen deutlich kleinere (und damit 
# insgesamt mehr) Zeitschritte erforderlich sind als fuer die Visualisierung.
#
# In diesem Beispiel betrachten wir eine Kraft, die explizit von der 
# Zeit abhaengt und dabei auch nur von der Zeit abhaengt (und nicht
# vom Ort oder von der Geschwindigkeit des Teilchens).

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.animation import FuncAnimation

# num_steps: Anzahl der Zeitschritte, die zur numerischen Loesung der
# Newton'schen Bewegungsgleichung verwendet werden
num_steps = 50000

# t_min: Anfangszeitpunkt
t_min = 0

#t_max: Endzeitpunkt
t_max = 50

# dt: Laenge eines Zeitschritts fuer die numerische Loesung
dt = (t_max - t_min)/num_steps

# num_points: Anzahl der fuer die Visualisierung zu betrachtenden Zeitpunkte 
# insgesamt (einschliesslich dem Anfangszeitpunkt)
num_points = 501

# delta: Anzahl der numerischen Zeitschritte pro Zeitpunkt fuer
# die Visualisierung. Der Einfachheit halber wird angenommen,
# dass sich bei der folgenden Rechnung eine ganze Zahl ergibt. Wenn dies nicht
# der Fall ist, werden bei der Rechnung weiter unten die Indexgrenzen unter-
# oder ueberschritten (was im zweiten Fall zu einer Fehlermeldung fuehrt).
delta = num_steps/(num_points - 1)

# Anfangsbedingung fuer die Teilchenposition
pos = np.zeros(3)   # Teilchen beginnt im Koordinatenursprung
# Anfangsbedingung fuer die Teilchengeschwindigkeit
vel = np.zeros(3)   # Teilchen startet in Ruhe
t = t_min

# Initialisierung von Liste fuer die Teilchenposition. Diese Liste wird nicht
# fuer die numerische Loesung selbst verwendet, sondern nur fuer die spaetere 
# Visualisierung.
counter = 0
data = np.zeros((num_points, 3))
data[counter, :] = pos            # Anfangsposition
# Initialisierung der dazugehoerigen Zeitpunkte fuer die spaetere 
# Visualisierung:
t_points = np.zeros(num_points)
t_points[counter] = t             # Anfangszeitpunkt

# Funktion zur Berechnung der betrachteten Kraft
# F(t) = F * sin(omega*t)
# entland der x-Achse:
def Kraft(pos, t):
    force = np.zeros(3)
# Parameter
    F = 1
    omega = 1
# Berechnung des gegenwaertigen Kraftvektors
    force[0] = F * np.sin(omega * t)
    return force

# In der folgenden Schleife wird schrittweise, mit Hilfe der Euler-Methode, 
# die Loesung der Newton'schen Bewegungsgleichung berechnet. Die 
# betrachtete Kraft (force) ist in der Funktion "Kraft" definiert und wird
# dort berechnet.
mass = 1
for n in range(num_steps):
# Kraft am Ort des Teilchens zur Zeit t. Diese muss berechnet werden, bevor
# wir die neue Teilchenposition bestimmen.    
    force = Kraft(pos, t)
# Euler-Update des Ortsvektors, also Berechnung des Ortsvektors zur Zeit 
# t+dt aus dem Ortsvektor zur Zeit t, unter Verwendung der Geschwindigkeit
# zur Zeit t:
    pos = pos + vel*dt     # Gl. (3.59)
# Euler-Update des Geschwindigkeitsvektors, unter Verwendung der Kraft
# zur Zeit t    
    vel = vel + (force/mass)*dt   # Gl. (3.56)
    t = t + dt
    if (n+1)%delta == 0:
        counter = counter + 1
        data[counter, :] = pos
        t_points[counter] = t

# Abbildung der drei Ortsvektorkomponenten als Funktion der Zeit.
plt.figure("Ortsvektor als Funktion der Zeit")
plt.subplot(3, 1, 1); plt.plot(t_points, data[:,0]); plt.ylabel("$x$")
plt.subplot(3, 1, 2); plt.plot(t_points, data[:,1]); plt.ylabel("$y$")
plt.subplot(3, 1, 3); plt.plot(t_points, data[:,2]); plt.ylabel("$z$")
plt.xlabel("t")

plt.show()

# 3D-Animation des Ortsvektors
fig = plt.figure()
ax = Axes3D(fig)
point, = ax.plot([], [], [], 'ro')
def init():
    ax.set_xlim(-50, 50)                   # Zu zeichnendes x-Intervall
    ax.set_ylim(-50, 50)                   # Zu zeichnendes y-Intervall
    ax.set_zlim(-50, 50)                   # Zu zeichnendes z-Intervall
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.set_zlabel("z")
    return point,
def update(n):
    point.set_data(data[n,0], data[n,1])     # x- und y-Komponenten des Ortsvektors
    point.set_3d_properties(data[n,2], 'z')   # z-Komponente des Ortsvektors
    return point,
ani = FuncAnimation(fig, update, init_func = init, 
                    frames = num_points, interval = 20)

plt.show()
