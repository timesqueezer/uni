# Kapitel_07_Variante_3.py
#
# Mit Hilfe dieses Skripts koennen Sie die Dynamik des Stosses eines diatomaren
# Molekuels mit einem diatomaren Molekuel im dreidimensionalen Raum gemaess der 
# Newton'schen Bewegungsgleichung berechnen und visualisieren. Insbesondere 
# koennen Sie den Gesamtimpuls bestimmen und sich auch die 
# Gesamtgeschwindigkeit anschauen. Die Kraft zwischen den jeweiligen Atompaaren
# (Sie haben hier insgesamt sechs solcher Paare) ist von dem sogenannten 
# Morse-Potential abgeleitet.

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.animation import FuncAnimation

# num_steps: Anzahl der Zeitschritte, die zur numerischen Loesung der
# Newton'schen Bewegungsgleichung verwendet werden
num_steps = 10000

# t_min: Anfangszeitpunkt
t_min = 0

# t_max: Endzeitpunkt
t_max = 20

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

# num_part: Anzahl der zu betrachtenden Teilchen
num_part = 4

# mass: Liste der Teilchenmassen 
mass = np.zeros(num_part)
mass[0] = 1     # Masse des ersten Teilchens (Projektil)
mass[1] = 1     # Masse des zweiten Teilchens (auch Projektil)
mass[2] = 2     # Masse des dritten Teilchens (Target)
mass[3] = 3     # Masse des vierten Teilchens (auch Target)

# Anfangsbedingung fuer die Teilchenpositionen
pos = np.zeros((num_part, 3))   # Drei Komponenten pro Teilchen
pos[0,:] = [-10,-0.5,0]
pos[1,:] = [-9,-0.5,0]
# Die Teilchen 1 und 2, die das Projektilmolekuel bilden, befinden sich im 
# Gleichgewichtsabstand r_e zueinander. Die beiden bilden ein gebundenes 
# Molekuel. Teilchen 3 (Target) lassen wir hier einfach im Ursprung sitzen.
pos[3,:] = [0,0,1]
# Teilchen 4 (auch Target) sitzt im Gleichgewichtsabstand r_e relativ zu
# Teilchen 3. Die beiden bilden ein zweites gebundenes Molekuel.

# Anfangsbedingung fuer die Teilchengeschwindigkeitsvektoren
vel = np.zeros((num_part, 3))
vel[0,:] = [2,0,0]
vel[1,:] = [2,0,0]
# Indem die Teilchen 1 und 2 jeweils den gleichen Geschwindigkeitsvektor haben,
# bewegen sie sich gemeinsam auf das Targetmolekuel zu.
# (Teilchen 3 und 4 sind in Ruhe.)

t = t_min

# Initialisierung der Liste fuer die Teilchenpositionen. Diese Liste wird nicht
# fuer die numerische Loesung selbst verwendet, sondern nur fuer die spaetere 
# Visualisierung.
counter = 0
data = np.zeros((num_points, num_part, 3))
data[counter, :, :] = pos            # Anfangspositionen

# Initialisierung der Liste fuer den Gesamtgeschwindigkeits- und den 
# Gesamtimpulsvektor. (Bemerkung: "Impuls" heisst auf Englisch "momentum".)
# Auch diese Listen werden nur fuer die spaetere Visualisierung verwendet.
vel_total = np.zeros((num_points, 3))
mom_total = np.zeros((num_points, 3))
for i in range(num_part):
    vel_total[counter, :] += vel[i, :]  # Summe der Teilchengeschwindigkeiten 
    mom_total[counter, :] += mass[i] * vel[i, :]  # Summe der Einzelimpulse
    
# Initialisierung der dazugehoerigen Zeitpunkte fuer die spaetere 
# Visualisierung:
t_points = np.zeros(num_points)
t_points[counter] = t             # Anfangszeitpunkt

# Funktion zur Berechnung der Kraft, die das j-te Teilchen (Ortsvektor pos_j)
# auf das i-te Teilchen (Ortsvektor pos_i) ausuebt. Der Einfachheit halber
# wird die Annahme gemacht, dass die Kraftparameter fuer alle Teilchenpaare
# jeweils gleich sind. Dies koennen Sie natuerlich gerne aendern.
def Kraft(pos_i, pos_j):
    D = 3
    a = 2
    r_e = 1
# Berechnung des Kraftvektors unter Verwendung der Morse-Kraft fuer
# zwei Teilchen im dreidimensionalen Raum
    diff = pos_i - pos_j  # Differenzvektor der beiden Teilchenpositionen
    dist = np.linalg.norm(diff)   # Teilchenabstand
    force = 2*D*a*(np.exp(-2*a*(dist-r_e)) - np.exp(-a*(dist-r_e))) \
            * diff/dist
    return force

# In der folgenden Schleife wird schrittweise, mit Hilfe der Euler-Methode, 
# die Loesung der Newton'schen Bewegungsgleichung berechnet. Die 
# betrachtete Kraft (force), die das j-te Teilchen auf das i-te Teilchen 
# ausuebt, ist in der Funktion "Kraft" definiert und wird dort berechnet.
for n in range(num_steps):
# In der folgenden Doppelschleife wird fuer jedes Teilchen die gesamte 
# Beschleunigung aufgrund der anderen Teilchen zur Zeit t berechnet. 
# Diese muss berechnet werden, bevor wir die neuen Teilchenpositionen 
# bestimmen.
    acc = np.zeros((num_part, 3))
    for i in range(num_part):
        pos_i = pos[i, :]
        for j in range(num_part):
            if j != i:
                pos_j = pos[j, :]
                acc[i, :] += Kraft(pos_i, pos_j)
        acc[i, :] = acc[i, :] / mass[i]        
# Euler-Update aller Ortsvektoren
    pos = pos + vel*dt
# Euler-Update aller Geschwindigkeitsvektoren    
    vel = vel + acc*dt
    t = t + dt
    if (n+1)%delta == 0:
        counter = counter + 1
        data[counter, :, :] = pos
        for i in range(num_part):
            vel_total[counter, :] += vel[i, :]  
            mom_total[counter, :] += mass[i] * vel[i, :]
        t_points[counter] = t        

# 3D-Animation der Ortsvektoren der Teilchen
fig = plt.figure()
ax = Axes3D(fig)
points, = ax.plot([], [], [], 'ro')
def init():
    ax.set_xlim(-10, 10)                   # Zu zeichnendes x-Intervall
    ax.set_ylim(-10, 10)                   # Zu zeichnendes y-Intervall
    ax.set_zlim(-10, 10)                   # Zu zeichnendes z-Intervall
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.set_zlabel("z")
    return points,
def update(n, data):
    points.set_data(data[n, :, 0], data[n, :, 1])
    points.set_3d_properties(data[n, :, 2], 'z')
    return points,
ani = FuncAnimation(fig, update, init_func = init, 
                    frames = num_points, interval = 20, fargs = (data,))

plt.show()

# Abbildung der drei Komponenten des Gesamtgeschwindigkeitsvektors
# als Funktion der Zeit.
plt.figure("Gesamtgeschwindigkeitsvektor als Funktion der Zeit")
plt.subplot(3, 1, 1); plt.plot(t_points, vel_total[:, 0]); plt.ylabel("$V_x$")
plt.subplot(3, 1, 2); plt.plot(t_points, vel_total[:, 1]); plt.ylabel("$V_y$")
plt.subplot(3, 1, 3); plt.plot(t_points, vel_total[:, 2]); plt.ylabel("$V_z$")
plt.xlabel("t")

plt.show()

# Abbildung der drei Komponenten des Gesamtimpulsvektors
# als Funktion der Zeit.
plt.figure("Gesamtimpulsvektor als Funktion der Zeit")
plt.subplot(3, 1, 1); plt.plot(t_points, mom_total[:, 0]); plt.ylabel("$P_x$")
plt.subplot(3, 1, 2); plt.plot(t_points, mom_total[:, 1]); plt.ylabel("$P_y$")
plt.subplot(3, 1, 3); plt.plot(t_points, mom_total[:, 2]); plt.ylabel("$P_z$")
plt.xlabel("t")

plt.show()
