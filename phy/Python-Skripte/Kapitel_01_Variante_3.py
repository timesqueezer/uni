# Kapitel_01_Variante_3.py
#
# Dieses Python-Skript bestimmt die Zustandsdynamik, die durch 
# Z[n+1] = Z[n] + f(n, Z[n]) [Gl. (1.5) im Lehrbuch] beschrieben wird.
# Als Anfangszeitpunkt wird n = 0 gewaehlt.
# Der Fokus liegt auf der logistischen Gleichung
# Z[n+1] = r*Z[n]*(1-Z[n])
# Auch in dieser dritten Variante wird r mit Hilfe einer Schleife automatisch 
# variiert, die Zeitentwicklung berechnet und unter Verwendung einer 
# Animation visualisiert. Dabei wird bei jedem r der gleiche Anfangszustand 
# verwendet. In der Animation entspricht jedes einzelne Bild einem Zeitpunkt n,
# der im Laufe der Animation durchlaufen wird.

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

# Wir muessen festlegen, ueber welches Intervall sich die r-Werte
# erstrecken sollen.
r_start = 0.0
r_stop = 4.0
# Anzahl der zu betrachtenden r-Werte insgesamt:
num_r = 1000
# Erzeugung der Liste der r-Werte:
r_werte = np.linspace(r_start, r_stop, num_r)
 
# num_points: Anzahl der bei jedem r zu betrachtenden Zeitpunkte insgesamt 
# (einschliesslich n = 0)
num_points = 101

# Z: Dieses "numpy array" wird hier initialisiert und spaeter mit
# den Z_n (Zustand zum Zeitpunkt n) fuer jedes r gefuellt. 
Z = np.zeros((num_r, num_points)) 

# Aeussere Schleife ueber die r-Werte
for i in range(num_r):
# Festlegung des Anfangszustands Z[0] beim gegenwaertigen r:
    Z[i,0] = 0.5
# Berechnung der Dynamik des Systems beim gegenwaertigen r:
    for n in range(num_points-1):
        Z[i,n+1] = r_werte[i]*Z[i,n]*(1-Z[i,n])
        
# Konstruktion der Animation (sehen Sie dazu auch
# https://matplotlib.org/examples/animation/index.html)

n_werte = np.arange(num_points)

fig = plt.figure()
ax = plt.axes(xlim=(r_start, r_stop), ylim=(0, 1))
ax.set_xlabel('$r$')
ax.set_ylabel('$Z$')
line, = ax.plot([], [], ".")

def init():
    line.set_data([], [])
    return line,

def update(n):
    line.set_data(r_werte, Z[:,n])
    return line,

ani = animation.FuncAnimation(fig, update, init_func=init,
                               frames=num_points, interval=200)

plt.show()