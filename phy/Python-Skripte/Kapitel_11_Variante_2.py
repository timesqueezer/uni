# Kapitel_11_Variante_2.py
#
# Bei dieser Variante des Python-Skripts zu Kapitel 11 werden am Anfang, nach
# der Konstruktion des Traegheitstensors, die Haupttraegheitsachsen und
# Haupttraegheitsmomente berechnet. Dann werden die Koordinatenachsen
# nach diesen anfaenglichen Haupttraegheitsachsen ausgerichtet, so wie in 
# Abschnitt 11.7 angenommen. Ansonsten ist alles wie gehabt. Mit Hilfe dieses 
# Skripts koennen Sie untersuchen, wie die Drehbewegung aussieht, wenn Sie den 
# anfaenglichen Winkelgeschwindigkeitsvektor entlang einer der 
# Haupttraegheitsachsen ausrichten.

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.animation import FuncAnimation

# num_steps: Anzahl der Zeitschritte, die zur numerischen Loesung der
# Bewegungsgleichung 11.85 verwendet werden
num_steps = 1000000

# t_min: Anfangszeitpunkt
t_min = 0

# t_max: Endzeitpunkt
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

# num_part: Anzahl der zu betrachtenden Teilchen
num_part = 9

# Teilchenpositionen
pos = np.zeros((num_part, 3))
pos[0, :] = [0.01247, 0.02254, 1.08262]     # C-Atom
pos[1, :] = [-0.00894, -0.01624, -0.43421]  # C-Atom
pos[2, :] = [-0.49334, 0.93505, 1.44716]    # H-Atom
pos[3, :] = [1.05522, 0.04512, 1.44808]     # H-Atom
pos[4, :] = [-0.64695, -1.12346, 2.54219]   # H-Atom
pos[5, :] = [0.50112, -0.91640, -0.80440]   # H-Atom
pos[6, :] = [0.49999, 0.86726, -0.84481]    # H-Atom
pos[7, :] = [-1.04310, -0.02739, -0.80544]  # H-Atom
pos[8, :] = [-0.66442, -1.15471, 1.56909]   # O-Atom

# Teilchenmassen
mass = np.zeros(num_part)
mass[0] = 12
mass[1] = 12
mass[2] = 1
mass[3] = 1
mass[4] = 1
mass[5] = 1
mass[6] = 1
mass[7] = 1
mass[8] = 16

# Verschiebung der Teilchen, so dass der resultierende 
# Schwerpunkt gleich dem Nullvektor ist:
mass_total = 0
pos_CM = np.zeros(3)
for i in range(num_part):
    mass_total += mass[i]   
    pos_CM[:] += mass[i]*pos[i, :]
pos_CM = pos_CM/mass_total
for i in range(num_part):
    pos[i, :] = pos[i, :] - pos_CM[:]

# Berechnung von I(0):
I_matrix = np.zeros((3, 3))
for i in range(num_part):
    I_matrix += mass[i]*(np.dot(pos[i, :], pos[i, :])*np.identity(3) \
    - np.outer(pos[i, :], pos[i, :]))

# Wir loesen nun das Eigenwertproblem fuer den Traegheitstensor (Gl. 11.84):
eigenvals, eigenvecs = np.linalg.eigh(I_matrix)    
# Die Liste "eigenvals" enthaelt nun die Haupttraegheitsmomente, nach
# aufsteigenden Werten geordnet. Die dazugehoerigen Eigenvektoren (die zu den 
# Haupttraegheitsachsen gehoerigen, normierten Richtungsvektoren) stehen nun
# in den Spalten von "eigenvecs".

# Wir stellen die Ortsvektoren der Teilchen nun in der Orthonormalbasis der
# neuen Eigenvektoren dar, d.h. wir richten die Koordinatenachsen von nun an 
# so aus, dass sie mit den Haupttraegheitsachsen, die wir gerade bestimmt 
# haben, zusammenfallen.
for i in range(num_part):
    pos[i, :] = np.matmul(np.transpose(eigenvecs), pos[i, :])
# Hier wird letzten Endes die Gl. 2.16 verwendet.    
        
# Bezueglich der neuen Orthonormalbasis ist der Traegheitstensor eine
# Diagonalmatrix (Gl. 11.82).
I_matrix = np.array([[eigenvals[0], 0, 0], \
                     [0, eigenvals[1], 0], \
                     [0, 0, eigenvals[2]]])
    
# Ab jetzt ist alles wie gehabt.
# Inverse des Traegheitstensors:     
I_inverse = np.linalg.inv(I_matrix)

# Anfangsbedingung fuer den Winkelgeschwindigkeitsvektor omega
omega = np.array([0, 0, 1])   # Anfaengliche Drehung um die z-Achse,
                              # d.h. um die Haupttraegheitsachse, die dem
                              # groessten Haupttraegheitsmoment entspricht.

# Berechnung des zeitlich konstanten Drehimpulsvektors mit Hilfe des 
# Traegheitstensors und des Winkelgeschwindigkeitsvektors zum Anfangszeitpunkt
L_rel = np.matmul(I_matrix, omega)  # Gl. 11.67

# Zum Anfangszeitpunkt ist die Drehmatrix R gleich der 3x3-Einheitsmatrix:
R_matrix = np.identity(3)

t = t_min

# Initialisierung der Liste fuer die Teilchenpositionen. Diese Liste wird nicht
# fuer die numerische Loesung selbst verwendet, sondern nur fuer die spaetere 
# Visualisierung.
counter = 0
data = np.zeros((num_points, num_part, 3))
data[counter, :, :] = pos            # Anfangspositionen
# Zur Ueberpruefung der Orthogonalitaet der berechneten Drehmatrix
test_ortho = np.zeros(num_points)

# Initialisierung der dazugehoerigen Zeitpunkte fuer die spaetere 
# Visualisierung:
t_points = np.zeros(num_points)
t_points[counter] = t             # Anfangszeitpunkt

# In der folgenden Schleife wird schrittweise, mit Hilfe der Euler-Methode, 
# die Loesung von Gl. 11.85 berechnet. Die zu jedem Zeitpunkt erforderliche 
# Matrix Omega in Gl. 11.85 wird unter Verwendung von Gl. 11.60 aus dem 
# Winkelgeschwindigkeitsvektor omega berechnet.
for n in range(num_steps):
# Zuerst Konstruktion von omega anhand von Gl. 11.88
    omega = np.matmul(np.transpose(R_matrix), L_rel)
    omega = np.matmul(I_inverse, omega)
    omega = np.matmul(R_matrix, omega)
# Dann Konstruktion von Omega anhand von Gl. 11.60
    Omega_matrix = np.array([[0, -omega[2], omega[1]], \
                            [omega[2], 0, -omega[0]], \
                            [-omega[1], omega[0], 0]])
# Euler-Update der Drehmatrix unter Verwendung von Gl. 11.85
    R_matrix = R_matrix + np.matmul(Omega_matrix, R_matrix)*dt
    t = t + dt
    if (n+1)%delta == 0:
        counter = counter + 1
        for i in range(num_part):
            data[counter, i, :] = np.matmul(R_matrix, pos[i, :]) # Gl. 11.42
            test_ortho[counter] = \
            np.linalg.norm(np.matmul(np.transpose(R_matrix), R_matrix) \
                                    - np.identity(3))
        t_points[counter] = t

plt.figure()
plt.plot(t_points, test_ortho)
plt.xlabel('t')
plt.ylabel('$||R^T R - \mathrm{id}||$')
plt.title('Test auf Orthogonalitaet der Drehmatrix')

plt.show()

# 3D-Animation der Ortsvektoren der Teilchen
fig = plt.figure()
ax = Axes3D(fig)
points, = ax.plot([], [], [], 'ro')
def init():
    ax.set_xlim(-2, 2)                   # Zu zeichnendes x-Intervall
    ax.set_ylim(-2, 2)                   # Zu zeichnendes y-Intervall
    ax.set_zlim(-2, 2)                   # Zu zeichnendes z-Intervall
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.set_zlabel("z")
    return points,
def update(n, data):
    points.set_data(data[n, :, 0], data[n, :, 1])
    points.set_3d_properties(data[n, :, 2], 'z')
    return points,
ani = FuncAnimation(fig, update, init_func = init, frames = num_points, 
                    interval = 20, fargs = (data,))

plt.show()
