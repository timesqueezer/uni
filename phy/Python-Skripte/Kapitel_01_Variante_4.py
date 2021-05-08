# Kapitel_01_Variante_4.py
#
# Dieses Python-Skript bestimmt die Zustandsdynamik, die durch 
# Z[n+1] = Z[n] + f(n, Z[n]) [Gl. (1.5) im Lehrbuch] beschrieben wird.
# Als Anfangszeitpunkt wird n = 0 gewaehlt.
# Der Fokus liegt auf der logistischen Gleichung
# Z[n+1] = r*Z[n]*(1-Z[n])
# In dieser vierten Variante wird r mit Hilfe einer Schleife automatisch 
# variiert und die Zeitentwicklung berechnet. Es wird aber keine Animation
# erstellt, sondern eine Abbildung, in der fuer jedes r die Z-Werte gezeigt 
# sind, die den Zeitpunkten zwischen n_min und n_max entsprechen.

import numpy as np
import matplotlib.pyplot as plt

r_start = 1.0
r_stop = 4.0
num_r = 10000
r_werte = np.linspace(r_start, r_stop, num_r)
 
num_points = 1001

Z = np.zeros((num_r, num_points)) 

for i in range(num_r):
    Z[i,0] = 0.5
    for n in range(num_points-1):
        Z[i,n+1] = r_werte[i]*Z[i,n]*(1-Z[i,n])
        
# Wir plotten im Folgenden den Zeitverlauf zwischen n = n_min und n = n_max.
n_min = int(0.8*num_points)
n_max = num_points
for n in range(n_min, n_max):
    plt.plot(r_werte, Z[:,n], ".", ms=2)
        
plt.xlabel("r")
plt.ylabel("Z")
plt.show()