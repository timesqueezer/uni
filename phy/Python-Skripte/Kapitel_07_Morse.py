# Kapitel_07_Morse.py
#
# Dieses Python-Skript dient der Visualisierung der "Morse-Kraft"
# zwischen zwei Atomen. 

import numpy as np
import matplotlib.pyplot as plt

# Im Folgenden sei r der Abstand zwischen den beiden Atomen.
r_start = 0
r_stop = 5
num_points = 1000
r_werte = np.linspace(r_start, r_stop, num_points)

# Die Morse-Kraft haengt von drei Parametern ab: D, a, r_e.
D = 3   # Mass fuer die Staerke der Bindung zwischen den beiden Atomen
a = 2   # Mass fuer die Reichweite der Bindung
r_e = 1 # Gleichgewichtsabstand der beiden Atome

# Auswertung der Morse-Kraftfunktion
f_werte = 2*D*a*(np.exp(-2*a*(r_werte-r_e)) - np.exp(-a*(r_werte-r_e)))

# Erzeugung der Abbildung
plt.figure()
plt.plot(r_werte, f_werte)
plt.xlim(r_start, r_stop)
plt.ylim(-D*a, D*a)
plt.xlabel('$r_{ij}$')
plt.ylabel('$f_{ij}$')
plt.title('Morse-Kraft')

plt.show()
