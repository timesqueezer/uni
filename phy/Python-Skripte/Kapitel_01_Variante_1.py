# Kapitel_01_Variante_1.py
#
# Dieses Python-Skript bestimmt die Zustandsdynamik, die durch 
# Z[n+1] = Z[n] + f(n, Z[n]) [Gl. (1.5) im Lehrbuch] beschrieben wird.
# Als Anfangszeitpunkt wird n = 0 gewaehlt.
# Der Fokus liegt auf der logistischen Gleichung
# Z[n+1] = r*Z[n]*(1-Z[n])
# In dieser ersten Variante wird fuer jeweils feste r die Zeitentwicklung
# berechnet und visualisiert.

import numpy as np
import matplotlib.pyplot as plt

# num_points: Anzahl der zu betrachtenden Zeitpunkte insgesamt 
# (einschliesslich n = 0)
num_points = 101

# Z: Dieses "numpy array" wird hier initialisiert und spaeter mit
# den Z_n (Zustand zum Zeitpunkt n) gefuellt. Z ist eine Liste, die
# aus num_points Eintraegen besteht. Durch den folgenden Befehl wird diese
# Liste erzeugt und mit Nullen gefuellt.
Z = np.zeros(num_points) 

# Festlegung des Anfangszustands Z[0]: Der erste Eintrag der Liste Z hat den
# Index 0 (per Python-Konvention). Durch Z[0] adressieren wir diesen ersten
# Eintrag der Liste und tragen dort den Anfangszustand ein. (Wird als 
# Anfangszustand Z[0] = 0 gewaehlt, ist die folgende Zeile nicht zwingend 
# erforderlich, da wir Z oben mit Nullen gefuellt hatten.)
# Beachten Sie: In Python wird, wie auch sonst in der englischgepraegten
# Programmierwelt, ein Dezimalpunkt, also kein Dezimalkomma, verwendet.
Z[0] = 0.5

# Zusaetzlich muessen wir eine Wahl fuer den Parameter r treffen, der die
# logistische Gleichung charakterisiert. 
r = 3

# In der folgenden Schleife wird schrittweise die Dynamik des Systems
# anhand von Z[n+1] = r*Z[n]*(1-Z[n]) berechnet. Nach Durchlaufen der
# Schleife enthaelt die Liste Z die aufgrund des Anfangszustands und des
# betrachteten dynamischen Gesetzes berechneten Zustaende Z_n zu den
# jeweiligen Zeitpunkten n.
for n in range(num_points-1):
    Z[n+1] = r*Z[n]*(1-Z[n])
    
# Bemerkung: Da es das Element Z[num_points] nicht gibt (es existieren, per 
# Konstruktion, lediglich Z[0], Z[1], ..., Z[num_points-1], also insgesamt 
# num_points Eintraege in der Liste Z), durfte sich die obige Schleife nur 
# ueber num_points-1 Schritte (n = 0, 1, ..., num_points-2) erstrecken. 
    
# Fuer die graphische Darstellung der im vorherigen Schritt berechneten
# Dynamik benoetigen wir nicht nur die Liste der Z_n (die y-Werte fuer die
# graphische Darstellung in einem xy-Koordinatensystem), sondern auch die 
# Liste der entsprechenden n (die x-Werte fuer die graphische Darstellung).
# Diese Liste erzeugen wir durch folgenden Befehl.
n_werte = np.arange(num_points)

# Jetzt haben wir alle Daten zusammen, um uns die berechnete Dynamik 
# graphisch anzuschauen. 
plt.plot(n_werte, Z, ".")
plt.xlabel("n")
plt.ylabel("Z[n]")
plt.show()
