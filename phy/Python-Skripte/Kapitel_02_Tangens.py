# Kapitel_02_Tangens.py
#
# Dieses Python-Skript dient der Visualisierung des Tangens (tan) 
# und dessen Umkehrfunktion, Arkustangens (arctan) im Zusammenhang mit 
# Polarkoordinaten in der Ebene.

import numpy as np
import matplotlib.pyplot as plt

# Erzeugung einer Liste von x-Werten, an denen die jeweilige Funktion 
# ausgewertet werden soll
x = np.linspace(-10, 10, 10000)

# Auswertung der ersten Funktion (hier Tangens) auf dem x-Gitter
"""y = np.tan(x)

# Erzeugung der Abbildung
plt.figure()
plt.plot(x, y, 'r.')
plt.xlim(-np.pi, np.pi)
plt.ylim(-30, 30)
plt.xlabel('$\phi$')
plt.ylabel('$y/x = tan{(\phi)}$')
plt.title('Tangens')

plt.show()

# Konstruktion der Umkehrfunktion
plt.figure()
plt.plot(y, x, 'y.')
y2 = np.arctan(x)
plt.plot(x, y2, 'g')
plt.xlim(-10, 10)
plt.ylim(-np.pi, np.pi)
plt.xlabel('$y/x$')
plt.ylabel('$\phi = arctan(y/x)$')
plt.title('Arkustangens')

plt.show()"""

# Bemerkung: Im Hinblick auf die Konversion von kartesischen Koordinaten (x, y)
# in Polarkoordinaten (r, phi) lohnt es sich, die Verwendung der numpy-Routine 
# np.arctan2 nachzuschauen.

for phi in [0, -np.pi/2, np.pi/2, -np.pi, np.pi]:
    y = np.sin(x+phi)
    y2 = np.cos(x+phi)

    # Erzeugung der Abbildung
    plt.figure()
    plt.plot(x, y, 'r.', linewidth=2)
    plt.plot(x, y2, 'b.', linewidth=2)
    plt.xlim(-2*np.pi, 2*np.pi)
    plt.ylim(-2, 2)
    plt.xlabel('$x$')
    plt.ylabel('$sin(x+\phi), cos(x+\phi)$')
    plt.title('$\phi = {}\pi$'.format(phi/np.pi))

    plt.show()
