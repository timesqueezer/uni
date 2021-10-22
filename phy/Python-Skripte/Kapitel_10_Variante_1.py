# Kapitel_10_Variante_1.py
#
# Mit Hilfe dieses Skripts koennen Sie die Dynamik von zwei gravitativ 
# miteinander wechselwirkenden Punktteilchen im dreidimensionalen Raum 
# gemaess der Newton'schen Bewegungsgleichung berechnen. Die Visualisierung
# der Teilchendynamik erfolgt in den folgenden Programmen. Zu diesem Zweck
# werden relevante Ergebnisse der Rechnung am Ende in eine Datei geschrieben.
# 
# Da es hier nur um zwei Teilchen geht, ist es fuer Sie als Nutzer leicht, 
# die Anfangsbedingungen und die Teilchenmassen per Hand einzutragen. 
# Das Skript ist aber so gehalten, dass es leicht ist, auch mehr als zwei 
# Teilchen zu untersuchen. 
# 
# In diesem Skript wird ausserdem der Zeitverlauf der gesamten kinetischen 
# Energie, der gesamten potentiellen Energie und der Gesamtenergie (= gesamte 
# kinetische Energie plus gesamte potentielle Energie) aller Teilchen 
# berechnet. Da es sich um ein abgeschlossenes System handelt, bei dem sich 
# die Kraefte der Teilchen aufeinander mit Hilfe eines Potentials 
# ausdruecken lassen, muss die Gesamtenergie grundsaetzlich erhalten sein. Da 
# wir aber die Bewegungsgleichungen numerisch integrieren, kommt es zu 
# Ungenauigkeiten. Diese Ungenauigkeiten haengen zum einen damit zusammen, dass 
# wir mit diskreten Zeitschritten arbeiten (Diskretisierungsfehler), und zum 
# anderen, dass die in der Rechnung auftretenden Zahlen nicht mit beliebig 
# hoher Genauigkeit im Rechner dargestellt werden koennen (Rundungsfehler). 
# Untersuchen Sie mit diesem Skript unter anderem, wie sich die numerisch 
# berechnete Gesamtenergie als Funktion der Zeit veraendert, wenn Sie die 
# Anzahl der Zeitschritte fuer die numerische Loesung der Newton'schen 
# Bewegungsgleichung systematisch veraendern. (Die langsame Konvergenz der
# numerischen Loesung mit zunehmender Anzahl von Zeitschritten ist der Grund,
# weshalb es in der Praxis unueblich ist, mit der Euler-Methode zu arbeiten.)

import numpy as np
import matplotlib.pyplot as plt

# num_steps: Anzahl der Zeitschritte, die zur numerischen Loesung der
# Newton'schen Bewegungsgleichung verwendet werden
num_steps = 1000000

# t_min: Anfangszeitpunkt
t_min = 0

# t_max: Endzeitpunkt
t_max = 200

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
num_part = 2

# mass: Liste der beiden Teilchenmassen
mass = np.zeros(num_part)
mass[0] = 1     # Masse von Teilchen 1
mass[1] = 5     # Masse von Teilchen 2

# Anfangsbedingung fuer die Teilchenpositionen
pos = np.zeros((num_part, 3))
pos[0, :] = [5, 0, 0]  # Ortsvektor von Teilchen 1
pos[1, :] = [0, 0, 0]  # Ortsvektor von Teilchen 2
# Bemerkung: Um Trends zu erkennen, variieren Sie beim numerischen 
# Experimentieren nie mehr als einen Parameter!

# Anfangsbedingung fuer die Teilchengeschwindigkeitsvektoren
vel = np.zeros((num_part, 3))
vel[0, :] = [0, 1.4, 0]    # Geschwindigkeitsvektor von Teilchen 1
vel[1, :] = [0, 0, 0]      # Geschwindigkeitsvektor von Teilchen 2

# Funktion zur Berechnung der gesamten kinetischen Energie, der gesamten 
# potentiellen Energie und der Gesamtenergie von allen Teilchen. 
def Energie(num_part, mass, pos, vel):
    e_kin, e_pot, e_tot = 0, 0, 0
# Gravitationskonstante
    G = 1
    for i in range(num_part):
        # Kinetische Energie:
        e_kin += 0.5 * mass[i] * np.linalg.norm(vel[i, :])**2
        # Potentielle Energie:
        for j in range(i+1, num_part):
            dist_ij = np.linalg.norm(pos[i, :] - pos[j, :])   # Teilchenabstand
            e_pot += -G * mass[i] * mass[j] / dist_ij
    e_tot = e_kin + e_pot
    return e_kin, e_pot, e_tot

# Initialisierung der Listen fuer die Teilchenpositionen und die verschiedenen
# Energien. Diese Listen werden nicht fuer die numerische Loesung selbst 
# verwendet, sondern nur fuer die spaetere Visualisierung.
counter = 0
data = np.zeros((num_points, num_part, 3))
data[counter, :, :] = pos            # Anfangspositionen
e_kin = np.zeros(num_points)
e_pot = np.zeros(num_points)
e_tot = np.zeros(num_points)
e_kin[counter], e_pot[counter], e_tot[counter] = \
    Energie(num_part, mass, pos, vel)

t = t_min

# Initialisierung der Zeitpunkte fuer die spaetere Visualisierung:
t_points = np.zeros(num_points)
t_points[counter] = t             # Anfangszeitpunkt

# Funktion zur Berechnung der Gravitationskraft, die das j-te Teilchen 
# (Ortsvektor pos_j und Masse mass_j) auf das i-te Teilchen (Ortsvektor pos_i
# und Masse mass_i) ausuebt. 
def Kraft(pos_i, mass_i, pos_j, mass_j):
# Gravitationskonstante
    G = 1
# Berechnung des Kraftvektors unter Verwendung des Newton'schen 
# Gravitationsgesetzes fuer zwei Teilchen im dreidimensionalen Raum
    diff_ij = pos_i - pos_j  # Differenzvektor der beiden Teilchenpositionen
    dist_ij = np.linalg.norm(diff_ij)   # Teilchenabstand
    force = -G * mass_i * mass_j * diff_ij / dist_ij**3
    # Abstand hoch drei ist nicht falsch. Ueberlegen Sie sich, weshalb.
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
        mass_i = mass[i]
        for j in range(num_part):
            if j != i:
                pos_j = pos[j, :]
                mass_j = mass[j]
                acc[i, :] += Kraft(pos_i, mass_i, pos_j, mass_j)
        acc[i, :] = acc[i, :] / mass_i        
# Euler-Update aller Ortsvektoren
    pos = pos + vel*dt
# Euler-Update aller Geschwindigkeitsvektoren    
    vel = vel + acc*dt
    t = t + dt
    if (n+1)%delta == 0:
        counter = counter + 1
        data[counter, :, :] = pos
        e_kin[counter], e_pot[counter], e_tot[counter] = \
            Energie(num_part, mass, pos, vel)
        t_points[counter] = t

# Abbildung der gesamten kinetischen Energie, der gesamten potentiellen
# Energie und der Summe der beiden (= Gesamtenergie) als Funktion der Zeit.
plt.figure("Kinetische, potentielle und Gesamtenergie als Funktion der Zeit")
plt.subplot(3, 1, 1); plt.plot(t_points, e_kin); plt.ylabel("$E_{\mathrm{kin}}$")
plt.subplot(3, 1, 2); plt.plot(t_points, e_pot); plt.ylabel("$E_{\mathrm{pot}}$")
plt.subplot(3, 1, 3); plt.plot(t_points, e_tot); plt.ylabel("$E_{\mathrm{tot}}$")
plt.xlabel("t")

plt.show()

# Rausschreiben der Teilchenmassen und -positionen in Dateien:
np.save('Kapitel_10_Massen', mass)
np.save('Kapitel_10_Positionen', data)