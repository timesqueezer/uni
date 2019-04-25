/**
 * Ein Spielfeld besteht aus neun Positionen. Man kann an den neun Positionen
 * einen der beiden Spieler als Besitzer eintragen und auslesen.
 * 
 * @author Fredrik Winkler
 * @author Axel Schmolitzky
 * @author Petra Becker-Pechau
 * @version 3. Januar 2015
 */
interface Spielfeld
{
    /**
     * Gibt den Besitzer der angegebenen Position auf dem Spielfeld.
     * 
     * @param position 0..8
     * @return 0 (unbesetzt), 1 (Spieler 1), 2 (Spieler 2)
     */
    public int gibBesitzer(int position);

    /**
     * Besetzt die angegebene Position auf dem Spielfeld fuer einen Spieler.
     * 
     * @param position 0..8
     * @param spieler 1 (Spieler 1), 2 (Spieler 2)
     */
    public void besetzePosition(int position, int spieler);

    /**
     * Gibt an, ob das Spielfeld an allen Positionen belegt ist.
     */
    public boolean istVoll();
}
