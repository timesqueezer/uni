
/**
 * Beschreiben Sie hier die Klasse ArraySpielfeld.
 * 
 * @author Matz Radloff
 * @version 09.01.17
 */
class ArraySpielfeld implements Spielfeld
{
    private int[] positionen; // Arraydeklaration (a)


    public ArraySpielfeld()
    {
        positionen = new int[9]; // Arrayerzeugung (b)
    }
    
    public int gibBesitzer(int position)
    {
        return positionen[position]; // Lesender Arrayzugriff (c)
    }
    
    public void besetzePosition(int position, int spieler)
    {
        positionen[position] = spieler; // Schreibender Arrayzugriff (d)
    }
    
    public boolean istVoll()
    {
        for (int i : positionen) // Lesender Arrayzugriff (c)
        {
            if (i == 0) {
                return false;
            }
        }
        return true;
    }
}

