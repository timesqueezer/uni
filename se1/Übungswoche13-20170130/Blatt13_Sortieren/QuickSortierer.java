/**
 * Sortiert eine InPlaceSortierbareIntListe mit Quicksort.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
class QuickSortierer implements Sortierer
{
    /**
     * Sortiere die angegebene InPlaceSortierbareIntListe aufsteigend in situ.
     * 
     * @param liste die zu sortierende Liste
     */
    public void sortiere(InPlaceSortierbareIntListe liste)
    {
        sortiere(liste, 0, liste.gibLaenge() - 1);
    }
    
    /**
     * Sortiere den Ausschnitt der InPlaceSortierbareIntListe aufsteigend in situ.
     * 
     * @param liste die zu sortierende Liste
     * @param von der Anfang des Ausschnitts
     * @param bis das Ende des Ausschnitts
     */
    private void sortiere(InPlaceSortierbareIntListe liste, final int von, final int bis)
    {
        if (bis-von <= 1) {
            return;
        }

        int pivotIndex = von + bis;
        if (pivotIndex % 2 != 0) {
            pivotIndex++;
        }
        pivotIndex /= 2;

        int pivot = liste.gib(pivotIndex);
        int i = von;
        int j = bis;
        
        while (i <= j)
        {
            while (liste.gib(i) < pivot)
            {
                ++i;
            }
            
            while (liste.gib(j) > pivot)
            {
                --j;
            }
            
            if (i <= j)
            {
                liste.vertausche(i, j);
                ++i;

                --j;
            }
        }
        
/*        if (j - i == 2)
            i--;*/
        
        sortiere(liste, von, j);
        sortiere(liste, i, bis);
    }
}
