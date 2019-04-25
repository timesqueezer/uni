/**
 * Sortiert eine InPlaceSortierbareIntListe mit Bubblesort.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
class BubbleSortierer implements Sortierer
{
    /**
     * Sortiere die angegebene InPlaceSortierbareIntListe aufsteigend in situ.
     * @param liste die zu sortierende Liste
     */
    public void sortiere(InPlaceSortierbareIntListe liste)
    {
        for (int lastSortedPosition = 0; lastSortedPosition < liste.gibLaenge() - 2; ++lastSortedPosition)
        {
            for (int i = 0; i < liste.gibLaenge() - lastSortedPosition - 1; ++i)
            {
                if (liste.gib(i) > liste.gib(i + 1))
                {
                    liste.vertausche(i, i+1);
                }
            }
        }
    }
}
