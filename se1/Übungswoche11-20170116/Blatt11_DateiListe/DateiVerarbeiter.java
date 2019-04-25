import java.io.File;

/**
 * Ein DateiVerarbeiter verarbeitet eine Datei.
 * 
 * @author Fredrik Winkler
 * @version 16. Dezember 2014
 */
interface DateiVerarbeiter
{
    /**
     * Diese Methode wird vom VerzeichnisWanderer fuer jede Datei aufgerufen.
     * Was konkret mit jeder einzelnen Datei passiert, legen die implementierenden Klassen fest.
     */
    public void verarbeite(File datei);
}
