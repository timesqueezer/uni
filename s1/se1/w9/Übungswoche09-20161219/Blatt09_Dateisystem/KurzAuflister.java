import java.io.File;

/**
 * Schreibt den Dateinamen exklusive Pfad auf die Konsole.
 * 
 * @author Fredrik Winkler
 * @version 6. Dezember 2014
 */
class KurzAuflister implements DateiVerarbeiter
{
    /**
     * @see Dateiverarbeiter.verarbeite
     */
    public void verarbeite(File datei)
    {
        System.out.println(datei.getName());
    }
}
