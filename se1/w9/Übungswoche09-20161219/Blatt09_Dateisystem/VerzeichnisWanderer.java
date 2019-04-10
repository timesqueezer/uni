import java.io.File;

/**
 * Ein Verzeichniswanderer wandert rekursiv durch alle Dateien eines Verzeichnisses.
 *  
 * @author Fredrik Winkler
 * @version 6. Dezember 2014
 */
class VerzeichnisWanderer
{
    private File _startverzeichnis;
    private DateiVerarbeiter _dateiVerarbeiter;

    /**
     * Initialisiert einen neuen Verzeichniswanderer, der das Home-Verzeichnis durchwandert.
     */
    public VerzeichnisWanderer()
    {
        this(System.getProperty("user.home"));
    }

    /**
     * Initialisiert einen neuen Verzeichniswanderer, der das angegebene Verzeichnis durchwandert.
     * 
     * @param startverzeichnis hier faengt der Verzeichniswanderer an
     */
    public VerzeichnisWanderer(String startverzeichnis)
    {
        _startverzeichnis = new File(startverzeichnis);
        if (!_startverzeichnis.exists())
        {
            throw new IllegalArgumentException(_startverzeichnis + " existiert nicht");
        }
        if (!_startverzeichnis.isDirectory())
        {
            throw new IllegalArgumentException(_startverzeichnis + " ist kein Verzeichnis");
        }
    }

    /**
     * Startet den Verzeichniswanderer. Jede gefundene Datei wird dem dateiverarbeiter uebermittelt.
     */
    public void start(DateiVerarbeiter dateiVerarbeiter)
    {
        _dateiVerarbeiter = dateiVerarbeiter;
        untersuche(_startverzeichnis);
    }

    /**
     * Untersucht rekursiv alle Dateien eines Verzeichnisses.
     */
    private void untersuche(File verzeichnis)
    {
        File[] files = verzeichnis.listFiles();
        if (files != null)
        {          
            for (File file : files)
            {
                if (!file.isHidden() && file.canRead())
                {
                    if (file.isDirectory())
                    {
                        untersuche(file);
                    }
                    else
                    {
                        _dateiVerarbeiter.verarbeite(file);
                    }
                }
            }
        }
    }
}
