import java.io.File;

/**
 * Ein VerzeichnisWanderer wandert rekursiv durch alle Dateien eines Verzeichnisses.
 *  
 * @author Fredrik Winkler
 * @version 16. Dezember 2014
 */
class VerzeichnisWanderer
{
    private File _startverzeichnis;
    private DateiVerarbeiter _dateiVerarbeiter;

    /**
     * Initialisiert einen neuen VerzeichnisWanderer, der das aktuelle Verzeichnis durchwandert.
     */
    public VerzeichnisWanderer()
    {
        this(System.getProperty("user.dir"));
    }

    /**
     * Initialisiert einen neuen VerzeichnisWanderer, der das angegebene Verzeichnis durchwandert.
     * 
     * @param startverzeichnis hier faengt der VerzeichnisWanderer an
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
     * Startet den VerzeichnisWanderer. Jede gefundene Datei wird dem dateiVerarbeiter uebermittelt.
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
