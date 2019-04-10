import java.io.File;

/**
 * Schreibt den Dateinamen inklusive Pfad auf die Konsole für Textdateien.
 * 
 * @author Matz Radloff
 * @version 19. Dezember 2014
 */
class TextdateiFilter implements DateiVerarbeiter
{
    /**
     * @see Dateiverarbeiter.verarbeite
     */
    public void verarbeite(File datei)
    {
        String[] parts = datei.getName().split("\\.");
        if (parts[parts.length-1] == "txt") {
            System.out.println(datei.getAbsolutePath());
        }
    }
}
