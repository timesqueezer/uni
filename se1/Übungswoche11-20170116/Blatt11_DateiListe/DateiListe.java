import java.io.File;
import java.util.List;
import java.util.ArrayList;

/**
 * Eine Liste von Dateien, die zum Beispiel von einem VerzeichnisWanderer befuellt werden kann.
 * 
 * @author Fredrik Winkler
 * @version 16. Dezember 2014
 */
class DateiListe implements DateiVerarbeiter
{
    private List<File> _dateien;
    
    
    /**
     * Zu Beginn ist eine DateiListe leer.
     */
    public DateiListe()
    {
        _dateien = new ArrayList<File>();
        // Vor Initialisierung wäre der dynamische Typ null;
    }
    
    /**
     * Fuegt die uebergebene Datei zur DateiListe hinzu.
     */
    public void verarbeite(File datei)
    {
        _dateien.add(datei);
    }
    
    /**
     * Schreibt alle Eintraege auf die Konsole, zusammen mit ihrem Index. Beispiel:
     * 0. config.sys
     * 1. autoexec.bat
     * 2. command.com
     */
    public void schreibeAufDieKonsole()
    {
        for (int i = 0; i < _dateien.size(); ++i)
        {
            System.out.println(_dateien.get(i).getName());
        }
    }
    
    /**
     * Loescht alle Eintraege aus der DateiListe.
     */
    public void loescheAlleEintraege()
    {
        _dateien.clear();
    }

    /**
     * Liefert die Laenge (in Bytes) aller Dateien in der DateiListe.
     */
    public long gesamtLaenge()
    {
        long byteCount = 0;
        for (File f : _dateien)
        {
            byteCount += f.length();
        }
        return byteCount;
    }
}
