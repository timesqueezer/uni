import java.io.File;

/**
 * Exemplare dieser Klasse repraesentieren Dateien im Dateisystem.
 * Ihr Zweck liegt darin, das Auffinden von Duplikaten zu erleichtern.
 *  
 * @author Fredrik Winkler
 * @version 16. Dezember 2014
 */
class Datei
{
    private final File _file;
    private final long _groesse;
    private String _fingerabdruck;

    /**
     * Initialisiert eine neue Datei und merkt sich deren Groesse.
     * 
     * @param file das einzulesende File
     */
    public Datei(File file)
    {
        _file = file;
        _groesse = file.length();
    }

    /**
     * Wann gelten zwei Exemplare dieser Klasse als gleich?
     * 
     * Zwei Dateien gelten als gleich, sobald sie die gleiche Größe und den gleichen Fingerabdruck besitzen.
     * Ansonsten muss es nicht dieselbe Datei sein.
     */
    public boolean equals(Object obj)
    {
        /* instanceof bezieht sich auf den dynamischen Typ;
         * der statische wäre Object.
         */
        if (obj instanceof Datei)
        {
            Datei zweite = (Datei) obj;
            /*
             * obj hat den statischen Type Object und den dynamischen Typ Datei.
             * zweite hat nur den dynamischen Typ Datei, da er in jedem Fall mit
             * einem Object vom Typ Datei initialisiert wird. Hierbei wird kein 
             * Object kopiert, sondern das vorhandene obj von zweite referenziert.
             */
            return (_groesse == zweite._groesse)
                && fingerabdruck().equals(zweite.fingerabdruck())
                && _file.getName() == zweite._file.getName();
        }
        return false;
    }

    /**
     * Wie wird der hashCode berechnet?
     * 
     * anscheinend über die Größe der Datei, limitiert auf den Wertebereich eines int.
     * Dadurch wird der Vertrag zwischen equals() und hashCode() nicht eingehalten, da
     * zwei unterschiedliche Dateien den selben hashCode besitzen können.
     */
    public int hashCode()
    {
        return (int) _groesse;
    }

    /**
     * Liefert den Dateinamen und den Fingerabdruck als String zurueck.
     * 
     * @return String im Format "DATEINAME (FINGERABDRUCK)"
     */
    public String toString()
    {
        return _file.toString() + " (" + fingerabdruck() + ")";
    }

    /**
     * Liefert den Fingerabdruck. Da der Fingerabdruck nur bei Bedarf generiert wird,
     * kann der erste Aufruf dieser Methode u.U. recht lange dauern.
     * 
     * @return der Fingerabdruck
     */
    public String fingerabdruck()
    {
        if (_fingerabdruck == null)
        {
            _fingerabdruck = Fingerabdruck.aus(_file);
        }
        return _fingerabdruck;
    }

}
