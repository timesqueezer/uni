import static java.util.Objects.requireNonNull;

/**
 * Eine Person hat einen Vornamen, einen Nachnamen, ein Geburtsjahr und ein Geschlecht.
 * 
 * @author Fredrik Winkler
 * @version 8. Dezember 2014
 */
public class Person
{
    private final String _vorname;
    private final String _nachname;
    private final int _geburtsjahr;
    private final boolean _maennlich;
    
    /**
     * Initialisiert ein Person-Objekt mit den angegeben Daten.
     */
    public Person(String vorname, String nachname, int geburtsjahr, boolean maennlich)
    {
        _vorname = requireNonNull(vorname);
        _nachname = requireNonNull(nachname);
        _geburtsjahr = geburtsjahr;
        _maennlich = maennlich;
    }

    /**
     * Liefert den Vornamen.
     */
    public String gibVorname()
    {
        return _vorname;
    }
    
    /**
     * Liefert den Nachnamen.
     */
    public String gibNachname()
    {
        return _nachname;
    }
    
    /**
     * Liefert das Geburtsjahr.
     */
    public int gibGeburtsjahr()
    {
        return _geburtsjahr;
    }
    
    /**
     * Gibt an, ob die Person maennlich ist.
     */
    public boolean istMaennlich()
    {
        return _maennlich;
    }
    
    /**
     * Liefert eine String-Darstellung im Format
     * Mustermann, Max (1987, maennlich)
     */
    public String toString()
    {
        return String.format("%s, %s (%d, %s)", _nachname, _vorname, _geburtsjahr, _maennlich ? "maennlich" : "weiblich");
    }
}
