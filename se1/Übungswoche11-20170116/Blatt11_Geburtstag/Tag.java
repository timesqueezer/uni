import java.util.Random;

/**
 * Diese Klasse modelliert einen Tag in einem unbestimmten Jahr, d.h. ohne
 * Angabe des Jahres selbst (z.B. den 1. April oder den 29. Februar).
 * 
 * @author Fredrik Winkler
 * @author Petra Becker-Pechau
 * @author Christian Spaeh
 * @version 16. Dezember 2014
 */
class Tag
{
    private final int _tagNummer;

    /**
     * Erstellt ein neues Tag-Objekt.
     * 
     * @param tagNummer der Tag des Jahres im Intervall [0,366).
     * 0 steht dabei fuer den 1. Januar, 365 fuer den 31. Dezember.
     */
    public Tag(int tagNummer)
    {
        if ((tagNummer < 0) || (tagNummer >= TAGE_PRO_JAHR))
        {
            throw new IllegalArgumentException(String.valueOf(tagNummer));
        }
        _tagNummer = tagNummer;
    }

    /**
     * Zwei Exemplare dieser Klasse, die denselben Tag repraesentieren,
     * werden als gleich angesehen.
     */
    public boolean equals(Object object)
    {
        if (object instanceof Tag)
        {
            Tag uebergebenerTag = (Tag) object;
            return _tagNummer == uebergebenerTag._tagNummer;
        }
        return false;
    }

    /**
     * @see Object.hashCode()
     */
    public int hashCode()
    {
        return _tagNummer;
    }

    /**
     * @see Object.toString()
     */
    public String toString()
    {
        int monat = 0;
        int tagNummer = _tagNummer;
        while (tagNummer >= TAGE_PRO_MONAT[monat])
        {
            tagNummer -= TAGE_PRO_MONAT[monat++];
        }
        return (tagNummer + 1) + ". " + MONATSNAME[monat];
    }

    /**
     * @return ein zufaelliger Tag im Jahr
     */
    public static Tag gibZufaelligenTag()
    {
        int tagNummer;
        do
        {
            tagNummer = zufall.nextInt(TAGE_PRO_JAHR);
        }
        while ((tagNummer == 59) && (zufall.nextInt(400) >= 97));
        // Der Tag mit der Nummer 59 ist der 29. Februar. Er darf nur 
        // mit einer Wahrscheinlichkeit von 97/400 erzeugt werden, 
        // da er in 400 Jahren genau 97x vorkommt. 
        return new Tag(tagNummer);
    }

    private static final int TAGE_PRO_JAHR = 366;

    private static final int[] TAGE_PRO_MONAT = { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

    private static final String[] MONATSNAME = { "Januar", "Februar", "Maerz", "April", "Mai",
            "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember" };

    private static final Random zufall = new Random();
}
