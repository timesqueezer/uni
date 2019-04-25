import java.util.Calendar;

/**
 * Diese Klasse modelliert Kontobewegungen. Eine Kontobewegung speichert den Betrag
 * einer Aus- oder Einzahlung mit der dazugehoerenden Zeit. Zusaetzlich
 * kann eine druckbare Stringrepraesenation ausgegeben werden.
 * 
 * Ein Exemplar dieser Klasse beschreibt genau eine Kontobewegung.
 * 
 * @author Fredrik Winkler
 * @author Christian Spaeh
 * @author Petra Becker-Pechau
 * @version WiSe 2013
 */
class Kontobewegung implements Comparable<Kontobewegung>
{
    private final int _betrag;
    private final Calendar _datum;

    /**
     * Initialisiert eine neue Kontobewegung. Ein positiver Betrag
     * repraesentiert eine Einzahlung, ein negativer eine Auszahlung.
     *
     * @param betrag die Hoehe der Kontobewegung.
     */
    public Kontobewegung(int betrag)
    {
        _betrag = betrag;
        _datum = Calendar.getInstance();
        try
        {
            Thread.sleep(20);
        }
        catch (InterruptedException ignore)
        {
            // Thread wurde vorzeitig wieder aufgeweckt. Dieser Fall ist fuer unser Programm irrelevant.
        }
    }

    /**
     * Liefert einen String mit einer druckbaren Darstellung dieser Kontobewegung.
     */
    public String gibDruckbareForm()
    {
        return String.format(
            "%02d.%02d.%04d    %02d:%02d:%02d %10d",
            gibTag(), gibMonat(), gibJahr(),
            gibStunde(), gibMinute(), gibSekunde(),
            gibBetrag());
    }

    /**
     * Liefert den Betrag dieser Kontobewegung.
     */
    public int gibBetrag()
    {
        return _betrag;
    }

    /**
     * Liefert das Jahr dieser Kontobewegung.
     */
    public int gibJahr()
    {
        return _datum.get(Calendar.YEAR);
    }

    /**
     * Liefert den Monat dieser Kontobewegung.
     */
    public int gibMonat()
    {
        return _datum.get(Calendar.MONTH) + 1;
    }

    /**
     * Liefert den Tag dieser Kontobewegung.
     */
    public int gibTag()
    {
        return _datum.get(Calendar.DAY_OF_MONTH);
    }

    /**
     * Liefert die Stunde dieser Kontobewegung.
     */
    public int gibStunde()
    {
        return _datum.get(Calendar.HOUR_OF_DAY);
    }

    /**
     * Liefert die Minute dieser Kontobewegung.
     */
    public int gibMinute()
    {
        return _datum.get(Calendar.MINUTE);
    }

    /**
     * Liefert die Sekunde dieser Kontobewegung.
     */
    public int gibSekunde()
    {
        return _datum.get(Calendar.SECOND);
    }

    /**
     * Vergleiche diese Kontobewegung mit einer anderen.
     * 
     * @param kb die andere Kontobewegung
     * @return -1, falls diese zeitlich vor der anderen liegt,
     * +1, falls diese zeitlich nach der anderen liegt,
     * ansonsten 0 (falls sie gleichzeitig sind)
     */
    public int compareTo(Kontobewegung kb)
    {
        return _datum.compareTo(kb._datum);
    }
}
