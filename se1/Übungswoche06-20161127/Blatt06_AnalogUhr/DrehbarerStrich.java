import java.awt.geom.Line2D;

/**
 * Exemplare dieser Klasse sind drehbare Striche in einem Einheitskreis.
 * Der Winkel kann fuer die Striche auf dem Ziffernblatt direkt gesetzt werden.
 * Fuer die Zeiger wird der Winkel abhaengig von der vergangenen Zeit berechnet.
 * 
 * @author Fredrik Winkler
 * @version 12. November 2015
 */
class DrehbarerStrich
{
    // Zwischen diesen beiden Radien erstreckt sich der Strich:
    private final double _radius1;
    private final double _radius2;

    // Der Winkel kann direkt gesetzt werden...
    private double _winkel;

    // ...oder er wird abhaengig von der vergangenen Zeit berechnet.
    private Zeitmesser _zeitmesser;
    private double _divisor;

    /**
     * Dieser Konstruktor ist fuer die Zeiger auf der Uhrenanzeige sinnvoll.
     * 
     * @param laenge
     *            die Laenge des Zeigers
     * @param zeitmesser
     *            der Zeitmesser, anhand dessen der Winkel berechnet wird
     * @param sekunden
     *            Wie vielen Sekunden entspricht der Zeiger?
     *            1 = Sekundenzeiger, 60 = Minutenzeiger, 3600 = Stundenzeiger
     */
    public DrehbarerStrich(double laenge, Zeitmesser zeitmesser, double sekunden)
    {
        _radius1 = 0;
        _radius2 = laenge;
        _zeitmesser = zeitmesser;
        _divisor = sekunden / 6.0;
    }

    /**
     * Dieser Konstruktor ist fuer die Striche auf dem Ziffernblatt sinnvoll.
     * 
     * @param laenge
     *            die Laenge des Strichs
     */
    public DrehbarerStrich(double laenge)
    {
        _radius1 = 1.0 - laenge;
        _radius2 = 1.0;
    }

    /**
     * Diese Methode ist fuer die Striche auf dem Ziffernblatt sinnvoll.
     * 
     * @param winkel
     *            der Winkel in Grad (360 Grad entspricht einer Umdrehung)
     */
    public void setzeWinkel(double winkel)
    {
        _winkel = winkel;
    }

    /**
     * Berechnet eine Linie, die sich durch Drehen des Strichs ergibt. Der
     * Drehwinkel ist entweder der explizit gesetzte oder der automatisch
     * berechnete, je nachdem, mit welchem Konstruktor das Exemplar
     * initialisiert wurde.
     * 
     * @param skalierung
     *            sollte der halben Aufloesung der Leinwand entsprechen, um
     *            diese komplett auszunutzen
     * @return die gedrehte und skalierte Linie
     */
    public Line2D berechneLinie(double skalierung)
    {
        if (_zeitmesser != null)
        {
            _winkel = _zeitmesser.abgelaufeneSekunden() / _divisor;
        }

        double radians = _winkel * (Math.PI / 180);
        double dx = Math.sin(radians) * skalierung;
        double dy = Math.cos(radians) * skalierung;

        double x1 = skalierung + dx * _radius1;
        double y1 = skalierung - dy * _radius1;
        double x2 = skalierung + dx * _radius2;
        double y2 = skalierung - dy * _radius2;

        return new Line2D.Double(x1, y1, x2, y2);
    }
}
