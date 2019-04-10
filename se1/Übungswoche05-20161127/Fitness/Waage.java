
/**
 * Write a description of class Waage here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class Waage
{
    private int _letztesGewicht;
    private int _trend;
    private int _min;
    private int _max;
    private int _numRegistrierungen;

    /**
     * Constructor for objects of class Waage
     * 
     * @param gewicht Gewicht in Gramm
     */
    public Waage(int gewicht)
    {
        _letztesGewicht = gewicht;
        _min = gewicht;
        _max = gewicht;
        _trend = 0;
        _numRegistrierungen = 1;
    }

    /**
     * Speichert das neue Gewicht
     * 
     * @param neuesGewicht neues Gewicht
     */
    public void registriere(int neuesGewicht)
    {
        _numRegistrierungen += 1;
        _trend = -1 * (_letztesGewicht - neuesGewicht);
        _letztesGewicht = neuesGewicht;
        _max = neuesGewicht > _max ? neuesGewicht : _max;
        _min = neuesGewicht < _min ? neuesGewicht : _min;
    }
    
    /**
     * Gibt zurück, ob der Nutzer leichter oder schwerer geworden ist bzw. gleichschwer geblieben ist.
     * 
     * @returns aktuellen trend
     */
    public int gibTrend()
    {
        if (_trend > 0)
        {
            return 1;
        }
        else if (_trend < 0)
        {
            return -1;
        }
        else
        {
            return 0;
        }
    }
    
    /**
     * gibt das Minimalgewicht zurück
     * 
     * @returns Minimalgewicht
     */
    public int gibMinimalGewicht()
    {
        return _min;
    }
    
    /**
     * gibt das Maximalgewicht zurück
     * 
     * @returns Maximalgewicht
     */
    public int gibMaximalGewicht()
    {
        return _max;
    }    
    
}
