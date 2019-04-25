
/**
 * Write a description of class QuadratischeGleichung here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class QuadratischeGleichung
{
    private double _a;
    private double _b;
    private double _c;

    /**
     * Constructor for objects of class QuadratischeGleichung
     * 
     * @param a Koeffizient a
     * @param b Koeffizient b
     * @param c Koeffizient c
     */
    public QuadratischeGleichung(double a, double b, double c)
    {
        _a = a;
        _b = b;
        _c = c;
    }

    /**
     * Erste Nullstelle
     * 
     * @return     erste Nullstelle
     */
    public double ersteNullstelle()
    {
        return ( (-1*_b) - Math.sqrt( (_b*_b) - (4*_a*_c) ) ) / (2*_a);
    }

    /**
     * Zweite Nullstelle
     * 
     * @return     zweite Nullstelle
     */
    public double zweiteNullstelle()
    {
        return ( (-1*_b) + Math.sqrt( (_b*_b) - (4*_a*_c) ) ) / (2*_a);
    }
    
    public int anzahlNullstellen()
    {
        double d = (_b*_b) - (4*_a*_c);
        
        if (d > 0)
        {
            return 2;
        }
        else if (d < 0) {
            return 0;
        }
        else
        {
            return 1;
        }
    }
}
