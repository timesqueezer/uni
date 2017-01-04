
/**
 * Generiert Lottozahlen.
 * 
 * @author Matz Radloff
 * @version 19.12.16
 */
class Lotto
{
    private Zahlensack _zahlensack;
    
    public Lotto()
    {
        _zahlensack = new Permutation(49);
    }
    
    public void sechsAus49()
    {
        for (int i=0; i < 6; ++i)
        {
            System.out.println(_zahlensack.entferneZahl());
        }
    }
}

