
/**
 * Generiert Lottozahlen.
 * 
 * @author Matz Radloff
 * @version 19.12.16
 */
class Lotto
{
    public static void sechsAus49()
    {
        Zahlensack _zahlensack = new Permutation(49);
        for (int i=0; i < 6; ++i)
        {
            System.out.println(_zahlensack.entferneZahl() + 1);
        }
    }
}

