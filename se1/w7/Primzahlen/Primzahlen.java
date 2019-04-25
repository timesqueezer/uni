
/**
 * Write a description of class Primzahlen here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class Primzahlen
{

    public boolean istTeilbar(int x, int y)
    {
        return x % y == 0;
    }
    
    public boolean istPrimzahl(int x)
    {
        for (int i = x-1; i > 1; --i)
        {
            if (istTeilbar(x, i))
                return false;
        }
        return true;
    }
    
    public void schreibePrimzahlenBis(int grenze)
    {
        for (int i=2; i < grenze; ++i)
        {
            if (istPrimzahl(i))
                System.out.println(i);
        }
    }
}
