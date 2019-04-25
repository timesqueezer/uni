/**
 * Ein einfaches Programm, das den Umgang mit Arrays und statische Methoden demonstriert.
 * Das Programm kann ueber die Konsole gestartet und mit Argumenten gefuettert werden:
 * 
 * java Histogramm Hallo Welt
 * 
 * @author Fredrik Winkler
 * @version 6. Januar 2015
 */
class Histogramm
{
    private static final int[] histogramm;
    
    static
    {
        histogramm = new int[26];
    }
    
    public static void main(String[] args)
    {
        for (String a : args)
        {
            analysiere(a);
        }
        zeigeVerteilung();
    }

    /**
     * Erhoeht die Zaehler fuer die im uebergebenen String enthaltenen Buchstaben.
     */
    private static void analysiere(String s)
    {
        for (int i = 0; i < s.length(); ++i)
        {
            char zeichen = s.charAt(i);
            int index = stelleImAlphabet(zeichen);
            if (index != -1)
            {
                ++histogramm[index];
            }
        }
    }
    
    /**
     * Berechnet, an welcher Stelle im Alphabet des uebergebene Zeichen liegt.
     * A liegt an Position 0, und Z liegt an Position 25.
     * Falls das uebergebene Zeichen kein Buchstabe ist, wird -1 geliefert.
     */
    private static int stelleImAlphabet(char x)
    {
        if ((x >= 'A') && (x <= 'Z')) return x - 'A';
        if ((x >= 'a') && (x <= 'z')) return x - 'a';
        return -1;
    }

    private static void zeigeVerteilung()
    {
        for (int i=0; i<histogramm.length; ++i)
        {
            if (histogramm[i] > 0)
            {
                char c = (char) (i + 'A');
                System.out.println(c + ": " + histogramm[i]);
            }
        }
    }
}
