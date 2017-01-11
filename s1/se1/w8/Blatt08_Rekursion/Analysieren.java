/**
 * Die Methoden in dieser Klasse haben keine besonders aussagekraeftigen Namen.
 * Schaue dir den Quelltext an, fuehre die Methoden aus und dokumentiere sie.
 * 
 * @author Fredrik Winkler
 * @version 1. Dezember 2011
 */
class Analysieren
{
    /**
    * Liefert die fuehrende Ziffer einer Zahl.
    * 
    *    a(5678)
    * -> a(567)
    * -> a(56)
    * -> a(5)
    * -> 5
    */
    public int a(int x)
    {
        if (x < 10)     // wenn die Zahl nur aus einer Ziffer besteht...
        {
            return x;   // ...dann ist diese Ziffer das Ergebnis
        }
        else
        {               // ansonsten wird die rechte Ziffer abgeschnitten...
            return a(x / 10);
        }               // ...und die Funktion erneut ausgefuehrt
    }
    
    /**
    * Liefert die Anzahl Ziffern einer Zahl.
    * 
    *   b(5678)
    * -> 1 + b(567)
    * -> 1 + (1 + b(56))
    * -> 1 + (1 + (1 + b(5)))
    * -> 1 + (1 + (1 + 1))
    * -> 1 + (1 + 2)
    * -> 1 + 3
    * -> 4
    */
    public int b(int x)
    {
        if (x < 10)     // wenn die Zahl nur aus einer Ziffer besteht...
        {
            return 1;   // ...dann ist die Anzahl Ziffern 1
        }
        else
        {               // ansonsten ist das Ergebnis um 1 hoeher...
            return 1 + b(x / 10);
        }               // ...als die Anzahl Ziffern in der Zahl ohne die rechte Ziffer
    }
    
    /**
    * Gibt die Quersumme von x zurück.
    */
    public int c(int x)
    {
        if (x < 10)
        {
            return x;
        }
        else
        {
            return (x % 10) + c(x / 10);
        }
    }
    
    /**
    * Wandelt x zifferweise in eine Zeichen um und gibt sie als zusammengesetzten String zurück.
    */
    public String d(int x)
    {
        if (x < 10)
        {
            return "" + zifferAlsZeichen(x);
        }
        else
        {
            return d(x / 10) + zifferAlsZeichen(x % 10);
        }
    }

    /**
     * Wandelt eine Ziffer (0 bis 9) in ein Zeichen ('0' bis '9') um.
     * Wie das genau funktioniert, muesst ihr nicht unbedingt verstehen.
     */
    private char zifferAlsZeichen(int x)
    {
        return (char)('0' + x);
    }
    
    /**
    * Gibt den "gespiegelten" Wert von x zurück
    * 1234 -> 4321
    */
    public int e(int x)
    {
        return e_(x, 0);
    }
    
    /**
    * rekursive Hilfsmethode für e
    */
    private int e_(int x, int y)
    {
        if (x < 10)
        {
            return (y * 10) + x;
        }
        else
        {
            return e_(x / 10, (y * 10) + (x % 10));
        }
    }
    
    /**
    * Zählt die Zeichen im String s, die vor dem Zeichen c stehen.
    * 
    * @returns Position des Zeichens c im String s
    */
    public int f(String s, char c)
    {
        if (s.isEmpty())
        {
            return -1;
        }
        else
        {
            if (s.charAt(0) == c)
            {
                return 0;
            }
            else
            {
                return 1 + f(s.substring(1), c);
            }
        }
    }
    
    /**
    *Kommentar von Methode g
    */
    public int g(String s, char c)
    {
        return g_(s, c, 0);
    }
    
    /**
    *Kommentar von Methode g_
    */
    private int g_(String s, char c, int i)
    {
        if (i == s.length())
        {
            return -1;
        }
        else
        {
            if (s.charAt(i) == c)
            {
                return i;
            }
            else
            {
                return g_(s, c, i + 1);
            }
        }
    }

    /**
     * Hier finden die Aufrufe statt, die in den Diagrammen
     * auf dem Aufgabenblatt veranschaulicht werden (sollen).
     */
    public void testeAlleMethoden()
    {
        System.out.println(a(5678));
        System.out.println(b(5678));
        System.out.println(c(5678));
        System.out.println(d(5678));
        System.out.println(e(5678));
        System.out.println(f("Rosen", 'e'));
        System.out.println(g("Rosen", 'e'));
    }
}
