/**
 * In dieser Klasse sind rekursive Algorithmen umzusetzen.
 * 
 * @author Fredrik Winkler
 * @version 29. November 2011
 */
class Schreiben
{
    /**
    * Liefert die Fakultaet von n, also das Produkt aller natuerlicher Zahlen bis n.
    * Die Fakultaet von 0 ist per mathematischer Definition 1.
    *
    * Beispielauswertung:
    * 
    *   fak(4)
    * -> 4 * fak(3)
    * -> 4 * (3 * fak(2))
    * -> 4 * (3 * (2 * fak(1)))
    * -> 4 * (3 * (2 * (1 * fak(0))))
    * -> 4 * (3 * (2 * (1 * 1)))
    * -> 4 * (3 * (2 * 1))
    * -> 4 * (3 * 2)
    * -> 4 * 6
    * -> 24
    */
    public int fak(int n)
    {
        if (n == 1 || n == 0)
        {
            return 1;
        }
        return n * fak(n-1);
    }
    public int fak_end(int n, int akku)
    {
        if(n == 1 || n == 0)
        {
            return akku;
        }
        else
        {
            return fak(n-1, n*akku);
        }
    }
    
    /**
    * Gibt an, ob die Zeichenkette einen Vokal enthaelt. Auswertungen:
    * 
    * enthaeltVokal("brei") -> enthaeltVokal("rei") -> enthaeltVokal("ei") -> true
    * enthaeltVokal("xyz") -> enthaeltVokal("yz") -> enthaeltVokal("z") -> enthaeltVokal("") -> false
    */
    public boolean enthaeltVokal(String s)
    {
        if (s.length() == 0)
        {
            return false;
        }

        return istVokal(s.charAt(0)) || enthaeltVokal(s.substring(1));
    }
    
    /**
    * Prüft ob der eingeben charakter ein Vokal ist.
    */
    
    private boolean istVokal(char c)
    {
        return "aeiou".indexOf(c) > -1;
    }
    
    /**
    * Liefert die umgedrehte Zeichenkette. Auswertung:
    * 
    *   umgedreht("regal")
    * -> umgedreht("egal") + 'r'
    * -> (umgedreht("gal") + 'e') + 'r'
    * -> ((umgedreht("al") + 'g') + 'e') + 'r'
    * -> (((umgedreht("l") + 'a') + 'g') + 'e') + 'r'
    * -> (((          "l"  + 'a') + 'g') + 'e') + 'r'
    * -> ((           "la"        + 'g') + 'e') + 'r'
    * -> (            "lag"              + 'e') + 'r'
    * ->              "lage"                    + 'r'
    * ->              "lager"
    */
    public String umgedreht(String s)
    {
        if (s.length() == 1)
        {
            return s;
        }
        return umgedreht(s.substring(1)) + s.charAt(0);
    }
    
    
    

    
    /**
    * Gibt an, ob die Zeichenkette ein Palindrom ist. Auswertungen:
    * 
    * istPalindrom("anna") -> istPalindrom("nn") -> istPalindrom("") -> true
    * istPalindrom("asta") -> istPalindrom("st") -> false
    * istPalindrom("axa") -> istPalindrom("x") -> true
    * istPalindrom("xyz") -> false
    */
    public boolean istPalindrom(String s)
    {
        if(s.length() <= 1)
        {
            return true;
        }
        else
        {
            return ( s.charAt(0) == s.charAt(s.length()-1) && istPalindrom(s.substring(1, s.length()-1)) );
        }
        //return s.length() <= 1 || ( s.charAt(0) == s.charAt(s.length()-1) && istPalindrom(s.substring(1, s.length()-1)) );
    }
    
    /**
    * Berechnet die Anzahl Leerzeichen in der Zeichenketten. Auswertung:
    * 
    * anzahlLeerzeichen("a bc d")
    * -> 0 + anzahlLeerzeichen(" bc d")
    * -> 0 + (1 + anzahlLeerzeichen("bc d"))
    * -> 0 + (1 + (0 + anzahlLeerzeichen("c d")))
    * -> 0 + (1 + (0 + (0 + anzahlLeerzeichen(" d"))))
    * -> 0 + (1 + (0 + (0 + (1 + anzahlLeerzeichen("d")))))
    * -> 0 + (1 + (0 + (0 + (1 + (0 + anzahlLeerzeichen(""))))))
    * -> 0 + (1 + (0 + (0 + (1 + (0 + 0)))))
    * -> 0 + (1 + (0 + (0 + (1 + 0))))
    * -> 0 + (1 + (0 + (0 + 1)))
    * -> 0 + (1 + (0 + 1))
    * -> 0 + (1 + 1)
    * -> 0 + 2
    * -> 2
    */
    public int anzahlLeerzeichen(String s)
    {
        if (s.length() == 0)
        {
            return 0;
        }
        return (s.charAt(0) == ' ' ? 1 : 0) + anzahlLeerzeichen(s.substring(1));
    }
}
