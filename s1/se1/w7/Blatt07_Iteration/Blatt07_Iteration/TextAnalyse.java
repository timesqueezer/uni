package Blatt07_Iteration;

/**
 * Diese Klasse bietet die Moeglichkeit, Texte zu analysieren.
 * Sie dient als Einstieg in Schleifenkonstrukte und zeigt in
 * der Methode istFrage beispielhaft einige Methodenaufrufe an
 * einem Objekt der Klasse String.
 * 
 * @author Fredrik Winkler
 * @author Joerg Rathlev
 * @author Petra Becker-Pechau
 * @version November 2014
 */
class TextAnalyse
{
    String vokale;
    
    public TextAnalyse()
    {
        vokale = new String("aeiou");
    }
    /**
     * Ermittelt, ob es sich bei dem uebergebenen Text um eine Frage
     * handelt. Eine Frage erkennt man am abschliessenden Fragezeichen.
     * 
     * @param text der zu analysierende Text
     * @return true, wenn es sich um eine Frage handelt, false sonst
     */
    public boolean istFrage(String text)
    {
        int anzahlZeichen = text.length();
        
        if (anzahlZeichen == 0)
        {
            return false;
        }

        int letztePosition = anzahlZeichen - 1;

        char letztesZeichen = text.charAt(letztePosition);

        boolean endetAufFragezeichen = (letztesZeichen == '?');

        return endetAufFragezeichen;
    }

    /**
     * Ermittelt, ob es sich bei dem uebergebenen Text um eine Frage
     * handelt. Eine Frage erkennt man am abschliessenden Fragezeichen.
     * 
     * @param text der zu analysierende Text
     * @return true, wenn es sich um eine Frage handelt, false sonst
     */
    public boolean istFrageKompakt(String text)
    {
        return text.length() > 0 && text.charAt(text.length() - 1) == '?';
    }
    
    public int zaehleVokale(String text)
    {
        int vokalZaehler = 0;
        
        for (int i=0; i < text.length(); ++i)
        {
            if (vokale.indexOf(text.charAt(i)) > -1)
            {
                vokalZaehler++;
            }
        }
        
        return vokalZaehler;
    }
    
    /**
     * Ermittelt, ob der übergebene Text ein Palindrom ist
     * 
     * @param text zu überprüfender Text
     * @returns true, wenn text ein Palindrom ist, false, wenn nicht
     */
    public boolean istPalindrom(String text)
    {
        text = text.toLowerCase();

        int laenge = text.length();

        int halbeLaenge = laenge / 2;
        for (int i=0; i < halbeLaenge; ++i)
        {
            if (text.charAt(i) != text.charAt(laenge-i-1)) {
                return false;
            }
        }
        return true;
    }

    
}














