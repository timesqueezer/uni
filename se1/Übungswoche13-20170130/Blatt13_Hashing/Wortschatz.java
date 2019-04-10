/**
 * Ein Wortschatz ist eine Menge von Woertern. Dieses Interface definiert
 * geeignete Operationen auf einer solchen Menge:
 * Es koennen Woerter hinzugefuegt werden, es kann abgefragt werden,
 * ob ein bestimmtes Wort im Wortschatz enthalten ist, und es kann die Anzahl
 * der gespeicherten Woerter abgefragt werden.
 * 
 * @author Fredrik Winkler
 * @author Petra Becker-Pechau
 * @author Axel Schmolitzky
 * @version 20. Januar 2015
 */
interface Wortschatz
{
    /**
     * Fuege ein Wort zum Wortschatz hinzu, sofern es noch nicht enthalten ist.
     * 
     * @param wort das hinzuzufuegende Wort
     */
    public void fuegeWortHinzu(String wort);

    /**
     * Gib an, ob ein Wort im Wortschatz enthalten ist.
     * 
     * @param wort das zu ueberpruefende Wort
     * @return true, falls das Wort enthalten ist, false sonst
     */
    public boolean enthaeltWort(String wort);
    
    /**
     * Gib an, wieviele Woerter im Wortschatz enthalten sind.
     * 
     * @return die Anzahl der Woerter im Wortschatz
     */
    public int anzahlWoerter();

    /**
     * Diese Beschreibung dient zur Unterscheidung beim Messen.
     */
    public String gibBeschreibung();
    
    /**
     * Schreibt den Wortschatz auf die Konsole (als Debugging-Hilfe gedacht).
     */
    public void schreibeAufKonsole();
}
