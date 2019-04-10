/**
 * Ein Wortschatz ist eine Menge von Woertern.
 * Es koennen Woerter hinzugefuegt werden, es kann abgefragt werden,
 * ob ein bestimmtes Wort im Wortschatz enthalten ist, und es kann die Anzahl
 * der gespeicherten Woerter abgefragt werden.
 * 
 * @author  (your name)
 * @version (a version number or a date)
 */
import java.util.LinkedList;


class HashWortschatz implements Wortschatz
{
    private final HashWertBerechner _berechner;
    private final WortListe[] _buckets;
    
    /**
     * Initialisiert ein neues Exemplar von HashWortschatz.
     * 
     * @param berechner der Berechner, welcher die Hashfunktion umsetzt
     * @param groesse die (initiale) Groesse der Hashtabelle
     */
    public HashWortschatz(HashWertBerechner berechner, int groesse)
    {
        _berechner = berechner;
        
        _buckets = new WortListe[groesse];
        for (int i = 0; i < groesse; ++i)
        {
            _buckets[i] = new WortListe();
        }
    }
    
    /**
     * Fuege ein Wort zum Wortschatz hinzu, sofern es noch nicht enthalten ist.
     * 
     * @param wort das hinzuzufuegende Wort
     */
    public void fuegeWortHinzu(String wort)
    {
        if (!enthaeltWort(wort))
        {
            int index = hashToIndex(_berechner.hashWert(wort));
            _buckets[index].fuegeWortHinzu(wort);
        }
    }
    
    private int hashToIndex(int hash)
    {
        return Math.abs(hash % _buckets.length);
    }
    
    /**
     * Gib an, ob ein Wort im Wortschatz enthalten ist.
     * 
     * @param wort das zu ueberpruefende Wort
     * @return true, falls das Wort enthalten ist, false sonst
     */
    public boolean enthaeltWort(String wort)
    {
        int index = hashToIndex(_berechner.hashWert(wort));
        return _buckets[index].enthaeltWort(wort);
    }
    
    /**
     * Gib an, wieviele Woerter im Wortschatz enthalten sind.
     * 
     * @return die Anzahl der Woerter im Wortschatz
     */
    public int anzahlWoerter()
    {
        int accu = 0;
        for (WortListe bucket : _buckets)
        {
            accu += bucket.anzahlWoerter();
        }
        return accu;
    }

    /**
     * Diese Beschreibung dient zur Unterscheidung beim Messen.
     */
    public String gibBeschreibung()
    {
        return _berechner.gibBeschreibung();
    }

    /**
     * Schreibt den Wortschatz auf die Konsole (als Debugging-Hilfe gedacht).
     */
    public void schreibeAufKonsole()
    {
        int i = 0;
        for (WortListe bucket : _buckets)
        {
            String outputLine = "[" + i + "]: " + bucket.toString();
            System.out.println(outputLine);
            ++i;
        }
    }
}
