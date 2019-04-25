import static org.junit.Assert.*;
import org.junit.Test;

/**
 * Diese Klasse testet den Wortschatz.
 *
 * @author Fredrik Winkler
 * @version 20. Januar 2015
 */
public class WortschatzTest
{
    private final Wortschatz _schatz;
    
    /**
     * Jede Testmethode arbeitet auf einem frisch erzeugten Exemplar.
     */
    public WortschatzTest()
    {
        _schatz = new HashWortschatz(new Delegation(), 10);
    }
    
    /**
     * Stellt sicher, dass ein neuer Wortschatz leer ist.
     */
    @Test
    public void testNeuerWortschatzIstLeer()
    {
        assertEquals(0, _schatz.anzahlWoerter());
    }

    /**
     * Stellt sicher, dass ein hinzugefuegtes Wort auch wirklich enthalten ist.
     */
    @Test
    public void testHinzugefuegtesWortIstEnthalten()
    {
        _schatz.fuegeWortHinzu("Suppenkasper");
        assertTrue(_schatz.enthaeltWort("Suppenkasper"));
        assertEquals(1, _schatz.anzahlWoerter());
    }
    
    /**
     * Stellt sicher, dass ein nicht hinzugefügtes Wort auch nicht enthalten ist.
     */
    @Test
    public void testNichtHinzugefuegtesWortIstNichtEnthalten()
    {
        _schatz.fuegeWortHinzu("Testwort");
        assertFalse(_schatz.enthaeltWort("NichtEnthalten"));
    }
    
    /**
     * Stellt sicher, dass bei mehrfachem Hinzufügen des gleichen Wortes, nur eins gespeichert wird. 
     */
    @Test
    public void testDuplikateWerdenNichtHinzugefuegt()
    {
        _schatz.fuegeWortHinzu("TestWort");
        assertEquals(1, _schatz.anzahlWoerter());
        _schatz.fuegeWortHinzu("TestWort");
        assertEquals(1, _schatz.anzahlWoerter());
    }
}
