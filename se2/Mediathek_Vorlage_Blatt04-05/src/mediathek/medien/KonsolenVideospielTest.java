package mediathek.medien;
import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

import mediathek.fachwerte.Geldbetrag;

/**
 */
public class KonsolenVideospielTest
{
    private static final String KOMMENTAR = "Kommentar";
    private static final String TITEL = "Titel";
    private static final String BEZEICHNUNG = "KonsolenVideospiel";
    private static final String SYSTEM = "Switch";
    private KonsolenVideospiel _konsolenVideospiel;

    @Before
    public void setUp()
    {
        _konsolenVideospiel = getMedium();
    }

    @Test
    public void testeKonstruktoren()
    {
        assertEquals(TITEL, _konsolenVideospiel.getTitel());
        assertEquals(KOMMENTAR, _konsolenVideospiel.getKommentar());
        assertEquals(SYSTEM, _konsolenVideospiel.getSystem());
    }

    @Test
    public void testGetMedienBezeichnung()
    {
        String KonsolenVideospielBezeichnung = BEZEICHNUNG;
        assertEquals(KonsolenVideospielBezeichnung, _konsolenVideospiel.getMedienBezeichnung());
    }

    protected KonsolenVideospiel getMedium()
    {
        return new KonsolenVideospiel(TITEL, KOMMENTAR, SYSTEM);
    }

    @Test
    public final void testSetKommentar()
    {
        KonsolenVideospiel medium = getMedium();
        medium.setKommentar("Kommentar2");
        assertEquals(medium.getKommentar(), "Kommentar2");
    }

    @Test
    public final void testSetTitel()
    {
        KonsolenVideospiel medium = getMedium();
        medium.setTitel("Titel2");
        assertEquals(medium.getTitel(), "Titel2");
    }
    
    @Test
    public void testBerechneMietgebuehr()
    {
        Medium medium = getMedium();
        int mietTage = 1;
        Geldbetrag geldbetrag = new Geldbetrag(200);
        assertEquals(medium.berechneMietgebuehr(mietTage), geldbetrag);
        
        mietTage = 3;
        geldbetrag = new Geldbetrag(900);
        assertEquals(medium.berechneMietgebuehr(mietTage), geldbetrag);
        
        mietTage = 4;
        geldbetrag = new Geldbetrag(900);
        assertEquals(medium.berechneMietgebuehr(mietTage), geldbetrag);
        
        mietTage = 9;
        geldbetrag = new Geldbetrag(2300);
        assertEquals(medium.berechneMietgebuehr(mietTage), geldbetrag);
    }

}
