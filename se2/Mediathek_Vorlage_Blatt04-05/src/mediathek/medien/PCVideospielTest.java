package mediathek.medien;
import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

import mediathek.fachwerte.Geldbetrag;

/**
 */
public class PCVideospielTest
{
    private static final String KOMMENTAR = "Kommentar";
    private static final String TITEL = "Titel";
    private static final String BEZEICHNUNG = "PCVideospiel";
    private static final String SYSTEM = "PC";
    private PCVideospiel _pcvideoSpiel;

    @Before
    public void setUp()
    {
        _pcvideoSpiel = getMedium();
    }

    @Test
    public void testeKonstruktoren()
    {
        assertEquals(TITEL, _pcvideoSpiel.getTitel());
        assertEquals(KOMMENTAR, _pcvideoSpiel.getKommentar());
        assertEquals(SYSTEM, _pcvideoSpiel.getSystem());
    }

    @Test
    public void testGetMedienBezeichnung()
    {
        String videospielBezeichnung = BEZEICHNUNG;
        assertEquals(videospielBezeichnung, _pcvideoSpiel.getMedienBezeichnung());
    }

    protected PCVideospiel getMedium()
    {
        return new PCVideospiel(TITEL, KOMMENTAR, SYSTEM);
    }

    @Test
    public final void testSetKommentar()
    {
       PCVideospiel medium = getMedium();
        medium.setKommentar("Kommentar2");
        assertEquals(medium.getKommentar(), "Kommentar2");
    }

    @Test
    public final void testSetTitel()
    {
        PCVideospiel medium = getMedium();
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
        
        mietTage = 7;
        geldbetrag = new Geldbetrag(200);
        assertEquals(medium.berechneMietgebuehr(mietTage), geldbetrag);
        
        mietTage = 8;
        geldbetrag = new Geldbetrag(700);
        assertEquals(medium.berechneMietgebuehr(mietTage), geldbetrag);
        
        mietTage = 13;
        geldbetrag = new Geldbetrag(1200);
        assertEquals(medium.berechneMietgebuehr(mietTage), geldbetrag);
    }

}
