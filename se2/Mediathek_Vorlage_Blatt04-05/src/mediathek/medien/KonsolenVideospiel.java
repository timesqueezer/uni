package mediathek.medien;

public class KonsolenVideospiel extends AbstractVideospiel
{
    private static final int preisProVolleDreiTage = 700;
    public KonsolenVideospiel(String titel, String kommentar, String system)
    {
        super(titel, kommentar, system);
    }
    
    @Override
    public String getMedienBezeichnung()
    {
        return "KonsolenVideospiel";
    }
    
    protected int getPreisNachTagen(int tage)
    {
        int zusaetzlicherBetrag = tage / 3 * preisProVolleDreiTage;
        return zusaetzlicherBetrag;
    }

}
