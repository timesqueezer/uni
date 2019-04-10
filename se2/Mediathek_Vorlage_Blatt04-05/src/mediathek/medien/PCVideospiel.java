package mediathek.medien;

public class PCVideospiel extends AbstractVideospiel
{
    private static final int zeitabhaengigerPreis = 500;
    public PCVideospiel(String titel, String kommentar, String system)
    {
        super(titel, kommentar, system);
    }
    
    @Override
    public String getMedienBezeichnung()
    {
        return "PCVideospiel";
    }
        
    protected int getPreisNachTagen(int tage)
    {
        int zusaetzlicherBetrag = 0;
        if (tage > 7) {
            zusaetzlicherBetrag += (int)Math.ceil((double)(tage - 7) / 5) * zeitabhaengigerPreis;
        }
        return zusaetzlicherBetrag;
    }
}
