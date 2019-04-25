package mediathek.medien;

import mediathek.fachwerte.Geldbetrag;

public abstract class AbstractMedium implements Medium
{
    private static final int preisProTag = 300;
    /**
     * Ein Kommentar zum Medium
     */
    protected String _kommentar;

    /**
     * Der Titel des Mediums
     * 
     */
    protected String _titel;

    /**
     * 
     * @param titel Der Titel der DVD.
     * @param kommentar Ein Kommentar zu der DVD.
     * 
     * @require titel != null
     * @require kommentar != null
     * 
     * @ensure getTitel() == titel
     * @ensure getKommentar() == kommentar
     */
    protected AbstractMedium(String titel, String kommentar)
    {
        assert titel != null : "Vorbedingung verletzt: titel != null";
        assert kommentar != null : "Vorbedingung verletzt: kommentar != null";
        
        _titel = titel;
        _kommentar = kommentar;
    }
    
    @Override
    public String getKommentar()
    {
        return _kommentar;
    }

    @Override
    public void setKommentar(String kommentar)
    {
        assert kommentar != null : "Vorbedingung verletzt: kommentar != null";
        _kommentar = kommentar;
    }

    @Override
    public String getTitel()
    {
        return _titel;
    }

    @Override
    public void setTitel(String titel)
    {
        assert titel != null : "Vorbedingung verletzt: titel != null";
        _titel = titel;
    }
    
    @Override
    public String getFormatiertenString()
    {
        return getMedienBezeichnung() + ":\n" + "    " + "Titel: " + _titel
                + "\n" + "    " + "Kommentar: " + _kommentar + "\n";
    }
    
    @Override
    public Geldbetrag berechneMietgebuehr(int mietTage)
    {
        return new Geldbetrag(preisProTag * mietTage);
    }
}
