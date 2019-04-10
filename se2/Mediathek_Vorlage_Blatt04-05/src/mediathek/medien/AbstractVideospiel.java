package mediathek.medien;

import mediathek.fachwerte.Geldbetrag;

/**
 * {@link Videospiel} ist ein {@link Medium} mit einer zusätzlichen
 * Informationen zum kompatiblen System.
 * 
 * @author SE2-Team
 * @version SoSe 2012
 */
public abstract class AbstractVideospiel extends AbstractMedium
{
    /**
     * Das System, auf dem das Spiel lauffähig ist
     */
    protected String _system;

    /**
     * Initialisiert ein neues Videospiel.
     * 
     * @param system Die Bezeichnung des System
     * 
     * @require system != null
     * 
     * @ensure getSystem() == system
     */
    protected AbstractVideospiel(String titel, String kommentar, String system)
    {
        super(titel, kommentar);

        assert system != null : "Vorbedingung verletzt: system != null";
        _system = system;
    }

    @Override
    public String getMedienBezeichnung()
    {
        return "Videospiel";
    }

    /**
     * Gibt das System zurück, auf dem das Spiel lauffähig ist.
     * 
     * @return Das System, auf dem das Spiel ausgeführt werden kann.
     * 
     * @ensure result != null
     */
    public String getSystem()
    {
        return _system;
    }

    @Override
    public String toString()
    {
        return getFormatiertenString();
    }

    @Override
    public String getFormatiertenString()
    {
        return super.getFormatiertenString() + "    " + "System: " + _system + "\n";
    }
    
    @Override
    public Geldbetrag berechneMietgebuehr(int mietTage)
    {
        return new Geldbetrag(200 + getPreisNachTagen(mietTage));
    }

    protected abstract int getPreisNachTagen(int tage);
}
