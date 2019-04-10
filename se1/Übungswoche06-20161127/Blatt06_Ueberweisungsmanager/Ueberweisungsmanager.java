
/**
 * Beschreiben Sie hier die Klasse Ueberweisungsmanager.
 * 
 * @author (Ihr Name) 
 * @version (eine Versionsnummer oder ein Datum)
 */
class Ueberweisungsmanager
{
    public Ueberweisungsmanager()
    {
        
    }
    
    public void ueberweise(Konto quellKonto, Konto zielKonto, int betrag)
    {
        if (quellKonto == null || zielKonto == null) {
            throw new IllegalArgumentException("Fehlendes Argument");
        }
        quellKonto.hebeAb(betrag);
        zielKonto.zahleEin(betrag);
    }
}

