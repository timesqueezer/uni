import java.util.HashSet;
/**
 * Beschreiben Sie hier die Klasse MengenParty.
 * 
 * @author (Ihr Name) 
 * @version (eine Versionsnummer oder ein Datum)
 */
class MengenParty implements Party
{
    private final HashSet<Tag> _geburtstage;
    private boolean _duplicateFound;
    
    MengenParty() {
        _geburtstage = new HashSet<Tag>();
        _duplicateFound = false;
    }
    
    public void fuegeGeburtstagHinzu(Tag geburtstag) {
        _duplicateFound = !_geburtstage.add(geburtstag);
    }
    
    public boolean mindestensEinGeburtstagMehrfach() {
        if (_duplicateFound) {
            _duplicateFound = false;
            return true;
        } else {
            return false;
        }
    }   
    
}

