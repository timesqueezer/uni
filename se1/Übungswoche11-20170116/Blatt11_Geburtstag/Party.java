/**
 * Auf einer Party werden Gaeste nach ihren Geburtstagen gefragt.
 * Falls irgendwann ein Geburtstag mehrfach vorkommt, liefert
 * mindestensEinGeburtstagMehrfach ab dem Zeitpunkt immer true (und vorher immer false).
 *  
 * @author Fredrik Winkler
 * @author Petra Becker-Pechau
 * @author Christian Spaeh
 * 
 * @version 16. Dezember 2014
 */
interface Party
{
    /**
     * Ein weiterer Gast hat seinen Geburtstag verraten :)
     * 
     * @param geburtstag der Geburtstag des Gasts
     */
    public void fuegeGeburtstagHinzu(Tag geburtstag);
    
    /**
     * Gibt an, ob mehrere Gaeste am gleichen Tag Geburtstag haben.
     */
    public boolean mindestensEinGeburtstagMehrfach();
}
