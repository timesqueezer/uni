
/**
 * Beschreiben Sie hier die Klasse Uhrenanzeige.
 * 
 * @author Matz, Anton
 * @version 0.1
 */
class Uhrenanzeige
{
    Nummernanzeige _stunden;
    Nummernanzeige _minuten;

    public Uhrenanzeige()
    {
        _stunden = new Nummernanzeige(24);
        _minuten = new Nummernanzeige(60);
    }
    
    public Uhrenanzeige(int stunden, int minuten)
    {
        _stunden = new Nummernanzeige(24);
        _minuten = new Nummernanzeige(60);
        _stunden.setzeWert(stunden);
        _minuten.setzeWert(minuten);
    }
    
    public String gibUhrzeitAlsString()
    {
        return _stunden.gibAnzeigewert() + ":" + _minuten.gibAnzeigewert();
    }
    
    public void setzeUhrzeit(int stunden, int minuten)
    {
        _stunden.setzeWert(stunden);
        _minuten.setzeWert(minuten);
    }
    
    public void taktsignalGeben() {
        _minuten.erhoehen();
        if (_minuten.gibWert() == 0)
            _stunden.erhoehen();

    }
}

