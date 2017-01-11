/**
 * In dieser Klasse ist die Spiellogik von Tic Tac Toe realisiert.
 * 
 * Zwei Spieler spielen auf einem Spielfeld der Groesse 3x3 = 9 Positionen. Die Spieler
 * besetzen abwechselnd Positionen auf dem Spielfeld, bis ein Spieler drei Positionen in einer
 * Reihe besetzt hat oder das Spielfeld voll ist.
 * 
 * @author Fredrik Winkler
 * @author Axel Schmolitzky
 * @author Petra Becker-Pechau
 * @version 3. Januar 2015
 */
class TicTacToe
{
    private Spielfeld _spielfeld;
    private int _aktuellerSpieler;
    private boolean _gameOver;
    
    /**
     * Erzeugt ein neues Tic Tac Toe Spiel mit einem leeren Spielfeld. Spieler 1 ist als erster
     * dran, und das Spiel ist noch nicht zu Ende.
     */
    public TicTacToe()
    {
        _spielfeld = new ArraySpielfeld();
        _aktuellerSpieler = 1;
        _gameOver = false;
    }

    /**
     * Gibt an, welcher Spieler gerade dran ist oder gewonnen hat.
     * 
     * @return 1 (Spieler 1) oder 2 (Spieler 2) oder 0 (Spiel ist unentschieden ausgegangen)
     */
    public int gibAktuellenSpieler()
    {
        return _aktuellerSpieler;
    }

    /**
     * Wechselt den aktuellen Spieler von 1 zu 2 und umgekehrt.
     */
    private void wechsleSpieler()
    {
        _aktuellerSpieler = 3 - _aktuellerSpieler;
    }

    /**
     * Gibt an, ob das Spiel zu Ende ist. Falls es zu Ende ist, liefert gibAktuellenSpieler()
     * den Gewinner.
     */
    public boolean istSpielZuEnde()
    {
        return _gameOver;
    }

    /**
     * Gibt den Besitzer der angegebenen Position auf dem Spielfeld.
     * 
     * @param position 0..8
     * @return 0 (unbesetzt), 1 (Spieler 1), 2 (Spieler 2)
     */
    public int gibBesitzer(int position)
    {
        return _spielfeld.gibBesitzer(position);
    }

    /**
     * Ist das Spielfeld an der angegebenen Position noch frei?
     * 
     * @param position 0..8
     */
    public boolean istFrei(int position)
    {
        return gibBesitzer(position) == 0;
    }

    /**
     * Besetzt eine Position auf dem Spielfeld fuer den aktuellen Spieler. Anschliessend wird
     * geprueft, ob der aktuelle Spieler gewonnen hat, oder ob das Spielfeld voll ist.
     * Ansonsten wird der aktuelle Spieler gewechselt.
     * 
     * @param position 0..8
     */
    public void besetzePosition(int position)
    {
        if (!istSpielZuEnde() && istFrei(position))
        {
            _spielfeld.besetzePosition(position, _aktuellerSpieler);

            if (hatAktuellerSpielerGewonnen())
            {
                _gameOver = true;
            }
            else if (_spielfeld.istVoll())
            {
                _aktuellerSpieler = 0;
                _gameOver = true;
            }
            else
            {
                wechsleSpieler();
            }
        }
    }

    /**
     * @param position 0..8
     * 
     * @return true, falls der aktuelle Spieler an der uebergebenen Position das Spielfeld
     *         besetzt hat.
     */
    private boolean aktuellerSpielerBesitzt(int position)
    {
        return gibBesitzer(position) == _aktuellerSpieler;
    }

    /**
     * Diese Methode ueberprueft, ob der aktuelle Spieler
     * an allen drei uebergebenen Positionen p1, p2 und p3 das Spielfeld besetzt hat.
     * 
     * @param position1 0..8
     * @param position2 0..8
     * @param position3 0..8
     * 
     * @return true, wenn der aktuelle Spieler alle drei spezifizierten
     *         Positionen besetzt, sonst false.
     */
    private boolean aktuellerSpielerBesitzt(int position1, int position2, int position3)
    {
        return aktuellerSpielerBesitzt(position1)
            && aktuellerSpielerBesitzt(position2)
            && aktuellerSpielerBesitzt(position3);
    }

    /**
     * @return true, falls der aktuelle Spieler eine der acht moeglichen Gewinnsituationen
     *         erreicht hat.
     */
    private boolean hatAktuellerSpielerGewonnen()
    {
            // Zeilen
        return aktuellerSpielerBesitzt(0, 1, 2)
            || aktuellerSpielerBesitzt(3, 4, 5)
            || aktuellerSpielerBesitzt(6, 7, 8)
            // Spalten
            || aktuellerSpielerBesitzt(0, 3, 6)
            || aktuellerSpielerBesitzt(1, 4, 7)
            || aktuellerSpielerBesitzt(2, 5, 8)
            // Diagonalen
            || aktuellerSpielerBesitzt(0, 4, 8)
            || aktuellerSpielerBesitzt(2, 4, 6);
    }
}
