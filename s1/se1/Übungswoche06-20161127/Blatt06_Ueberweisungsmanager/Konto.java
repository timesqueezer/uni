/**
 * Ein einfaches Konto. Es kann abgehoben und eingezahlt werden.
 * Das Konto kann ueberzogen werden und speichert den Saldo als Integer.
 * 
 * @author Martti Jeenicke, Fredrik Winkler, Axel Schmolitzky, Christian Spaeh
 * @version WiSe 2013
 */
class Konto
{
    // der ganzzahlige Saldo
    private int _saldo;

    /**
     * Initialisiert ein neues, leeres Konto
     */
    public Konto()
    {
        _saldo = 0;
    }

    /**
     * Initialisiert ein neues Konto mit einem Startguthaben
     * 
     * @param startguthaben das gewuenschte Startguthaben
     */
    public Konto(int startguthaben)
    {
        _saldo = startguthaben;
    }

    /**
     * Zahlt einen Betrag auf das Konto ein
     * 
     * @param betrag der einzuzahlende Betrag
     */
    public void zahleEin(int betrag)
    {
        if (betrag >= 0)
        {
            _saldo = _saldo + betrag;
        }
        else
        {
            throw new IllegalArgumentException("Negativer Betrag");
        }
    }

    /**
     * Hebt einen Betrag vom Konto ab
     * 
     * @param betrag der abzuhebende Betrag
     */
    public void hebeAb(int betrag)
    {
        if (betrag >= 0)
        {
            _saldo = _saldo - betrag;
        }
        else
        {
            throw new IllegalArgumentException("Negativer Betrag");
        }
    }

    /**
     * Liefert den Saldo des Kontos zurueck
     * 
     * @return der Saldo des Kontos
     */
    public int gibSaldo()
    {
        return _saldo;
    }
}
