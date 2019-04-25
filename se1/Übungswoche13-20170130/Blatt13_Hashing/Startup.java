import java.util.List;

/**
 * Diese Klasse dient als Startup fuer das Aufgabenblatt zur
 * Implementierung von Hash-Verfahren.
 * 
 * @author Fredrik Winkler
 * @author Petra Becker-Pechau
 * @author Axel Schmolitzky
 * @version 20. Januar 2015
 */
class Startup
{
    // Einige Primzahlen, die sich gut zur Dimensionierung von Hashtabellen eignen:
    private static final int KLEIN  =    11;
    private static final int MITTEL =   101;
    private static final int GROSS  =  1009;
    private static final int RIESIG = 10007;

    /**
     * Zeigt die Hashtabelle eines Wortschatzes auf der Konsole an.
     */
    public static void visualisiereHashtabelle()
    {
        List<String> kurz = Worteinleser.dateiAlsText("trinken.txt");
        HashWertBerechner delegation = new Delegation();

        generiereWortschatz(kurz, new HashWortschatz(delegation, KLEIN)).schreibeAufKonsole();
    }
    
    /**
     * Vergleicht die Performance verschiedener Hashfunktionen.
     */
    public static void vergleichePerformance()
    {
        List<String> lang = Worteinleser.dateiAlsText("moby10b.txt");
        HashWertBerechner delegation = new Delegation();
        
        vermesse(lang, new HashWortschatz(delegation, MITTEL));
    }

    /**
     * Generiert einen Wortschatz aus dem gegebenenen Text.
     *
     * @param text der zu verarbeitende Text
     * @param wortschatz in diesen Wortschatz wird eingefuegt
     * @return wortschatz
     */
    private static Wortschatz generiereWortschatz(List<String> text, Wortschatz wortschatz)
    {
        for (String wort : text)
        {
            wortschatz.fuegeWortHinzu(wort);
        }
        return wortschatz;
    }

    /**
     * Diese Methode misst, wie lange es dauert,
     * einen Wortschatz aus dem gegebenen Text zu generieren.
     *
     * @param text der zu verarbeitende Text
     * @param wortschatz in diesen Wortschatz wird eingefuegt
     */
    private static void vermesse(List<String> text, Wortschatz wortschatz)
    {
        Stoppuhr uhr = new Stoppuhr(System.out, wortschatz.gibBeschreibung());
        generiereWortschatz(text, wortschatz);
        uhr.stopp();
        System.out.println(wortschatz.anzahlWoerter() + " verschiedene Woerter gefunden\n");

    }
}
