import java.io.*;
import java.util.ArrayList;
import java.util.Collections;

/**
 * Die Klasse TitelBibliothek implementiert eine einfache Datenbasis von
 * Musiktiteln.
 * 
 * @author Axel Schmolitzky
 * @author Fredrik Winkler
 * @version 8. Januar 2015
 */
class TitelBibliothek
{
    private ArrayList<Titel> _bibliothek;

    /**
     * Initialisiert die TitelBibliothek aus der angegebenen Datei
     * 
     * @param bibliotheksdatei
     *            der Name einer Datei mit Titelinformationen
     */
    public TitelBibliothek(String bibliotheksdatei)
    {
        _bibliothek = new ArrayList<Titel>();
        liesEin(bibliotheksdatei);
    }

    /**
     * Initialisiert die TitelBibliothek aus der Datei JazzMix.txt
     */
    public TitelBibliothek()
    {
        this("JazzMix.txt");
    }
    
    /**
     * Liefert n zufaellige Titel aus dieser Bibliothek.
     */
    public Titel[] gibZufaelligeTitel(int n)
    {
        Collections.shuffle(_bibliothek);
        return _bibliothek.subList(0, n).toArray(new Titel[n]);
    }

    /**
     * Schreibt alle Titel in dieser Bibliothek auf die Konsole.
     */
    public void schreibeAufKonsole()
    {
        for (Titel titel : _bibliothek)
        {
            System.out.println(titel);
        }
    }

    /**
     * Liest Titelinformationen aus einer Textdatei ein. Jeder Titel steht in
     * einer eigenen Zeile.
     * 
     * @param dateiName
     *            der Dateiname der Textdatei
     */
    private void liesEin(String dateiName) 
    {
        try (BufferedReader reader = new BufferedReader(new FileReader(dateiName)))
        {
            String zeile;
            while ((zeile = reader.readLine()) != null)
            {
                verarbeiteZeile(zeile);
            }
        }
        catch (IOException ex)
        {
            System.out.println(ex);
        }
    }

    private void verarbeiteZeile(String zeile)
    {
        String[] spalten = zeile.split("\t");
        
        String name = spalten[0];
        String interpret = spalten[1];
        String album = spalten[2];
        String jahr = spalten[6];
        String genre = spalten[3];
        String dauer = spalten[4];

        Titel titel = new Titel(name, interpret, album, Integer.parseInt(jahr), genre, Integer.parseInt(dauer));
        _bibliothek.add(titel);
    }
}
