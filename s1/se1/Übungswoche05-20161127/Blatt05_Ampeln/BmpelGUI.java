import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;

/**
 * Stellt eine Ampel graphisch dar und ermoeglicht das Weiterschalten per Knopfdruck.
 *  
 * @author Fredrik Winkler
 * @author Christian Spaeh
 * @version November 2014
 */
class BmpelGUI
{
    private Bmpel _ampel;
    private JFrame _frame;
    private JButton _roteLampe;
    private JButton _gelbeLampe;
    private JButton _grueneLampe;

    /**
     * Erzeugt eine neue Ampel und stellt diese anschliessend dar.
     */
    public BmpelGUI()
    {
        this(new Bmpel());
    }

    /**
     * Initialisiert eine neue Ampeldarstellung mit einer gegebenen Ampel.
     * 
     * @param ampel die darzustellende Ampel
     */
    private BmpelGUI(Bmpel ampel)
    {
        _ampel = ampel;
        initialisiereFrame();
        initialisiereLampen();
        initialisiereWeiter();
        aktualisiereLampen();
        macheGUIsichtbar();
    }

    /**
     * Konfiguriert den Fenstertitel und das Layout.
     */
    private void initialisiereFrame()
    {
        _frame = new JFrame("B");
        _frame.setLayout(new BoxLayout(_frame.getContentPane(), BoxLayout.PAGE_AXIS));
    }

    /**
     * Erzeugt fuer jede Lampe einen Button.
     */
    private void initialisiereLampen()
    {
        _roteLampe = neueLampe();
        _gelbeLampe = neueLampe();
        _grueneLampe = neueLampe();
    }

    /**
     * Erzeugt einen LampenButton.
     */
    private JButton neueLampe()
    {
        JButton lampe = new JButton("\u25c9");
        lampe.setFont(font);
        lampe.setBackground(Color.LIGHT_GRAY);
        lampe.setForeground(Color.BLACK);
        _frame.add(lampe);
        return lampe;
    }

    private static final Font font = new Font(Font.MONOSPACED, Font.PLAIN, 50);

    /**
     * Erzeugt den Weiter-Button und fueuegt einen ActionListener hinzu,
     * der die Ampel weiterschaltet, wenn der Button gedrueckt wird.
     */
    private void initialisiereWeiter()
    {
        JButton weiter = new JButton("weiter");
        weiter.addActionListener(new ActionListener()
            {
                public void actionPerformed(ActionEvent e)
                {
                    _ampel.schalteWeiter();
                    aktualisiereLampen();
                }
            });
        _frame.add(weiter);
    }

    /**
     * Setzt die Farben der LampenButtons entsprechend der Phase der verwendetet Ampel.
     */
    private void aktualisiereLampen()
    {
        _roteLampe.setForeground(_ampel.leuchtetRot() ? Color.RED : Color.BLACK);
        _gelbeLampe.setForeground(_ampel.leuchtetGelb() ? Color.YELLOW : Color.BLACK);
        _grueneLampe.setForeground(_ampel.leuchtetGruen() ? Color.GREEN : Color.BLACK);
    }

    /**
     * Setzt die DefaultCloseOperation, packt das Frame auf die richtige Groesse und macht es
     * sichtbar.
     */
    private void macheGUIsichtbar()
    {
        _frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        _frame.pack();
        _frame.setVisible(true);
    }
}
