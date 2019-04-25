import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 * Eine optisch ansprechende, automatisch animierte Analog-Uhr.
 * 
 * @author Fredrik Winkler
 * @version 12. November 2015
 */
class AnalogUhr
{
    private Zeitmesser _zeitmesser = new Zeitmesser();

    private DrehbarerStrich _sekunden = new DrehbarerStrich(1.0, _zeitmesser, 1);
    private DrehbarerStrich _minuten = new DrehbarerStrich(0.9, _zeitmesser, 60);
    private DrehbarerStrich _stunden = new DrehbarerStrich(1.0, _zeitmesser, 3600/5);

    private DrehbarerStrich _grob = new DrehbarerStrich(0.1);
    private DrehbarerStrich _fein = new DrehbarerStrich(0.03);

    private Graphics2D _leinwand;
    private double _skalierung;

    private void zeichne()
    {
        zeichneZiffernblatt();
        zeichneZeiger();
    }

    private void zeichneZeiger()
    {
        zeichneZeiger(_stunden, 6, Color.BLACK);
        zeichneZeiger(_minuten, 4, Color.BLUE);
        zeichneZeiger(_sekunden, 1, Color.MAGENTA);
    }

    private void zeichneZeiger(DrehbarerStrich strich, float breite, Color farbe)
    {
        _leinwand.setStroke(new BasicStroke(breite));
        _leinwand.setColor(farbe);
        _leinwand.draw(strich.berechneLinie(_skalierung));
    }

    private void zeichneZiffernblatt()
    {
        // Grobes Ziffernblatt
        for (int i=0; i < 360; i += 30) {
            zeichneAbschnitt(i);
        }
    }

    private void zeichneAbschnitt(double startWinkel)
    {
        _grob.setzeWinkel(startWinkel);
        _leinwand.draw(_grob.berechneLinie(_skalierung));

       for (int i=1; i < 5; ++i) {
           _fein.setzeWinkel(startWinkel+(i*6));
           _leinwand.draw(_fein.berechneLinie(_skalierung));
        }
    }










    // Die folgenden Implementationsdetails muesst ihr noch nicht verstehen.
    // Grafische Oberflaechen werden erst in SE2 thematisiert.

    private JPanel _panel;
    private JFrame _frame;

    /**
     * Initialisiert die analoge Uhr auf Mitternacht und startet die Animation.
     */
    public AnalogUhr()
    {
        configurePanel();
        configureFrame();
        startTimer();
    }

    private void configurePanel()
    {
        _panel = new JPanel()
        {
            public void paint(Graphics graphics)
            {
                super.paint(graphics);
                _leinwand = (Graphics2D) graphics;
                _leinwand.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                _leinwand.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);
                _skalierung = Math.min(_panel.getWidth(), _panel.getHeight()) * 0.5;
                zeichne();
                Toolkit.getDefaultToolkit().sync();
            }
        };
        _panel.setPreferredSize(new Dimension(500, 500));
    }

    private void configureFrame()
    {
        _frame = new JFrame("Analog-Uhr");
        _frame.add(_panel);
        _frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        _frame.pack();
        _frame.setVisible(true);
    }

    private void startTimer()
    {
        ActionListener schritt = new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                _zeitmesser.notiereAktuelleZeit();
                _panel.repaint();
            }
        };
        new Timer(40, schritt).start();
    }
}
