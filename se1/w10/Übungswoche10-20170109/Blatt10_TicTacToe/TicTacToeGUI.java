import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 * Eine einfache grafische Oberflaeche fuer das Spiel Tic Tac Toe.
 * 
 * @author Fredrik Winkler
 * @version 3. Januar 2015
 */
class TicTacToeGUI
{
    private TicTacToe _game;
    private JFrame _frame;
    private JButton[] _buttons;
    
    public TicTacToeGUI()
    {
        _game = new TicTacToe();
        _frame = new JFrame();
        _frame.setLayout(new GridLayout(3, 3));
        _buttons = new JButton[9];
        final Font font = new Font(Font.MONOSPACED, Font.PLAIN, 50);
        
        for (int i = 0; i < 9; ++i)
        {
            _buttons[i] = new JButton();
            _buttons[i].setFont(font);
            _buttons[i].addActionListener(new ButtonListener(i));
            _frame.add(_buttons[i]);
        }

        _frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        _frame.setSize(300, 300);
        _frame.setResizable(false);
        _frame.setVisible(true);
        
        refreshTitle();
    }

    private void refreshGrid()
    {
        for (int i = 0; i < 9; ++i)
        {
            _buttons[i].setText(BESCHRIFTUNG[(_game.gibBesitzer(i))]);
        }
    }

    private void refreshTitle()
    {
        _frame.setTitle(SPIELER[_game.gibAktuellenSpieler()]);
    }

    private void refresh()
    {
        refreshGrid();
        refreshTitle();
    }

    private class ButtonListener implements ActionListener
    {
        private final int _position;

        public ButtonListener(int position)
        {
            _position = position;
        }

        public void actionPerformed(ActionEvent ignored)
        {
            if (_game.istFrei(_position))
            {
                _game.besetzePosition(_position);
                refreshGrid();

                if (_game.istSpielZuEnde())
                {
                    JOptionPane.showMessageDialog(null, String.format(GEWONNEN, SPIELER[_game.gibAktuellenSpieler()]), "Spielende", JOptionPane.INFORMATION_MESSAGE);
                    _game = new TicTacToe();
                    refresh();
                }
                else
                {
                    refreshTitle();
                }
            }
            else
            {
                JOptionPane.showMessageDialog(null, BELEGT, "Position belegt", JOptionPane.ERROR_MESSAGE);
            }
        }
    }

    private static final String[] BESCHRIFTUNG = { " ", "X", "O" };
    private static final String[] SPIELER = { "Keiner", "Spieler 1 (X)", "Spieler 2 (O)" };

    private static final String GEWONNEN = "%s hat gewonnen. OK druecken fuer ein weiteres Spiel!";
    private static final String BELEGT = "Diese Position ist bereits belegt. Bitte eine andere waehlen!";
}
