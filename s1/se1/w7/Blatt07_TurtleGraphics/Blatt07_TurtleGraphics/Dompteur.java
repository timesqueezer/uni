 /**
 * Exemplare dieser Klasse veranlassen Turtles dazu,
 * Spuren auf einer Zeichenflaeche zu hinterlassen.
 *
 * @author Till Aust
 * @author Axel Schmolitzky
 * @version 26. November 2005
 */
class Dompteur
{
    /**
     * 'SE1' auf die Zeichenflaeche zeichnen.
     */
    public void start()
    {
        // Neue Turtle an Position (50, 100) erzeugen.
        Turtle turtle = new Turtle(50, 100);

        // 'S' zeichnen:
        turtle.geheVor(30);
        turtle.drehe(-90);
        turtle.geheVor(30);
        turtle.drehe(-90);
        turtle.geheVor(30);
        turtle.drehe(90);
        turtle.geheVor(30);
        turtle.drehe(90);
        turtle.geheVor(30);

        // Ohne Spur zum naechsten Buchstaben bewegen:
        turtle.hinterlasseKeineSpur();
        turtle.geheZu(130, 100);

        // 'E' zeichnen:
        turtle.hinterlasseSpur();
        turtle.drehe(-180);
        turtle.geheVor(30);
        turtle.drehe(90);
        turtle.geheVor(30);
        turtle.drehe(90);
        turtle.geheVor(30);
        turtle.drehe(-180);
        turtle.geheVor(30);
        turtle.drehe(90);
        turtle.geheVor(30);
        turtle.drehe(90);
        turtle.geheVor(30);

        // Ohne Spur zum naechsten Buchstaben bewegen:
        turtle.hinterlasseKeineSpur();
        turtle.geheZu(180, 100);

        // '1' zeichnen:
        turtle.hinterlasseSpur();
        turtle.setzeFarbe("rot");
        turtle.drehe(-90);
        turtle.geheVor(60);
        turtle.drehe(-120);
        turtle.geheVor(20);
    }
    
    public void zeichneNEck(int anzahlEcken, double laengeKanten, double startX, double startY, String farbe)
    {
        Turtle turtle = new Turtle(startX, startY);
        turtle.setzeFarbe(farbe);
        turtle.hinterlasseSpur();
        for (int i=0; i < anzahlEcken; ++i)
        {
            turtle.geheVor(laengeKanten);
            turtle.drehe((double) 360.0f / anzahlEcken);
        }
    }
    
    public void zeichneVerschNEcke(int anzahlEcken, double grLaengeKanten)
    {
        for (double i = grLaengeKanten; i >= 0; i -= 10) 
        {
            zeichneNEck(anzahlEcken, i, 100, 100, "blau");
        }
    }
}
