package mediathek.services;
import java.io.FileWriter;
import java.io.IOException;

import mediathek.materialien.Verleihkarte;

public class VerleihProtokollierer
{
    private static FileWriter writer;

    /**
     * Protokolliert Verleih- und Rückgabeereignisse
     * @param ereignis: "ausgeliehen", "zurückgegeben"
     * @param verleihkarte
     */
    public static void protokolliere(String ereignis, Verleihkarte verleihkarte) throws ProtokollierException
    {
        String protString = verleihkarte.getMedium().getTitel() + 
                    " wurde von " +
                    verleihkarte.getEntleiher().getKundennummer() + 
                    " " + ereignis;
        try (FileWriter writer = new FileWriter("./protokoll.txt", true))
        {
            writer.write(protString + "\n");
        }
        catch (IOException e)
        {
            // System.err.println(e.getMessage());
            throw new ProtokollierException(e.getMessage());
        }
    }
}
