import java.io.*;
import java.security.MessageDigest;

/**
 * Diese Klasse dient zum Generieren von Fingerabdruecken aus Dateien.
 *
 * @author Fredrik Winkler
 * @version 16. Dezember 2014
 */
class Fingerabdruck
{
    /**
     * Generiert aus dem Inhalt einer Datei einen Fingerabdruck.
     * Genau wie bei Menschen ist es extrem unwahrscheinlich,
     * dass inhaltsverschiedene Dateien denselben Fingerabdruck haben.
     * 
     * @param file die einzulesende Datei
     * @return der Fingerabdruck, sofern alles klappt, ansonsten der Dateiname 
     */
    public static String aus(File file)
    {
        try (BufferedInputStream in = new BufferedInputStream(new FileInputStream(file)))
        {
            MessageDigest digest = MessageDigest.getInstance("SHA");
            int numberOfBytesRead;
            while ((numberOfBytesRead = in.read(buffer)) != -1)
            {
                digest.update(buffer, 0, numberOfBytesRead);
            }
            return asHexString(digest.digest());
        }
        catch (Exception ignore)
        {
            // In seltenen Ausnahmefaellen kommt es zu einer Exception,
            // weil die Datei nicht mehr existiert oder geschuetzt ist.
            return file.toString();
        }
    }

    /**
     * Wandelt ein Byte-Array in einen hexadezimalen String um.
     * Arrays sind Sammlungen fester Groesse,
     * die erst spaeter in der Vorlesung drankommen werden.
     */
    private static String asHexString(byte[] bytes)
    {
        StringBuilder result = new StringBuilder(2 * bytes.length);
        for (byte b : bytes)
        {
            result.append("0123456789abcdef".charAt((b >> 4) & 15));
            result.append("0123456789abcdef".charAt(b & 15));
        }
        return result.toString();
    }

    private static final byte[] buffer = new byte[0x10000];
}
