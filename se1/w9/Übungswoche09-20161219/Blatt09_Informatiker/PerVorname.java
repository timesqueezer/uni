/**
 * Vergleicht zwei Personen anhand ihres Vornames.
 */
class PerVorname implements Vergleicher
{
    /**
     * @see Vergleicher.vergleiche
     */
    public int vergleiche(Person a, Person b)
    {
        return a.gibVorname().compareTo(b.gibVorname());
    }
}
