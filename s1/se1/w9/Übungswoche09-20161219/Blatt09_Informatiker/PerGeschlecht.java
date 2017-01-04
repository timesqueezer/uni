/**
 * Vergleicht zwei Personen anhand ihres Geschlechts.
 */
class PerGeschlecht implements Vergleicher
{
    /**
     * @see Vergleicher.vergleiche
     */
    public int vergleiche(Person a, Person b)
    {
        return Boolean.compare(a.istMaennlich(), b.istMaennlich());
    }
}
