
/**
 * Beschreiben Sie hier die Klasse ListenTimer.
 * 
 * @author (Ihr Name) 
 * @version (eine Versionsnummer oder ein Datum)
 */
class ListenTimer
{
    private LinkedTitelListe _linkedList;
    private ArrayTitelListe _arrayList;
    private Titel[] _testTitel;
    
    public ListenTimer()
    {
        _linkedList = new LinkedTitelListe();
        _arrayList = new ArrayTitelListe();
        
        TitelBibliothek bibliothek = new TitelBibliothek("JazzMix.txt");
        _testTitel = bibliothek.gibZufaelligeTitel(20);
        
        for (int i = 0; i < 100000; ++i)
        {
            _linkedList.fuegeEin(_testTitel[i%20], _linkedList.gibLaenge());
            _arrayList.fuegeEin(_testTitel[i%20], _arrayList.gibLaenge());
        }
    }
    
    public void measurePerformance()
    {
        System.out.println("Testing insertion at the beginning.");
        
        long[] measurements = new long[10];
        
        for (int i = 0; i < 10; ++i)
        {
            long time = System.nanoTime();
            _linkedList.fuegeEin(_testTitel[i], 0);
            
            long nextTime = System.nanoTime();
            measurements[i] = nextTime - time;
        }
        
        long average = 0;
        double msAverage = 0;
        for (int i = 0; i < 10; ++i) {
            average += measurements[i];
        }
        average /= 10;
        msAverage = (double) average / (double) 1000; // ms

        System.out.println("LINKED LIST: average: " + msAverage + "us");
        
        measurements = new long[10];
        
        for (int i = 0; i < 10; ++i)
        {
            long time = System.nanoTime();
            _arrayList.fuegeEin(_testTitel[i], 0);
            
            long nextTime = System.nanoTime();
            measurements[i] = nextTime - time;
        }
        
        average = 0;
        for (int i = 0; i < 10; ++i) {
            average += measurements[i];
        }
        average /= 10;
        msAverage = (double)average / 1000; // ms

        System.out.println("ARRAY LIST: average: " + msAverage + "us");
        
        System.out.println("\nTesting insertion at the end.");
        measurements = new long[10];
        
        for (int i = 0; i < 10; ++i)
        {
            long time = System.nanoTime();
            _linkedList.fuegeEin(_testTitel[i], _linkedList.gibLaenge());
            
            long nextTime = System.nanoTime();
            measurements[i] = nextTime - time;
        }
        
        average = 0;
        for (int i = 0; i < 10; ++i) {
            average += measurements[i];
        }
        average /= 10;
        msAverage = (double)average / 1000; // ms
        
        System.out.println("LINKED LIST: average: " + msAverage + "us");
        measurements = new long[10];
        
        for (int i = 0; i < 10; ++i)
        {
            long time = System.nanoTime();
            _arrayList.fuegeEin(_testTitel[i], _arrayList.gibLaenge());
            
            long nextTime = System.nanoTime();
            measurements[i] = nextTime - time;
        }
        
        average = 0;
        for (int i = 0; i < 10; ++i) {
            average += measurements[i];
        }
        average /= 10;
        msAverage = (double)average / 1000; // ms

        System.out.println("ARRAY LIST: average: " + msAverage + "us");


        System.out.println("\nTesting get middle element.");
        measurements = new long[10];
        
        for (int i = 0; i < 10; ++i)
        {
            long time = System.nanoTime();
            _linkedList.gibTitel(_linkedList.gibLaenge() / 2);
            
            long nextTime = System.nanoTime();
            measurements[i] = nextTime - time;
        }
        
        average = 0;
        for (int i = 0; i < 10; ++i) {
            average += measurements[i];
        }
        average /= 10;
        msAverage = (double)average / 1000; // ms

        System.out.println("LINKED LIST: average: " + msAverage + "us");
        measurements = new long[10];
        
        for (int i = 0; i < 10; ++i)
        {
            long time = System.nanoTime();
            _arrayList.gibTitel(_arrayList.gibLaenge() / 2);
            
            long nextTime = System.nanoTime();
            measurements[i] = nextTime - time;
        }
        
        average = 0;
        for (int i = 0; i < 10; ++i) {
            average += measurements[i];
        }
        average /= 10;
        msAverage = (double)average / 1000; // ms

        System.out.println("ARRAY LIST: average: " + msAverage + "us");
        
    }
}

