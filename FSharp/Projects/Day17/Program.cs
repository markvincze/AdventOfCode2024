//int[] expectedOutput = [2, 4, 1, 5, 7, 5, 1, 6, 0, 3, 4, 1, 5, 5, 3, 0];
//2, 4, 1, 5, 7, 5, 1, 6, 0, 3, 4, 1, 5, 5, 3, 0
//5, 2, 4, 1, 5, 7, 5, 1, 6, 0, 3, 4, 1, 5, 5, 3, 0
//Console.WriteLine("Length: {0}", expectedOutput.Length);

//TestSol();

//return;

var result = FindSol();

Console.WriteLine("Result: {0}", result);
return;
// Last attempt: 530250000000
// Last attempt: 603680000000
// Last attempt: 35184372088832

IEnumerable<long> Range(long start, int count)
{
    for (long i = start; i < start + count; i++)
    {
        yield return i;
    }
}

int[] expectedOutput = [2, 4, 1, 5, 7, 5, 1, 6, 0, 3, 4, 1, 5, 5, 3, 0];

var options = new List<long> { 0 };
List<long> prevOptions = null;

foreach (var output in expectedOutput.Reverse())
{
    prevOptions = options;
    Console.WriteLine("Finding solutions for output {0}, option count: {1}", output, options.Count);
    options = options
        .SelectMany(opt => Range(opt, 8).Where(o => CalcOutput(o) == output))
        .Select(opt => opt * 8)
        .ToList();
}

foreach (var opt in prevOptions)
{
    Console.WriteLine(opt);
}
return;
foreach (var a in Enumerable.Range(0, 8))
{
    Console.WriteLine("A: {0}, Output: {1}", a, CalcOutput(a));
}

foreach (var a in Enumerable.Range(24, 8))
{
    Console.WriteLine("A: {0}, Output: {1}", a, CalcOutput(a));
}

foreach (var a in Enumerable.Range(192, 8))
{
    Console.WriteLine("A: {0}, Output: {1}", a, CalcOutput(a));
}

long CalcOutput(long a)
{
    var b = 0L;
    var c = 0L;

    b = a % 8; // 2,4
    b = b ^ 5; // 1,5, XOR 00000101. 0 -> 5, 1 -> 4, 2 -> 7, 3 -> 6, 4 -> 1, 5 -> 0, 6 -> 3, 7 -> 2
    c = a / (long)Math.Pow(2, b); // 7,5
    b = b ^ 6; // 1,6, XOR 00000110
    a = a / 8; // 0,3
    b = b ^ c; // 4,1

    return b % 8;
}

long FindSol()
{
    int[] expectedOutput = [2, 4, 1, 5, 7, 5, 1, 6, 0, 3, 4, 1, 5, 5, 3, 0];
    //var aInit = 35184372088832L;
    //var aInit = 847849244544200L;
    var aInit = 105981155568020L;

    while (true)
    {
        if (aInit % 10000000 == 0)
        {
            Console.WriteLine("Trying with {0}", aInit);
        }

        var a = aInit;
        var b = 0L;
        var c = 0L;

        var outCnt = 0;

        while (true)
        {
            b = a % 8; // 2,4
            b = b ^ 5; // 1,5, XOR 00000101. 0 -> 5, 1 -> 4, 2 -> 7, 3 -> 6, 4 -> 1, 5 -> 0, 6 -> 3, 7 -> 2
            c = a / (long)Math.Pow(2, b); // 7,5
            b = b ^ 6; // 1,6, XOR 00000110
            a = a / 8; // 0,3
            b = b ^ c; // 4,1

            var o = b % 8;

            // 

            if (outCnt > expectedOutput.Length - 1)
            {
                //Console.WriteLine("Produced too much output");
                break;
            }
            else if (o != expectedOutput[outCnt])
            {
                //Console.WriteLine("Output did not match");
                break;
            }

            outCnt++;

            if (a == 0)
            {
                if (outCnt == expectedOutput.Length)
                {
                    return aInit;
                }
                else
                {
                    //Console.WriteLine("Produced not enought output");
                    break;
                }
            }
        }

        aInit++;
    }
}

void TestSol()
{
    var a = 847849244544208L;
    var b = 0L;
    var c = 0L;

    var output = new List<long>();

    while (true)
    {
        b = a % 8; // 2,4
        b = b ^ 5; // 1,5
        c = a / (long)Math.Pow(2, b); // 7,5
        b = b ^ 6; // 1,6
        a = a / 8; // 0,3
        b = b ^ c; // 4,1

        var o = b % 8;

        output.Add(o);

        if (a == 0)
        {
            Console.WriteLine("Result: {0}", String.Join(",", output));
            return;
        }
    }
}
