[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Functional.Math

open System

[<Measure>]
type radians
[<Measure>]
type degrees

/// <summary>
/// Squares the given number.
/// </summary>
/// <param name="x">The number to square.</param>
/// <returns>The square of the given number.</returns>
let inline square x = x * x

/// <summary>
/// Returns the distance between two points.
/// Always use <see cref="distSquared"/> when possible as it is more efficient.
/// </summary>
/// <param name="x1">The x coordinate of the first point.</param>
/// <param name="y1">The y coordinate of the first point.</param>
/// <param name="x2">The x coordinate of the second point.</param>
/// <param name="y2">The y coordinate of the second point.</param>
/// <returns>The distance between the two specified points.</returns>
let inline dist (x1, y1) (x2, y2) =
    square (x2 - x1) + square (y2 - y1)
    |> float
    |> sqrt
/// <summary>
/// Returns the square of the distance between two points.
/// Always use <see cref="distSquared"/> when possible as it is more efficient.
/// </summary>
/// <param name="x1">The x coordinate of the first point.</param>
/// <param name="y1">The y coordinate of the first point.</param>
/// <param name="x2">The x coordinate of the second point.</param>
/// <param name="y2">The y coordinate of the second point.</param>
/// <returns>The square of the distance between the two specified points.</returns>
let inline distSquared x1 y1 x2 y2 =
    square (x2 - x1) + square (y2 - y1)
    |> float

/// <summary>
/// Returns a pair of the minimum and the maximum.
/// The first element is the minimum and the second element is the maximum.
/// </summary>
/// <param name="a">The first value to compare.</param>
/// <param name="b">The second value to compare.</param>
/// <returns>A pairing of the smaller of the two values and the larger, in that order.</returns>
let inline minMax a b =
    if a < b then a, b else b, a

/// <summary>
/// Returns the angle between the two specified points.
/// </summary>
/// <param name="x1">The x coordinate of the first point.</param>
/// <param name="y1">The y coordinate of the first point.</param>
/// <param name="x2">The x coordinate of the second point.</param>
/// <param name="y2">The y coordinate of the second point.</param>
/// <returns>The angle between the two specified points.</returns>
let inline getRotation (x1: 'n, y1: 'n) (x2: 'n, y2: 'n): float<radians> =
    atan2
        (float x2 - float x1)
        (float y1 - float y2)
    |> LanguagePrimitives.FloatWithMeasure

/// <summary>
/// Computes the greatest common divisor of two numbers.
/// </summary>
/// <param name="a">The first number.</param>
/// <param name="b">The second number.</param>
/// <returns>The greatest common divisor of the two specified numbers.</returns>
let rec inline gcd a b =
    let mutable a = a
    let mutable b = b

    while a <> 0G && b <> 0G do
        let na, nb = b, a % b

        a <- na
        b <- nb

    if a = 0G then b else a

/// The Fibonacci sequence.
let fibonacci = Seq.unfold (fun (last, next) -> Some (last, (next, last + next))) (0I, 1I)

/// The first one thousand primes precomputed because most use cases don't need more than this.
let private primeTable = ResizeArray([|
    2I; 3I; 5I; 7I; 11I; 13I; 17I; 19I; 23I; 29I; 31I; 37I; 41I; 43I; 47I; 53I; 59I; 61I; 67I; 71I;
    73I; 79I; 83I; 89I; 97I; 101I; 103I; 107I; 109I; 113I; 127I; 131I; 137I; 139I; 149I; 151I; 157I; 163I; 167I; 173I;
    179I; 181I; 191I; 193I; 197I; 199I; 211I; 223I; 227I; 229I; 233I; 239I; 241I; 251I; 257I; 263I; 269I; 271I; 277I; 281I;
    283I; 293I; 307I; 311I; 313I; 317I; 331I; 337I; 347I; 349I; 353I; 359I; 367I; 373I; 379I; 383I; 389I; 397I; 401I; 409I;
    419I; 421I; 431I; 433I; 439I; 443I; 449I; 457I; 461I; 463I; 467I; 479I; 487I; 491I; 499I; 503I; 509I; 521I; 523I; 541I;
    547I; 557I; 563I; 569I; 571I; 577I; 587I; 593I; 599I; 601I; 607I; 613I; 617I; 619I; 631I; 641I; 643I; 647I; 653I; 659I;
    661I; 673I; 677I; 683I; 691I; 701I; 709I; 719I; 727I; 733I; 739I; 743I; 751I; 757I; 761I; 769I; 773I; 787I; 797I; 809I;
    811I; 821I; 823I; 827I; 829I; 839I; 853I; 857I; 859I; 863I; 877I; 881I; 883I; 887I; 907I; 911I; 919I; 929I; 937I; 941I;
    947I; 953I; 967I; 971I; 977I; 983I; 991I; 997I; 1009I; 1013I; 1019I; 1021I; 1031I; 1033I; 1039I; 1049I; 1051I; 1061I; 1063I; 1069I;
    1087I; 1091I; 1093I; 1097I; 1103I; 1109I; 1117I; 1123I; 1129I; 1151I; 1153I; 1163I; 1171I; 1181I; 1187I; 1193I; 1201I; 1213I; 1217I; 1223I;
    1229I; 1231I; 1237I; 1249I; 1259I; 1277I; 1279I; 1283I; 1289I; 1291I; 1297I; 1301I; 1303I; 1307I; 1319I; 1321I; 1327I; 1361I; 1367I; 1373I;
    1381I; 1399I; 1409I; 1423I; 1427I; 1429I; 1433I; 1439I; 1447I; 1451I; 1453I; 1459I; 1471I; 1481I; 1483I; 1487I; 1489I; 1493I; 1499I; 1511I;
    1523I; 1531I; 1543I; 1549I; 1553I; 1559I; 1567I; 1571I; 1579I; 1583I; 1597I; 1601I; 1607I; 1609I; 1613I; 1619I; 1621I; 1627I; 1637I; 1657I;
    1663I; 1667I; 1669I; 1693I; 1697I; 1699I; 1709I; 1721I; 1723I; 1733I; 1741I; 1747I; 1753I; 1759I; 1777I; 1783I; 1787I; 1789I; 1801I; 1811I;
    1823I; 1831I; 1847I; 1861I; 1867I; 1871I; 1873I; 1877I; 1879I; 1889I; 1901I; 1907I; 1913I; 1931I; 1933I; 1949I; 1951I; 1973I; 1979I; 1987I;
    1993I; 1997I; 1999I; 2003I; 2011I; 2017I; 2027I; 2029I; 2039I; 2053I; 2063I; 2069I; 2081I; 2083I; 2087I; 2089I; 2099I; 2111I; 2113I; 2129I;
    2131I; 2137I; 2141I; 2143I; 2153I; 2161I; 2179I; 2203I; 2207I; 2213I; 2221I; 2237I; 2239I; 2243I; 2251I; 2267I; 2269I; 2273I; 2281I; 2287I;
    2293I; 2297I; 2309I; 2311I; 2333I; 2339I; 2341I; 2347I; 2351I; 2357I; 2371I; 2377I; 2381I; 2383I; 2389I; 2393I; 2399I; 2411I; 2417I; 2423I;
    2437I; 2441I; 2447I; 2459I; 2467I; 2473I; 2477I; 2503I; 2521I; 2531I; 2539I; 2543I; 2549I; 2551I; 2557I; 2579I; 2591I; 2593I; 2609I; 2617I;
    2621I; 2633I; 2647I; 2657I; 2659I; 2663I; 2671I; 2677I; 2683I; 2687I; 2689I; 2693I; 2699I; 2707I; 2711I; 2713I; 2719I; 2729I; 2731I; 2741I;
    2749I; 2753I; 2767I; 2777I; 2789I; 2791I; 2797I; 2801I; 2803I; 2819I; 2833I; 2837I; 2843I; 2851I; 2857I; 2861I; 2879I; 2887I; 2897I; 2903I;
    2909I; 2917I; 2927I; 2939I; 2953I; 2957I; 2963I; 2969I; 2971I; 2999I; 3001I; 3011I; 3019I; 3023I; 3037I; 3041I; 3049I; 3061I; 3067I; 3079I;
    3083I; 3089I; 3109I; 3119I; 3121I; 3137I; 3163I; 3167I; 3169I; 3181I; 3187I; 3191I; 3203I; 3209I; 3217I; 3221I; 3229I; 3251I; 3253I; 3257I;
    3259I; 3271I; 3299I; 3301I; 3307I; 3313I; 3319I; 3323I; 3329I; 3331I; 3343I; 3347I; 3359I; 3361I; 3371I; 3373I; 3389I; 3391I; 3407I; 3413I;
    3433I; 3449I; 3457I; 3461I; 3463I; 3467I; 3469I; 3491I; 3499I; 3511I; 3517I; 3527I; 3529I; 3533I; 3539I; 3541I; 3547I; 3557I; 3559I; 3571I;
    3581I; 3583I; 3593I; 3607I; 3613I; 3617I; 3623I; 3631I; 3637I; 3643I; 3659I; 3671I; 3673I; 3677I; 3691I; 3697I; 3701I; 3709I; 3719I; 3727I;
    3733I; 3739I; 3761I; 3767I; 3769I; 3779I; 3793I; 3797I; 3803I; 3821I; 3823I; 3833I; 3847I; 3851I; 3853I; 3863I; 3877I; 3881I; 3889I; 3907I;
    3911I; 3917I; 3919I; 3923I; 3929I; 3931I; 3943I; 3947I; 3967I; 3989I; 4001I; 4003I; 4007I; 4013I; 4019I; 4021I; 4027I; 4049I; 4051I; 4057I;
    4073I; 4079I; 4091I; 4093I; 4099I; 4111I; 4127I; 4129I; 4133I; 4139I; 4153I; 4157I; 4159I; 4177I; 4201I; 4211I; 4217I; 4219I; 4229I; 4231I;
    4241I; 4243I; 4253I; 4259I; 4261I; 4271I; 4273I; 4283I; 4289I; 4297I; 4327I; 4337I; 4339I; 4349I; 4357I; 4363I; 4373I; 4391I; 4397I; 4409I;
    4421I; 4423I; 4441I; 4447I; 4451I; 4457I; 4463I; 4481I; 4483I; 4493I; 4507I; 4513I; 4517I; 4519I; 4523I; 4547I; 4549I; 4561I; 4567I; 4583I;
    4591I; 4597I; 4603I; 4621I; 4637I; 4639I; 4643I; 4649I; 4651I; 4657I; 4663I; 4673I; 4679I; 4691I; 4703I; 4721I; 4723I; 4729I; 4733I; 4751I;
    4759I; 4783I; 4787I; 4789I; 4793I; 4799I; 4801I; 4813I; 4817I; 4831I; 4861I; 4871I; 4877I; 4889I; 4903I; 4909I; 4919I; 4931I; 4933I; 4937I;
    4943I; 4951I; 4957I; 4967I; 4969I; 4973I; 4987I; 4993I; 4999I; 5003I; 5009I; 5011I; 5021I; 5023I; 5039I; 5051I; 5059I; 5077I; 5081I; 5087I;
    5099I; 5101I; 5107I; 5113I; 5119I; 5147I; 5153I; 5167I; 5171I; 5179I; 5189I; 5197I; 5209I; 5227I; 5231I; 5233I; 5237I; 5261I; 5273I; 5279I;
    5281I; 5297I; 5303I; 5309I; 5323I; 5333I; 5347I; 5351I; 5381I; 5387I; 5393I; 5399I; 5407I; 5413I; 5417I; 5419I; 5431I; 5437I; 5441I; 5443I;
    5449I; 5471I; 5477I; 5479I; 5483I; 5501I; 5503I; 5507I; 5519I; 5521I; 5527I; 5531I; 5557I; 5563I; 5569I; 5573I; 5581I; 5591I; 5623I; 5639I;
    5641I; 5647I; 5651I; 5653I; 5657I; 5659I; 5669I; 5683I; 5689I; 5693I; 5701I; 5711I; 5717I; 5737I; 5741I; 5743I; 5749I; 5779I; 5783I; 5791I;
    5801I; 5807I; 5813I; 5821I; 5827I; 5839I; 5843I; 5849I; 5851I; 5857I; 5861I; 5867I; 5869I; 5879I; 5881I; 5897I; 5903I; 5923I; 5927I; 5939I;
    5953I; 5981I; 5987I; 6007I; 6011I; 6029I; 6037I; 6043I; 6047I; 6053I; 6067I; 6073I; 6079I; 6089I; 6091I; 6101I; 6113I; 6121I; 6131I; 6133I;
    6143I; 6151I; 6163I; 6173I; 6197I; 6199I; 6203I; 6211I; 6217I; 6221I; 6229I; 6247I; 6257I; 6263I; 6269I; 6271I; 6277I; 6287I; 6299I; 6301I;
    6311I; 6317I; 6323I; 6329I; 6337I; 6343I; 6353I; 6359I; 6361I; 6367I; 6373I; 6379I; 6389I; 6397I; 6421I; 6427I; 6449I; 6451I; 6469I; 6473I;
    6481I; 6491I; 6521I; 6529I; 6547I; 6551I; 6553I; 6563I; 6569I; 6571I; 6577I; 6581I; 6599I; 6607I; 6619I; 6637I; 6653I; 6659I; 6661I; 6673I;
    6679I; 6689I; 6691I; 6701I; 6703I; 6709I; 6719I; 6733I; 6737I; 6761I; 6763I; 6779I; 6781I; 6791I; 6793I; 6803I; 6823I; 6827I; 6829I; 6833I;
    6841I; 6857I; 6863I; 6869I; 6871I; 6883I; 6899I; 6907I; 6911I; 6917I; 6947I; 6949I; 6959I; 6961I; 6967I; 6971I; 6977I; 6983I; 6991I; 6997I;
    7001I; 7013I; 7019I; 7027I; 7039I; 7043I; 7057I; 7069I; 7079I; 7103I; 7109I; 7121I; 7127I; 7129I; 7151I; 7159I; 7177I; 7187I; 7193I; 7207I;
    7211I; 7213I; 7219I; 7229I; 7237I; 7243I; 7247I; 7253I; 7283I; 7297I; 7307I; 7309I; 7321I; 7331I; 7333I; 7349I; 7351I; 7369I; 7393I; 7411I;
    7417I; 7433I; 7451I; 7457I; 7459I; 7477I; 7481I; 7487I; 7489I; 7499I; 7507I; 7517I; 7523I; 7529I; 7537I; 7541I; 7547I; 7549I; 7559I; 7561I;
    7573I; 7577I; 7583I; 7589I; 7591I; 7603I; 7607I; 7621I; 7639I; 7643I; 7649I; 7669I; 7673I; 7681I; 7687I; 7691I; 7699I; 7703I; 7717I; 7723I;
    7727I; 7741I; 7753I; 7757I; 7759I; 7789I; 7793I; 7817I; 7823I; 7829I; 7841I; 7853I; 7867I; 7873I; 7877I; 7879I; 7883I; 7901I; 7907I; 7919I
|])
let private minFloatBigInt = bigint Double.MinValue
let private maxFloatBigInt = bigint Double.MaxValue

let rec private computeNextPrime () =
    let mutable current = primeTable[primeTable.Count - 1] + 2I
    let mutable foundPrime = false
    
    while not foundPrime do
        if isPrime current then
            primeTable.Add current
            foundPrime <- true
        else
            current <- current + 2I

/// <summary>
/// Determines if the specified number is prime.
/// </summary>
/// <param name="number">The number to check the primeness of.</param>
/// <returns>Whether the specified number is prime.</returns>
and isPrime (number: bigint) =
    let mutable isPrime = true
    let limit =
        if number > minFloatBigInt && number < maxFloatBigInt then
            sqrt (float number) |> bigint |> (+) 1I
        else
            number / 2I

    let mutable factorIndex = 0
    
    while isPrime && primeTable[factorIndex] < limit do
        if number % primeTable[factorIndex] = 0I then
            isPrime <- false
        
        factorIndex <- factorIndex + 1
        
        if isPrime && factorIndex >= primeTable.Count then
            computeNextPrime()
    
    isPrime
 
/// The sequence of all prime numbers.
let primes = Seq.initInfinite(fun index ->
    while index >= primeTable.Count do
        computeNextPrime()
    
    primeTable[index]
)