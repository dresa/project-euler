;; Solutions to Project Euler problems
;; http://projecteuler.net/problems
;;
;; Esa Junttila 20.8.2012

(ns euler)

;; (set! *warn-on-reflection* true)

(defn divisible-by? [x y]
	"Returns true if x is divisible by y; otherwise false."
	(zero? (rem x y)))

(defn divides? [x y]
	"Returns true if x divides y; otherwise false."
	(divisible-by? y x))

(defn euler-1 []
	"Solves the first Euler problem. Solution: 233168"
	(reduce + (for [i (range 1000) :when (or
		(divisible-by? i 3)
		(divisible-by? i 5))] i)))


(defn fibo-one []
	"A lazy Fibonacci sequence starting with 1 and 2."
	(map first (iterate (fn [[a b]] [b (+' a b)]) [1 2])))

(defn euler-2 []
	"Solves the second Euler problem. Solution: 4613732"
	(reduce + (filter #(divisible-by? % 2) (take-while #(<= % 4000000) (fibo-one)))))


(defn prime-factors [n]
	"Returns the prime factors for the given positive integer. Slow."
	(if (< n 2)
		nil
		(loop [factors [], divisor 2, num n]
			(if (> num 1)
				(if (divisible-by? num divisor)
					(recur (conj factors divisor) divisor (/ num divisor))
					(recur factors (inc divisor) num)
				)
				factors))))

(defn euler-3 []
	"Solves the third Euler problem. Solution: 6857"
	(reduce max (prime-factors 600851475143)))


(defmulti palindrome?
	"Is the sequence a palindrome -- that is, same as it's reverse?"
	class)
(defmethod palindrome? String [s]
	(= (seq s) (reverse s)))
(defmethod palindrome? Integer [i]
	(palindrome? (str i)))
(defmethod palindrome? Long [L]
	(palindrome? (str L)))

(defn euler-4 []
	"Solves Problem 4. Solution: 906609"
	(let [nums (range 1000)]
		(reduce max (for [n nums, m nums :when (palindrome? (* n m))] (* n m)))))


(defn divisible-by-all? [x s]
	"Returns true if x is divisible by all numbers in s; otherwise false"
	(every? #(divisible-by? x %) s))

(defn smallest-dividable [s]
	"Given a seq of dividers, returns the smallest number divisible by them. SLOW!"
	(let [m (reduce max s)]
		(some #(if (divisible-by-all? % s) %) (iterate #(+ m %) m))))

(defn euler-5 []
	"Solves Problem 5, slowly. Solution: 232792560"
	(smallest-dividable (range 2 21))
)


(defn sum-up-to [n]
	"Return the sum of nonnegative integeres up to n."
	(if (and (integer? n) (<= 0 n))
		(/ (* n (inc n)) 2)))

(defn sum-from-to [start end]
	"The sum of nonnegative integers from start up to end."
	(if (<= start end)
		(-
			(sum-up-to end)
			(if (zero? start) 0 (sum-up-to (dec start))))))

(defn euler-6 []
	"Solves Problem 6. Solution: 25164150"
	(let [
		nums (range 1 101)
		sum (reduce + nums)
		sum-squared (* sum sum)
		squared-sum (reduce + (map #(* % %) nums))]
		(- sum-squared squared-sum)))


(defn next-prime [s]
	"Based on prime numbers up to a point, deduce the next prime number. SLOW"
	(let [nums (iterate inc (last s))]  ; vec assumed, last is same as maximum
		(some
			(fn [cand] (if (not-any? #(divisible-by? cand %) s) cand))
			nums)))

(defn nth-prime [n]
	"Returns the nth prime number (SLOW)."
	(if (= n 1)
		2
		(loop [old-primes [2]]
			(let [p (next-prime old-primes)]
				(if (= (count old-primes) (dec n))
					p
					(recur (conj old-primes p)))))))

(defn euler-7 []
	"Solves Problem 7. Solution: 104743"
	(nth-prime 10001))


(def num8
"7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")

(defn pick-str-number [^String numstring index]
	(-> numstring (.substring index (inc index)) (Integer/parseInt)))

(defn consecutive-num-product [start end]
	(reduce * (for [i (range start end)] (pick-str-number num8 i)))
)

(defn euler-8 []
	"Solves Problem 8. Solution: 40824"
	(let [len 5]
		(reduce
			max
			(map
				#(consecutive-num-product % (+ % 5))
				(range (- (count num8) len))))))


(defn get-pythagoras [a b]
	"Given a and b, return the length c of hyphotenuse using Pythagoras' theorem."
	(Math/sqrt (+ (* a a) (* b b))))

(defn get-pythagoras-int-c [a b]
	"Return an int hyphotenuse c for given a and b, if one exists; otherwise nil."
	(let [c (get-pythagoras a b)]
		(if (zero? (- c (int c)))
			(int c)
			nil)))

(defn euler-9 []
	"Solves Problem 9. Solution: 31875000 (a=200, b=375, c=425)"
	(let [
		n 1000
		nums (range 1 n)
		cand (for [a nums, b nums :when (< (+ a b) n)] (list a b))]
		(some
			(fn [[a b]]
				(let [c (get-pythagoras-int-c a b)]
					(if (and c (= (+ a b c) n)) (* a b c))
				)
			)
			cand)))


(defn get-next-prime [old-primes]
	"Based on prime numbers up to a point, deduce the next prime number. SLOW"
	(let [
		max-old-prime (first old-primes)
		next-cand-fn #(+ % 2)
		first-cand (next-cand-fn max-old-prime)
		all-cand (iterate next-cand-fn first-cand)]
		(some
			(fn [cand] (if (not-any? #(divisible-by? cand %) old-primes) cand))
			all-cand)))

;(defn primes
;	"Returns a lazy sequence of prime numbers. (subroutine is slow)"
;	([] (cons 2 (cons 3 (primes (list 3)))))
;	([old-primes]
;		(lazy-seq
;			(let [p (get-next-prime old-primes)]
;				(cons p (primes (conj old-primes p)))))))



; There is a bug: with n=1000 the sieve returns also 961 (=31*31) which is not prime.
(defn sieve [n]
  (let [n (int n)]
    "Returns a list of all primes from 2 to n. From: http://paste.lisp.org/display/69952"
    (let [root (int (Math/round (Math/floor (Math/sqrt n))))]
      (loop [i (int 3)
             a (int-array n)
             result (list 2)]
        (if (>= i n)
          (reverse result)
          (recur (+ i (int 2))
                 (if (<= i root)  ; Equality by Esa. n=1000->961 not prime.
                   (loop [arr a
                          inc (+ i i)
                          j (* i i)]
                     (if (>= j n)
                       arr
                       (recur (do (aset arr j (int 1)) arr)
                              inc
                              (+ j inc))))
                   a)
                 (if (zero? (aget a i))
                   (conj result i)
                   result)))))))

(defn euler-10 []
	"Solves Problem 10. Solution: 142913828922"
	(reduce + (sieve 2000000)))


(def nums-eleven
	[
[ 8  2 22 97 38 15  0 40  0 75  4  5  7 78 52 12 50 77 91  8]
[49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48  4 56 62  0]
[81 49 31 73 55 79 14 29 93 71 40 67 53 88 30  3 49 13 36 65]
[52 70 95 23  4 60 11 42 69 24 68 56  1 32 56 71 37  2 36 91]
[22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80]
[24 47 32 60 99  3 45  2 44 75 33 53 78 36 84 20 35 17 12 50]
[32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70]
[67 26 20 68  2 62 12 20 95 63 94 39 63  8 40 91 66 49 94 21]
[24 55 58  5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72]
[21 36 23  9 75  0 76 44 20 45 35 14  0 61 33 97 34 31 33 95]
[78 17 53 28 22 75 31 67 15 94  3 80  4 62 16 14  9 53 56 92]
[16 39  5 42 96 35 31 47 55 58 88 24  0 17 54 24 36 29 85 57]
[86 56  0 48 35 71 89  7  5 44 44 37 44 60 21 58 51 54 17 58]
[19 80 81 68  5 94 47 69 28 73 92 13 86 52 17 77  4 89 55 40]
[ 4 52  8 83 97 35 99 16  7 97 57 32 16 26 26 79 33 27 98 66]
[88 36 68 87 57 62 20 72  3 46 33 67 46 55 12 32 63 93 53 69]
[ 4 42 16 73 38 25 39 11 24 94 72 18  8 46 29 32 40 62 76 36]
[20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74  4 36 16]
[20 73 35 29 78 31 90  1 74 31 49 71 48 86 81 16 23 57  5 54]
[ 1 70 54 71 83 51 54 69 16 92 33 48 61 43 52  1 89 19 67 48]
	]
)

(defn pad-zeros-right [matrix]  ;moves secondary diagonal lines into vertical lines
	(let [
		zeros (repeat 0)
		n-rows (count matrix)]
		(map-indexed
			(fn [index row]
				(vec (flatten [(take index zeros) row (take (- n-rows (inc index)) zeros)]))
			)
			matrix)))

(defn pad-zeros-left [matrix]  ;moves main diagonal lines into vertical lines
	(let [
		zeros (repeat 0)
		n-rows (count matrix)]
		(map-indexed
			(fn [index row]
				(vec (flatten [(take (- n-rows (inc index)) zeros) row (take index zeros)]))
			)
			matrix)))

(defn compute-max-4-product [matrix]
	(reduce
		max  ;choose maximum from row-wise maximum products
		(map  ; for all rows a the matrix
			(fn [row] (reduce max (map #(reduce * %) (partition 4 1 row))))  ;compute maximum product from 4-sequences
			matrix)))

(defn generate-matrix-rotations [matrix]
	[
	matrix  ;matrix as is (horizontal search)
	(vec (apply map vector matrix))  ;transpose (vertical search)
	(vec (apply map vector (pad-zeros-right matrix))) ;secondary diagonal search
	(vec (apply map vector (pad-zeros-left matrix)))  ;main diagonal search
	]
)

(defn euler-11 []
	"Solves Problem 11. Solution 70600674."
	(reduce max (map compute-max-4-product (generate-matrix-rotations nums-eleven)))
)


(defn triangle-numbers
	([] (triangle-numbers 0 1))
	([^long tri, ^long n]
		(let [n-tri (+ tri n)]
		(cons n-tri (lazy-seq (triangle-numbers n-tri (inc n)))))))

(defn num-dividers [^long n]
	(* 2 (count (filter #(divisible-by? n %) (range 1 (Math/sqrt n))))))

(defn euler-12 []
	"Solves Problem 12. Solution 76576500"
	(some #(if (> (num-dividers %) 500) %) (triangle-numbers)))


(def bignums13
	[
37107287533902102798797998220837590246510135740250N
46376937677490009712648124896970078050417018260538N
74324986199524741059474233309513058123726617309629N
91942213363574161572522430563301811072406154908250N
23067588207539346171171980310421047513778063246676N
89261670696623633820136378418383684178734361726757N
28112879812849979408065481931592621691275889832738N
44274228917432520321923589422876796487670272189318N
47451445736001306439091167216856844588711603153276N
70386486105843025439939619828917593665686757934951N
62176457141856560629502157223196586755079324193331N
64906352462741904929101432445813822663347944758178N
92575867718337217661963751590579239728245598838407N
58203565325359399008402633568948830189458628227828N
80181199384826282014278194139940567587151170094390N
35398664372827112653829987240784473053190104293586N
86515506006295864861532075273371959191420517255829N
71693888707715466499115593487603532921714970056938N
54370070576826684624621495650076471787294438377604N
53282654108756828443191190634694037855217779295145N
36123272525000296071075082563815656710885258350721N
45876576172410976447339110607218265236877223636045N
17423706905851860660448207621209813287860733969412N
81142660418086830619328460811191061556940512689692N
51934325451728388641918047049293215058642563049483N
62467221648435076201727918039944693004732956340691N
15732444386908125794514089057706229429197107928209N
55037687525678773091862540744969844508330393682126N
18336384825330154686196124348767681297534375946515N
80386287592878490201521685554828717201219257766954N
78182833757993103614740356856449095527097864797581N
16726320100436897842553539920931837441497806860984N
48403098129077791799088218795327364475675590848030N
87086987551392711854517078544161852424320693150332N
59959406895756536782107074926966537676326235447210N
69793950679652694742597709739166693763042633987085N
41052684708299085211399427365734116182760315001271N
65378607361501080857009149939512557028198746004375N
35829035317434717326932123578154982629742552737307N
94953759765105305946966067683156574377167401875275N
88902802571733229619176668713819931811048770190271N
25267680276078003013678680992525463401061632866526N
36270218540497705585629946580636237993140746255962N
24074486908231174977792365466257246923322810917141N
91430288197103288597806669760892938638285025333403N
34413065578016127815921815005561868836468420090470N
23053081172816430487623791969842487255036638784583N
11487696932154902810424020138335124462181441773470N
63783299490636259666498587618221225225512486764533N
67720186971698544312419572409913959008952310058822N
95548255300263520781532296796249481641953868218774N
76085327132285723110424803456124867697064507995236N
37774242535411291684276865538926205024910326572967N
23701913275725675285653248258265463092207058596522N
29798860272258331913126375147341994889534765745501N
18495701454879288984856827726077713721403798879715N
38298203783031473527721580348144513491373226651381N
34829543829199918180278916522431027392251122869539N
40957953066405232632538044100059654939159879593635N
29746152185502371307642255121183693803580388584903N
41698116222072977186158236678424689157993532961922N
62467957194401269043877107275048102390895523597457N
23189706772547915061505504953922979530901129967519N
86188088225875314529584099251203829009407770775672N
11306739708304724483816533873502340845647058077308N
82959174767140363198008187129011875491310547126581N
97623331044818386269515456334926366572897563400500N
42846280183517070527831839425882145521227251250327N
55121603546981200581762165212827652751691296897789N
32238195734329339946437501907836945765883352399886N
75506164965184775180738168837861091527357929701337N
62177842752192623401942399639168044983993173312731N
32924185707147349566916674687634660915035914677504N
99518671430235219628894890102423325116913619626622N
73267460800591547471830798392868535206946944540724N
76841822524674417161514036427982273348055556214818N
97142617910342598647204516893989422179826088076852N
87783646182799346313767754307809363333018982642090N
10848802521674670883215120185883543223812876952786N
71329612474782464538636993009049310363619763878039N
62184073572399794223406235393808339651327408011116N
66627891981488087797941876876144230030984490851411N
60661826293682836764744779239180335110989069790714N
85786944089552990653640447425576083659976645795096N
66024396409905389607120198219976047599490197230297N
64913982680032973156037120041377903785566085089252N
16730939319872750275468906903707539413042652315011N
94809377245048795150954100921645863754710598436791N
78639167021187492431995700641917969777599028300699N
15368713711936614952811305876380278410754449733078N
40789923115535562561142322423255033685442488917353N
44889911501440648020369068063960672322193204149535N
41503128880339536053299340368006977710650566631954N
81234880673210146739058568557934581403627822703280N
82616570773948327592232845941706525094512325230608N
22918802058777319719839450180888072429661980811197N
77158542502016545090413245809786882778948721859617N
72107838435069186155435662884062257473692284509516N
20849603980134001723930671666823555245252804609722N
53503534226472524250874054075591789781264330331690N
	]
)

(defn euler-13 []
	"Solves Problem 13. Solution 5537376230"
	(let [sum (reduce +' bignums13)]
		(loop [s sum]
			(if (and (< s 10000000000) (>= s 1000000000))
				s
				(recur (quot s 10))))))


(defn sequence14 [init-num]
	(loop [s (list), n init-num]
		(if (= n 1)
			(conj s n)
			(recur (conj s n) (if (even? n) (/ n 2) (inc (* 3 n)))))))

(defn max-index [coll]
	"Given a collection, returns [m i], where m is the maximum item and i
	 its index. Function > is used for comparison. If the collection
	 is empty, [nil nil] is returned instead."
	(loop [c coll, idx 0, maxi nil, maxidx nil]
		(let [newmax (or (nil? maxi) (and (not (empty? c)) (> (first c) maxi)))]
			(if (empty? c)
				[maxi maxidx]
				(recur
					(rest c)
					(inc idx)
					(if newmax (first c) maxi)
					(if newmax idx maxidx))))))

(defn euler-14 []
	"Solves Problem 14. Solution 837799"
	(inc (second (max-index (map #(count (sequence14 %)) (range 1 1000000)))))
)

(defn fact [n]
	(reduce *' (range 1 (inc n))))

(defn nCr [n k]
	(/ (fact n) (*' (fact k) (fact (- n k)))))

(defn euler-15 []
	"Solves Problem 15. Solution 137846528820"
	(nCr 40 20)
	; solution is nCr((n+m) n), where n=20 and m=20.
	; The is the same as the number of nested matrices (my dissertation)!
	; There are n chances of moving right out of (n+m) down/right steps.
)

(defn digit-sum-power-2 [n]
	(let [dig (vec (flatten (list 1 (take (dec n) (repeat 0)))))]
		(loop [digits dig, n-mult n, idx 0, carry 0]
			(if (zero? n-mult)  ;all multiplications done?
				digits          ;return digits when ready
				(if (= idx n)   ; multiplication run over all indices
					(recur digits (dec n-mult) 0 0)  ;start a new multiplication
					(let [multiple (+ carry (* 2 (get digits idx)))]
						(recur  ;continue with the same multiplication
							(assoc digits idx (rem multiple 10))  ; update digit
							n-mult                     ; belongs to same multiplication
							(inc idx)                  ; move to next index
							(quot multiple 10))))))))  ; carry to next index?

(defn euler-16 []
	"Solves Problem 16. Solution 1366"
	(reduce + (digit-sum-power-2 1000)))


(def ones17 (vec (map count ["" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])))
(def teens17 (vec (map count ["ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"])))
(def tens17 (vec (map count ["" "ten" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])))
(def num-hundred17 (count "hundred"))

(defn num-word-length [n]
	(let [
		n-hundreds (quot n 100)
		n-tens (quot (rem n 100) 10)
		n-ones (rem n 10)
		]
		(+
			(if (> n-hundreds 0)
				(+
					(nth ones17 n-hundreds)  ; how may hundreds?
					num-hundred17            ; word "hundred"
					(if (> (+ n-tens n-ones) 0)  ; there are tens or ones?
						3  ;and
						0  ;no and
					)
				)
				0  ;no mention of hundreds, no and
			)
			(cond
				(and (= n-tens 0) (= n-ones 0)) 0  ; X00
				(= n-tens 0) (nth ones17 n-ones)   ; XON
				(= n-tens 1) (nth teens17 n-ones)  ; X1N
				(> n-tens 1) (+ (nth tens17 n-tens) (nth ones17 n-ones))  ; XMN
				:else (throw (Exception. "invalid number"))
			)
		)
	)
)

(defn euler-17 []
	"Solves Problem 17. Solution 21124"
	(+ (reduce + (map num-word-length (range 1 1000))) (+ (count "one") (count "thousand"))))


(def pyramid18
	[
[75 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1]
[95 64 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1]
[17 47 82 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1]
[18 35 87 10 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1]
[20  4 82 47 65 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1]
[19  1 23 75  3 34 -1 -1 -1 -1 -1 -1 -1 -1 -1]
[88  2 77 73  7 63 67 -1 -1 -1 -1 -1 -1 -1 -1]
[99 65  4 28  6 16 70 92 -1 -1 -1 -1 -1 -1 -1]
[41 41 26 56 83 40 80 70 33 -1 -1 -1 -1 -1 -1]
[41 48 72 33 47 32 37 16 94 29 -1 -1 -1 -1 -1]
[53 71 44 65 25 43 91 52 97 51 14 -1 -1 -1 -1]
[70 11 33 28 77 73 17 78 39 68 17 57 -1 -1 -1]
[91 71 52 38 17 14 91 43 58 50 27 29 48 -1 -1]
[63 66  4 68 89 53 67 30 73 16 69 87 40 31 -1]
[ 4 62 98 27 23  9 70 98 73 93 38 53 60  4 23]
	]
)

(defn choose-pyramid-path [pyramid]
	(let [
		num-vals (count (first pyramid))  ;number of values per row
		init-row (vec (take num-vals (repeat 0)))]  ;cumulative row zeroed
		(loop [cumu-row init-row, row-idx (dec (count pyramid))]  ;from bottom to top
			(if (< row-idx 0)
				(reduce max cumu-row)  ;beyond top row, return maximum value
				(let [row-vals (nth pyramid row-idx)]  ;processing a new row
					(recur
						(map-indexed  ;add the maximum cumulative value from paths
							(fn [idx val]
								(+
									(nth row-vals idx)
									(max
										(nth cumu-row idx)
										(nth cumu-row (min (inc idx) (dec num-vals))))))
							init-row)
						(dec row-idx)))))))

(defn euler-18 []
	"Solves Problem 18. Solution 1074"
	(choose-pyramid-path pyramid18))


(defn leap-year? [^long year]
	"Is a given year a leap year or not?"
	(or
		(divisible-by? year 400)
		(and
			(divisible-by? year 4)
			(not (divisible-by? year 100)))))

(def days19 {0 :mon, 1 :tue, 2 :wed, 3 :thu, 4 :fri, 5 :sat, 6 :sun})
(def months19 {1 :jan, 2 :feb, 3 :mar, 4 :apr, 5 :may, 6 :jun, 7 :jul, 8 :aug, 9 :sep, 10 :oct, 11 :nov, 12 :dec})
(def days-in-month19 {:jan 31, :feb 28, :mar 31, :apr 30, :may 31, :jun 30, :jul 31, :aug 31, :sep 30, :oct 31, :nov 30, :dec 31})

(defn compute-starting-sundays []
	(let [year 1901, month 1, last-year 2000, init-day (rem (+ 0 365) 7)]
		(loop [y year, m month, num-first-suns 0, week-day init-day]
			(let [
				is-last-month (= m 12)
				month-days (if (and (= (get months19 m) :feb) (leap-year? y))
					29
					((get months19 m) days-in-month19))
				day (rem (+ week-day month-days) 7)]
				(if (> y last-year)
					num-first-suns  ; ruturn the number of first sundays
					(recur
						(if is-last-month (inc y) y)
						(if is-last-month 1 (inc m))
						(+ num-first-suns (if (= (get days19 week-day) :sun) 1 0))
						day))))))

(defn euler-19 []
	"Solves Problem 19. Solution 171"
	(compute-starting-sundays)
)


(defn euler-20 []
	"Solves Problem 20. Solution 648"
	(loop [num (fact 100), sum-digits 0]
		(let [q (quot num 10), r (rem num 10)]
			(if (and (zero? q) (zero? r))
				sum-digits
				(recur q (+ sum-digits r))))))


(defn proper-dividers [^long n]
	(filter #(divisible-by? n %) (range 1 (inc (quot n 2)))))

(defn proper-dividers-fast [^long n]
	(let [small-divs (filter #(divisible-by? n %) (reverse (range 1 (inc (int (Math/sqrt n))))))]
		(reverse (rest (reduce #(if (not= %2 (/ n %2)) (conj %1 (/ n %2)) %1) small-divs small-divs)))))

(defn amicable-numbers [n]
	(let [sums (vec (map #(reduce + (proper-dividers %)) (range 0 (inc n))))]
		(keep-indexed
			(fn [idx val]
				(if (and (< val (count sums)) (= idx (nth sums val)) (not= idx val))
					val))
			sums)))

(defn euler-21 []
	"Solves Problem 21. Solution 31626"
	(reduce + (amicable-numbers 9999)))


(defn process-names [^String name-string]
	(reduce +
		(map-indexed
			(fn [^long idx, ^String name]
				(*
					(inc idx)
					(reduce + (map #(inc (- (int %) (int \A))) (seq name))))
			)
			(sort
				(map
					#(.substring % 1 (dec (count %)))
					(.split name-string ","))))))

(defn euler-22 []
	"Solves Problem 22. Solution 871198282"
	(process-names (slurp "names.txt")))


(defn abundant-numbers [^long n]
	"Return all abundant numbers up to n."
	(keep-indexed #(if (> %2 %1) %1) (map #(reduce + (proper-dividers-fast %)) (range 0 (inc n)))))

(defn non-abundable-numbers-slow [n]
	"SLOW! Return all numbers that cannot be expressed as a sum of two abundant numbers."
	(let [
		abundant (abundant-numbers n)
		abundable-sums (set (flatten (reduce
			(fn [coll ^long ab] (conj coll (map #(+ ab %) abundant))) (list) abundant)))]
		(filter #(not (contains? abundable-sums %)) (range 1 (inc n)))
	)
)

(defn abundable? [^long num ab-set]
	"Is a number abundable (can be expressed as a sum of two numbers in ab-set)?"
	(true? (some #(if (contains? ab-set (- num %)) true) ab-set)))

(defn non-abundable-numbers [n]
	"Return all numbers that cannot be expressed as a sum of two abundant numbers."
	(let [abundant (set (abundant-numbers n))]
		(filter (fn [^long num] (not (abundable? num abundant))) (range 1 (inc n)))))

(defn euler-23 []
	"Solves Problem 23, (in 20 seconds). Solution 4179871"
	(reduce + (non-abundable-numbers 28123)))


(use 'clojure.math.combinatorics)

(defn euler-24 []
	"Solves Problem 24. Solution 2783915460"
	(reduce #(+ (* %1 10) %2) 0 (nth (permutations '(0 1 2 3 4 5 6 7 8 9)) (dec 1000000))))


(defn euler-25 []
	"Solves Problem 25. Solution 4782"
	(+ 2 (some #(if (>= (count (str (second %))) 1000) (first %)) (map-indexed vector (fibo-one)))))


(defn find-decimal-cycle [n]
	"Decimal cycle length for number 1/n. Mathematical analysis
	 shows that when a remainder occurs again in the division
	 process, the cycle has ended."
	(loop [remainder 1, seen-rem {}, dec-index 1]
		(let [
			numerator (* 10 remainder)
			new-remainder (rem numerator n)
			already-seen (contains? seen-rem new-remainder)]
			(if already-seen
				(- dec-index (get seen-rem new-remainder))
				(recur
					new-remainder
					(assoc seen-rem new-remainder dec-index)
					(inc dec-index))))))

(defn euler-26 []
	"Solves Problem 26. Solution 983"
	(inc (second (max-index (map #(find-decimal-cycle %) (range 1 1000))))))


(defn prime-slow? [n]
	(if (or (< n 2) (divisible-by? n 2))
		false
		(= 1 (count (proper-dividers n)))))

(def prime-slow-memo? (memoize prime-slow?))

(defn num-consecutive-primes [a b]
	(count (take-while identity (map #(prime-slow-memo? (+ (* % %) (* a %) b)) (iterate inc 0)))))

(defn euler-27 []
	"Solves Problem 27. Solution -59231"
	(apply
		*
		(subvec
			(reduce
				#(if (> (first %2) (first %)) %2 %)
				[-1 -1 -1]
				(for
					[a (range -999 1000), b (range -999 1000)]
					[(num-consecutive-primes a b) a b]
				)
			)
			1
			3)))


(defn next-spiral-diagonal-value [[^long spiral-val ^long anchor]]
	(let [
		step (inc anchor),
		next-val (+ spiral-val step),
		anc-cand (+ 2 anchor)]
	[next-val, (if (= next-val (* anc-cand anc-cand)) anc-cand anchor)]))

(defn spiral-diagonal-sum [n]
	(reduce + (map first (take-while #(<= (first %) (* n n)) (iterate next-spiral-diagonal-value [1 1])))))

(defn euler-28 []
	"Solves Problem 28. Solution 669171001"
	(spiral-diagonal-sum 1001))


(defn euler-29 []
	"Solves Problem 29. Solution 9183"
	(count
		(set
			(for [a (range 2 (inc 100)), b (range 2 (inc 100))]
				(apply *' (repeat b a))))))


(defn power-summable? [n expo]
	"Is n equal to the sum of each its digits in expo:th power?"
	(loop [q n, cumu 0]
		(if (> cumu n)
			false
			(if (zero? q)
				(= n cumu)
				(recur (quot q 10) (+' cumu (apply *' (repeat expo (rem q 10)))))))))

(defn euler-30 []
	"Solves Problem 30, slowly. Solution 443839. Last number supposedly 194979."
	(reduce + (filter #(power-summable? % 5) (range 2 200000))))


(defn num-coin-comb [curr max coins]
	(cond
		(> curr max) 0
		(= curr max) 1
		(empty? coins) 0
		:else
		(+
			(num-coin-comb (+ curr (first coins)) max coins)
			(num-coin-comb curr max (rest coins)))))

(defn euler-31 []
	"Solves Problem 31. Solution 73682."
	(num-coin-comb 0 200 (list 200 100 50 20 10 5 2 1)))



(defn digits [n]
	"Return a list of the digits in a number (most significant first)."
	(map #(- (int %) (int \0)) (seq (str n))))

(defn pandigital-product? [a b c]
	(let [all (into (into (digits a) (digits b)) (digits c))]
		(and
			(= (* a b) c)  ; equality a*b=c
			(= (set all) #{1 2 3 4 5 6 7 8 9}); all digits used
			(= (count all) 9)))) ; all digits exactly once

(defn list-to-num [numlist]
	"Converts single digits in a list into a number. (most significant first.)"
	(loop [n 0, digits numlist]
		(if (empty? digits)
			n
			(recur
				(+ (* 10 n) (first digits))
				(rest digits)))))

(defn convert-part-to-nums [partition len len2]
	(let [
		a (replace partition (range len))
		b (replace partition (range len (+ len len2)))
		c (replace partition (range (+ len len2) (count partition)))
		]
		[(list-to-num a), (list-to-num b), (list-to-num c)]))

(defn gen-pandigital-from-perm [permutation a-len b-len]
	(let [
		nums (convert-part-to-nums permutation a-len b-len)
		a (first nums)
		b (second nums)
		c (last nums)]
		(if (and (= c (* a b)) (pandigital-product? a b c) )
			[a b c]
			nil)))

(defn gen-pandigital-split-from-perm [permutation]
	(let [
		coll (filter (complement nil?)
				(for [a-len (range 1 6), b-len (range 1 6)]
					(gen-pandigital-from-perm permutation a-len b-len)))]
		(if (empty? coll) nil (first coll))))

(defn gen-pandigital []
	(let [perm (clojure.math.combinatorics/permutations [1 2 3 4 5 6 7 8 9])]
		(filter (complement empty?) (for [p perm] (gen-pandigital-split-from-perm p)))))

(defn euler-32 []
	"Solves Problem 32, slowly. Solution 45228"
	(reduce + (set (map #(last %) (gen-pandigital)))))


(defn gen-2x2-ratios []
	(for [n (range 10 100), d (range 10 100) :when (< n d)] {:numer n, :denom d}))

(defn tens [n] (quot (rem n 100) 10))

(defn ones [n] (rem n 10))

(defn curious? [ratio]
	(let [n (:numer ratio), d (:denom ratio)]
		(or
			(and
				(= (ones n) (tens d))
				(not (zero? (ones d)))
				(= (/ (tens n) (ones d)) (/ n d)))
			(and
				(= (tens n) (ones d))
				(not (zero? (tens d)))
				(= (/ (ones n) (tens d)) ratio)))))

(defn curious-fractions []
	(filter curious? (gen-2x2-ratios)))

(defn euler-33 []
	"Solves Problem 33. Solution 100. (16/64, 19/95, 26/65, 49/98)"
	(denominator (reduce * (map #(/ (:numer %) (:denom %)) (curious-fractions)))))


(defn number-to-digits [num]
	(map #(Character/digit % 10) (str num)))

(defn num-to-digits [num]
	"Returns a list of digits in the number. Most significant first."
	(loop [q num, dig (list)]
		(if (zero? q) dig (recur (quot q 10) (conj dig (rem q 10))))))

(defn fact-lazy []
	(map #(first %) (iterate (fn [[cumu n]] [(*' (inc n) cumu) (inc n)]) [1 0])))

(def digit-factorials (vec (take 10 (fact-lazy))))

(defn curious-fact? [n]
	(= n (reduce + (map #(get digit-factorials %) (number-to-digits n)))))

(defn euler-34 []
	"Solves Problem 34, slowly (6 mins). Solution 40730"
	;Must have less than eight digits, as 9999999 -> 2540160
	(reduce + (filter curious-fact? (range 10 3000000)))) ;3000000


(defn digits-to-num [digits]
	"Returns a number from a list of digits. Most significant first."
	(loop [cumu (long 0), dig digits]
		(if (empty? dig) cumu (recur (+ (* 10 cumu) (int (first dig))) (rest dig)))))

(defn other-rotations [s]
	"Retuns the rotations of s (except s itself). Fast! Copied from somewhere else."
	(map #(concat (second %) (first %)) (map #(split-at % s) (range 1 (count s)))))

(defn rotations [coll]
	"Returns a list of all rotations of the items in given collection."
	(let [cycle2 (into (vec coll) coll)]
		(for [start (range (count coll))]
			(vec (replace cycle2 (range start (+ start (count coll))))))))

(defn circular-prime? [primes-set n]
	"Is a number a circular prime, that is, all rotations are primes?"
	(let [rots (rotations (num-to-digits n))]
		(every? #(contains? primes-set (digits-to-num %)) rots)))

(defn circular-primes [n]
	"Return a collection of all circular primes up to n."
	(let [
		max-n (reduce max (map #(digits-to-num %) (rotations (num-to-digits n))))
		primes (set (sieve max-n))
		]
		(filter (partial circular-prime? primes) primes)))

(defn euler-35 []
	"Solves Problem 35. Solution 55"
	(count (circular-primes 999999)))


(defn palindromes-in-bases-2-10 [up-to-n]
	(filter
		#(and (palindrome? %) (palindrome? (Integer/toString % 2)))
		(range 1 (inc up-to-n))))

(defn euler-36 []
	"Solves Problem 36. Solution 872187"
	(reduce + (palindromes-in-bases-2-10 999999)))


(defn right-truncated-numbers [num]
	(loop [q (quot num 10), coll (list)]
		(if (zero? q)
			coll
			(recur (quot q 10) (conj coll q)))))

(defn left-truncated-numbers [num]
	(loop [q 0, coll (list), div 10]
		(if (= q num)
			(rest coll)
			(recur (rem num div) (conj coll (rem num div)) (* div 10)))))

(defn truncatable-prime? [primes-set p]
	(and
		(> p 10)  ;one-digit primes are not truncatable
		(every?
			#(contains? primes-set %)
			(flatten (list (left-truncated-numbers p) p (right-truncated-numbers p))))))

(defn truncatable-primes [primes-set]
	(filter (fn [p] (truncatable-prime? primes-set p)) primes-set))

(defn euler-37 []
	"Solves Problem 37. Solution 748317"
	(let [max-n 1000000, primes-set (set (sieve max-n))]
		(reduce + (truncatable-primes primes-set))))


(defn pandigital? [num]
	(let [dig (number-to-digits num), dig-set (set dig)]
		(and
			(= (count dig) 9)
			(= dig-set #{1 2 3 4 5 6 7 8 9}))))

(defn concat-product [n mult-n]
	(let [num-string (apply str (map #(* n %) (range 1 (inc mult-n))))]
		(if (> (count num-string) 9)
			0
			(Integer/parseInt num-string))))

(defn euler-38 []
	"Solves Problem 38. Solution 932718654"
	(reduce
		max
		(for [num (range 1 10000), mult-n (range 2 10)
			:let [prod (concat-product num mult-n)]
			:when (pandigital? prod)]
			prod)))


(defn euler-39 []
	"Solves Problem 39. Solution 840"
	(inc (second (max-index (for [p (range 1 (inc 1000))]
		(count
			(for [
			a (range 1 (quot p 3))
			b (range a (+ a (quot (- p a) 2)))
			:let [c (- p a b)]
			:when (= (* c c) (+ (* a a) (* b b)))]
			[a b c])))))))


(defn irrational-concat
	([] (irrational-concat 0))
	([num] (concat (str num) (lazy-seq (irrational-concat (inc num))))))

(defn euler-40 []
	"Solves Problem 40. Solution 210"
	(let [indices #{1 10 100 1000 10000 100000 1000000} ]
		(->> (irrational-concat)
			(keep-indexed #(if (contains? indices %) (Character/digit %2 10)))
			(take (count indices))
			(reduce *))))
; Or alternatively:
;		(reduce
;			*
;			(take
;				(count indices)
;				(keep-indexed
;					#(if (contains? indices %) (Character/digit %2 10))
;					(irrational-concat))))))


(defn is-prime? [n]
	"Returns true if n is a prime number. (recomputes primes on each call)"
	(cond
		(contains? #{2 3 5} n) true
		(some #(if (divisible-by? n %) %) [2 3 5]) false
		(< n 7) false
		:else
		(let [max-div (long (Math/sqrt n)), incr [0 2 0 4 0 0 0 2 0 2]]
			(loop [i 7]
				(cond
					(> i max-div) true
					(divisible-by? n i) false
					:else
					(recur (+ i (get incr (rem i 10)))))))))

(defn pandigital-primes [n]
	"Returns all n-digit pandigital numbers that are also primes."
	(let [
		proper-rem? #(contains? #{1 3 7 9} (last %))
		perm clojure.math.combinatorics/permutations
		candidates (filter proper-rem? (perm (range 1 (inc n))))]
		(filter #(is-prime? %) (map #(digits-to-num %) candidates))))

(defn euler-41 []
	"Solves Problem 41. Solution 7652413"
	; Note: n-digit pandigitals cannot be primes if n in {2,3,5,6,8,9}
	; because they all are divisible by 3 (sum of digits divisible by 3).
	; That leaves us only 4 and 7.
	(reduce max (pandigital-primes 7)))


(defn euler-42 []
	"Solves Problem 42. Solution 162"
		(let [triangles (set (map sum-up-to (range 1 10000)))]
		(count (filter (fn [n] (contains? triangles n))
				(map
					(fn [^String s] (reduce + (map #(- (inc (int %)) (int \A)) (seq (subs s 1 (dec (count s)))))))
					(.split (slurp "words.txt") ","))))))


(defn euler-43 []
	"Solves Problem 43, slowly (30 sec). Solution 16695334890"
	(let [
		perm clojure.math.combinatorics/permutations
		starts (vec(range 1 8))
		ends (vec(range 4 11))
		divs [2 3 5 7 11 13 17]]
		(reduce + (map #(digits-to-num %)
			(for [p (perm [0 1 2 3 4 5 6 7 8 9])
				:when (every?
					#(divisible-by? (digits-to-num (subvec p (starts %) (ends %))) (divs %))
					(range 7))]
				p)))))


(defn nth-pentagonal [n]
	"Return the nth pentagonal number."
	(/ (* n (dec (* 3 n))) 2))

(defn pentagonal
	"Generate a lazy sequence of pentagonal numbers."
	([] (pentagonal 1))
	([n] (cons (nth-pentagonal n) (lazy-seq (pentagonal (inc n))))))

(defn relevant-pentagonals [diff]
	(let [
		up-to-n (int (inc (/ (dec diff) 3)))  ;P(n+1) - P(n) > diff
		summable-n (int (inc (* (Math/sqrt 2) up-to-n)))]  ;2*P(n) <= P(summable)
		(vec (take summable-n (pentagonal)))))

(defn find-specific-pentagonals [diff]
	(let [
		pen (relevant-pentagonals diff)
		pen-set (set pen)]
		(loop [i 0, j 1]
			(cond
				(> (- (pen j) (pen i)) diff)
					(if (and (= i (- (count pen) 2)) (= j (dec (count pen))))
						nil
						(recur (inc i) (+ i 2)))
				(and
					(contains? pen-set (+ (pen j) (pen i)))
					(contains? pen-set (- (pen j) (pen i)))
				) (list (pen i) (pen j))
				:else
				(recur
					(if (= j (dec (count pen))) (inc i) i)
					(if (= j (dec (count pen))) (+ i 2) (inc j)))))))

(defn euler-44 []
	"Solves Problem 44, with ugly code. Solution 5482660. (7042750 - 1560090)."
	(let [pair (find-specific-pentagonals 5500000)]
		(- (second pair) (first pair))))

; Another (faster, nicer) implementation for Euler-44:
(defn pentagonal? [n] (let [k (/ (+ 1 (Math/sqrt (+ 1 (* 24 n)))) 6)] (zero? (- k (int k)))))
(defn sdpent? [p1 p2] (and (pentagonal? (- p2 p1)) (pentagonal? (+ p2 p1))))
(defn pent [n] (/ (* n (dec (* 3 n))) 2))
(defn sum-diff-pent-pent []
	"Return the first two pentagonal numbers whose sum and difference are pentagonal.
	Sketch by wwinogrod, completed by Esa Junttila."
	(loop [pents [1] n 2]
		(let [p (pent n), result (filter #(sdpent? % p) pents)]
			(cond
				(not (empty? result)) (vector (first result) p (+ p (first result)) (- p (first result)))
				:else (recur (conj pents p) (inc n))))))


(defn nth-triangle [n] (/ (* n (inc n)) 2))
(defn triangle
	"A lazy sequence of triangle numbers."
	([] (triangle 1))
	([n] (cons (nth-triangle n) (lazy-seq (triangle (inc n))))))

;(defn triangle2 []
;	"A lazy sequence of triangle numbers."
;	(reductions + (iterate inc 1)))

(defn nth-hexagonal [n] (* n (dec (* 2 n))))
(defn hexagonal
	"A lazy sequence of hexagonal numbers."
	([] (hexagonal 1))
	([n] (cons (nth-hexagonal n) (lazy-seq (hexagonal (inc n))))))

(defn find-tri-hex-pen [start]
	(let [skip (fn [s] (drop-while #(< % start) s))]
		(loop [tri (skip (triangle)), hex (skip (hexagonal)), pen (skip (pentagonal))]
			(let [t (first tri), h (first hex), p (first pen)]
				(cond
					(= t h p) t
					(and (<= t h) (<= t p)) (recur (rest tri) hex pen)
					(<= h p)                (recur tri (rest hex) pen)
					:else                   (recur tri hex (rest pen)))))))

(defn euler-45 []
	"Solves Problem 45. Solution 1533776805"
	(find-tri-hex-pen 40756))


(defn goldbach? [n]
	(loop [i 1]
		(let [doublesquare (* i i 2)]
			(cond
				(>= doublesquare n) false
				(is-prime? (- n doublesquare)) true
				:else (recur (inc i))))))

(defn euler-46 []
	"Solves Problem 46. Solution 5777"
	(some #(if ((complement goldbach?) %) %) (filter (complement is-prime?) (iterate #(+ 2 %) 9))))


(comment ;; TOO SLOW
(defn next-prime [p]
	(let [skip [1 2 1 4 3 2 1 2 1 2]]
		(if (<= p 5)
			(cond (< p 0) 2 :else ([2 2 3 5 5 7] p))
			(loop [i (+ p (skip (rem p 10)))]
				(if (is-prime? i)
					i
					(recur (+ i (skip (rem i 10)))))))))
(defn primes [] (iterate next-prime 2)) ;SLOW
(defn prime-factors [n]
	(loop [q n, pri (primes), factors (list)]
		(let [p (first pri)]
			(cond
				(= q 1) factors
				(divisible-by? q p)
					(recur (quot q p), pri, (conj factors p))
				:else
					(recur q, (rest pri), factors)))))
(defn distinct-primes? [pri-freq pri-freq2]
	(empty? (for [a pri-freq, b pri-freq2 :when (= a b)] a)))
(require 'clojure.math.combinatorics)
(defn euler-47-slow []  ;TOO SLOW!!
	"Solves Problem 47. Solution ?"
	(let [n 4, start 2, freqs (reverse (map #(frequencies (prime-factors %)) (range start (+ start n))))]
		(loop [s start, f freqs]
			(if
				(and
					(every? #(= n (count %)) f)
					(every? #(distinct-primes? (first %) (second %)) (clojure.math.combinatorics/combinations f 2)))
				s
				(recur (inc s), (conj (take (dec n) f) (frequencies (prime-factors (+ s n)))))))))
) ;end comment


(defn prime-factors [n primes prev-fact]
	(loop [q n, pri primes, n-fact (list)]
		(let [p (first pri)]
			(cond
				(= q 1) n-fact
				(< q (count prev-fact)) (into (prev-fact q) n-fact)
				(divisible-by? q p) (recur (quot q p), (rest pri), (conj n-fact p))
				:else (recur q, (rest pri), n-fact)))))

(defn all-prime-factors [up-to-n]
	(let [primes (sieve (inc up-to-n))]
		(loop [i 2, factors [nil nil]]
			(if	(> i up-to-n)
				factors
				(recur
					(inc i)
					(conj factors (prime-factors i primes factors)))))))

(defn distinct-prime-factors? [freq-fact freq-fact2]
	(empty? (for [a freq-fact, b freq-fact2 :when (= a b)] a)))

(require 'clojure.math.combinatorics)
(defn all-distinct-prime-factors? [all-freq-factors]
	(every?
		#(distinct-prime-factors? (first %) (second %))
		(clojure.math.combinatorics/combinations all-freq-factors 2)))

(defn find-consecutive-prime-distinct [length maxnum]
	(let [
		fact (all-prime-factors maxnum)
		fact-freqs (reverse (map frequencies (take (+ length 2) fact)))
		]
		(loop [start 2, freq fact-freqs]
			(let [f (take length freq)]
				(if (and
						(every? #(= length (count %)) f)
						(all-distinct-prime-factors? f)
					)
					start
					(recur
						(inc start)
						(conj freq (frequencies (fact (+ start length))))))))))

(defn euler-47 []
	"Solves Problem 47 in 40 seconds. Solution 134043."
	(find-consecutive-prime-distinct 4 140000))


(defn power-digits [base expo n-digits]
	(let [divisor (reduce * (repeat n-digits 10))]
		(loop [n expo, curr 1]
			(if (zero? n)
				curr
				(recur (dec n) (rem (* curr base) divisor))))))

(defn euler-48 []
	"Solves Problem 48. Solution 9110846700"
	(rem (reduce + (map #(power-digits % % 10) (range 1 (inc 1000)))) 10000000000))


(require 'clojure.math.combinatorics)
(defn arithmetic-seq [a primes-set]
	(let [
		valid-fn? (fn [n] (and (> n a) (contains? primes-set n)))
		cand-set (set (filter valid-fn? (map list-to-num (clojure.math.combinatorics/permutations (digits a)))))
		arith
			(for [b cand-set
			:let [c (+ b (- b a))]
			:when (contains? cand-set c)]
			[a b c])]
		(if (empty? arith) nil arith)))

(defn find-arithmetic-seq [start]
	(let [
		primes (drop-while #(< % start) (sieve 10000))
		primes-set (set primes)]
		(loop [pri primes]
			(let [p (first pri), arith (arithmetic-seq p primes-set)]
				(if arith (first arith) (recur (rest pri)))))))

(defn euler-49 []
	"Solves Problem 49. Solution 296962999629"
	(Long/parseLong (apply str (map str (find-arithmetic-seq 1488)))))


(defn cumulative-sum [coll]
	"Returns a vector of cumulative sums."
	(vec (reductions + (conj coll 0))))

(defn euler-50 []
	"Solves Problem 50. Solution 997651"
	(let [
		n 1000000
		max-length 546  ;sum of 547 first primes > 1000000
		primes (sieve n),
		pset (set primes),
		cumu (cumulative-sum primes)]
		(loop [length max-length]
			(let [matches (for [start (range (- (count cumu) length 1))
						:let [end (+ start length), sum (- (cumu end) (cumu start))]
						:when (and (< sum n) (contains? pset sum))]
						sum)]
				(if (empty? matches) (recur (dec length)) matches)))))


(defn all-replaced-numbers [p comb]
	"Return all combinations of modified numbers, given the replaced indices."
	(reduce
		(fn [coll new-digit]
			(conj coll (list-to-num (map-indexed
				(fn [idx old-digit]
					(if (some #(if (= idx %) true) comb) new-digit old-digit))
				(digits p)))))
		(list)
		(range 10)))

(defn family-match [p pset comb size]
	"Returns the smallest prime in a size-matching family, or nil."
	(let [result (filter #(and (>= % p) (contains? pset %)) (all-replaced-numbers p comb))]
		(if (= (count result) size) (reduce min result))))

(require 'clojure.math.combinatorics)
(defn family-combinations [p pset size]
	"Tries all combinations to achieve a matching family."
	(let [
		len (count (digits p))
		com (partial clojure.math.combinatorics/combinations (range len))]
		(some
			(fn [c] (family-match p pset c size))
			(reduce #(into %1 (com %2)) (list) (range (inc len))))))

(defn euler-51 []
	"Solves Problem 51. Solution 121313"
	(let [primes (sieve 1000000), pset (set primes), size 8]
	(some (fn [p] (family-combinations p pset size)) primes)))


(defn multipliable-digits? [num max-multi]
	(every?
		(fn [multi-num] (= (set (digits num)) (set (digits multi-num))))
		(map (partial * num) (range 2 (inc max-multi)))))

(require 'clojure.math.combinatorics)
(defn find-multipliable-number [len max-multi]
	(let [
		avail-digits (list 0 2 3 4 5 6 7 8 9)
		gen-comb clojure.math.combinatorics/combinations
		gen-perm clojure.math.combinatorics/permutations]
		(some
			(fn [cand] (if (multipliable-digits? cand max-multi) cand))
			(sort (map
				(fn [per] (list-to-num (cons 1 (seq per))))
				(reduce
					(fn [coll comb] (into coll (gen-perm comb)))
					(list)
					(gen-comb avail-digits (dec len))))))))

(defn euler-52 []
	"Solves Problem 52. Solution 142857"
	; Note: first two digits must be 10, 11, 12, 13, 14, 15, or 16.
	; Otherwise multiplying by 6 brings in another digit.
	(let [max-num-length 6, max-multi 6]
		(some
			(fn [len] (find-multipliable-number len max-multi))
			(iterate inc 2))))


(def fact-memo (memoize fact))
(defn nCr53 [n k]
	(/ (fact-memo n) (*' (fact-memo k) (fact-memo (- n k)))))
(defn count-exceeding [max-n threshold]
	(count
		(for [n (range (inc max-n)), k (range (inc n))
		:let [n (nCr53 n k)]
		:when (> n threshold)]
		n)))
(defn euler-53 []
	"Solves Problem 53. Solution 4075"
	(count-exceeding 100 1000000))


(defrecord PokerCard [value suit])
(defrecord PokerHand [cards])
(defrecord PokerHandInfo [values freqs multiples is-flush])
(defrecord PokerResult [rank primary secondary values])
(def ^:const poker-rank
	{:high 0, :pair 1, :pairs 2, :threes 3, :straight 4, :flush 5,
	 :house 6, :fours 7, :straightflush 8})

(defn parse-card-value [valchar]
	(let [high-values {\T 10, \J 11, \Q 12, \K 13, \A 14}]
		(cond
			(Character/isDigit valchar) (Character/digit valchar 10)
			(contains? high-values valchar) (high-values valchar))))

(defn create-card [cardstring]
	(PokerCard. (parse-card-value (first cardstring)) (second cardstring)))

(defn create-hand [card-seq] (PokerHand. (vec card-seq)))

(defn parse-poker-hands [hand-string]
	(let [cards (map create-card (clojure.string/split hand-string #"\s"))]
		[(create-hand (take 5 cards)) (create-hand (drop 5 cards))]))

(defn poker-high-card [info] (PokerResult. (:high poker-rank), 0, 0, (:values info)))
(defn poker-multiples [info n rank]
	"Finds a single pair, threes, or fours, assuming there are no other multiples."
	(if-let [multi-val (first (get (:multiples info) n))]
		(PokerResult. rank, multi-val, 0, (:values info))))
(defn poker-pair [info] (poker-multiples info 2 (:pair poker-rank)))
(defn poker-threes [info] (poker-multiples info 3 (:threes poker-rank)))
(defn poker-fours [info] (poker-multiples info 4 (:fours poker-rank)))
(defn poker-flush [info] (if (:is-flush info) (PokerResult. (:flush poker-rank), 0, 0, (:values info))))
(defn poker-two-pairs [info]
	(let [pair-vals (get (:multiples info) 2)]
		(if (= 2 (count pair-vals))
			(PokerResult. (:pairs poker-rank), (first pair-vals), (second pair-vals), (:values info)))))
(defn poker-full-house [info]
	(let [[pair-val threes-val] (map #(first (get (:multiples info) %)) [2 3])]
		(if (and pair-val threes-val)
			(PokerResult. (:house poker-rank), threes-val, 0, (:values info)))))
(defn poker-straight [info]
	(let [vals (:values info), min (last vals)]
		(if (= vals (reverse (take 5 (iterate inc min))))
			(PokerResult. (:straight poker-rank), (first vals), 0, vals))))
(defn poker-straight-flush [info]
	(if-let [straight (poker-straight info)]
		(if (:is-flush info) (assoc straight :rank (:straightflush poker-rank)))))


(defn map-invert-with-duplicates [mapping]
	"Inverts a map. Values are vectors of (original) keys. Code by amalloy."
	(apply merge-with into (for [[key val] mapping] {val [key]})))

(defn extract-hand-info [hand]
	(let [
		values (vec (sort > (map :value (:cards hand))))  ; in non-increasing order!
		freqs (frequencies values)
		multiples (map-invert-with-duplicates freqs)
		is-flush (apply = (map :suit (:cards hand)))]
		(PokerHandInfo. values freqs multiples is-flush)))

(defn determine-hand [hand]
	(let [info (extract-hand-info hand)]
	(or  ;returns the first non-nil value
		(poker-straight-flush info)  ;start from the highest hand
		(poker-fours info)
		(poker-full-house info)
		(poker-flush info)
		(poker-straight info)
		(poker-threes info)
		(poker-two-pairs info)
		(poker-pair info)
		(poker-high-card info)))) ;always returns a value

(defn compare-card-values [my-result other-result]
	(let [cmp
			(some (fn [[myval otherval]]
				(cond
					(> myval otherval) :my-win,
					(< myval otherval) :other-win,
					:else nil))
			(map vector (:values my-result) (:values other-result)))]
		(cond (= cmp :my-win) true, (= cmp :other-win) false, :else nil)))

(defn compare-higher [my-result other-result]
	(cond
		(> (:primary my-result) (:primary other-result)) true
		(< (:primary my-result) (:primary other-result)) false
		(> (:secondary my-result) (:secondary other-result)) true
		(> (:secondary my-result) (:secondary other-result)) false
		:else
		(compare-card-values my-result other-result)))

(defn rank-set [& more] (set (map poker-rank more)))

(defn wins-poker? [my-hand other-hand]
	(let [
		my-result (determine-hand my-hand)
		my-rank (:rank my-result)
		other-result (determine-hand other-hand)
		other-rank (:rank other-result)]
		(cond
			(> my-rank other-rank) true
			(< my-rank other-rank) false
			:else  ;ranks are identical, let the highest card decide
			(cond
				(contains? (rank-set :high :flush) my-rank) (compare-card-values my-result other-result)
				(contains? (rank-set :straight :straightflush) my-rank) (compare-higher my-result other-result)
				(contains? (rank-set :pair :threes :fours) my-rank) (compare-higher my-result other-result)
				(contains? (rank-set :pairs :house) my-rank) (compare-higher my-result other-result)
				:else (throw (Exception. "unknown hand rank"))))))

(defn euler-54 []
	"Solves Problem 54. Solution 376"
	(with-open [rdr (clojure.java.io/reader "poker.txt")]
		(count (filter
			(fn [[me other]] (wins-poker? me other))
			(map parse-poker-hands (line-seq rdr))))))


(defn reverse-number [n]
	"Returns a number with its digits reversed: 123 -> 321."
	(loop [q n, rev 0]
		(if (zero? q)
			rev
			(recur (quot q 10), (+ (rem q 10) (* 10 rev))))))

(defn lychrel? [n]  ; the first unmodified number never counts as a palindrome
	(not-any? #(palindrome? (str %)) (rest (take 50 (iterate #(+' % (reverse-number %)) n)))))

(defn euler-55 []
	"Solves Problem 55. Solution 249"
	(count (filter lychrel? (range 10000))))


(defn sum-digits [n] (reduce + (digits n)))
(defn big-power [base expo] (reduce *' (repeat expo base)))
(defn euler-56 []
	"Solves Problem 56. Solution 972"
	(reduce max (for [a (range 100), b (range 100)] (sum-digits (big-power a b)))))


(defn continued-sqrt2-fraction []
	(map inc (iterate #(/ (+ 2 %)) (/ 2))))

(defn euler-57 []
	"Solves Problem 57. Solution 153"
	(count (filter
		#(> (-> % numerator str count) (-> % denominator str count))
		(take 1001 (continued-sqrt2-fraction)))))


;(defn spiral-diagonal-values [] (map first (iterate next-spiral-diagonal-value [1 1])))
;
;(defn euler-58 [] ;; CONSUMES TOO MUCH MEMORY
;	"Solves Problem 58. Solution ?. Uses a function from euler-28."
;	(let [primes-set (set (sieve 100000000))]
;		(loop [n-total 1, n-primes 0, diags (rest (spiral-diagonal-values)), side-len 1]
;			(do (println n-total (first diags) (double (/ n-primes n-total)))
;			(if (or (< 99990000 (first diags)) (and (> side-len 7) (< (/ n-primes n-total) 0.1)))
;				side-len
;				(recur
;					(+ n-total 4)
;					(+ n-primes (count (filter #(contains? primes-set %) (take 4 diags))))
;					(drop 4 diags)
;					(+ 2 side-len)))))))
;; CONSUMES TOO MUCH MEMORY


(def primes-seq
	"Lazy sequence of all the prime numbers.
	 Converted from contrib.lazy-seq.
	 (implicitly stores computed values)"
	(concat
		[2 3 5 7]
		(lazy-seq
			(let [
				primes-from (fn primes-from [n [f & r]]
					(if (some #(zero? (rem n %)) (take-while #(<= (* % %) n) primes-seq))
						(recur (+ n f) r)
						(lazy-seq (cons n (primes-from (+ n f) r)))))
				wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6 4 2
				              6 4 6 8 4 2 4 2 4 8 6 4 6 2 4 6
				              2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
				(primes-from 11 wheel)))))

(defn prime-number? [n]
	"Primality test based on the lazy sequence of prime numbers."
	(every?
		(complement (partial divisible-by? n))
		(take-while #(<= (* % %) n) primes-seq)))

(defn spiral-diagonal-values []
	"A lazy sequence of numbers on the diagonals of the spiral grid."
	(map first (iterate next-spiral-diagonal-value [1 1])))

(defn euler-58 []
	"Solves Problem 58. Solution 26241. Uses a function from euler-28."
	(loop [spirals (rest (spiral-diagonal-values)), n-primes 0, n-total 1]
		(if (and (> n-total 49) (< (/ n-primes n-total) 0.1))
			(inc (* 2 (quot n-total 4)))
			(recur
				(drop 4 spirals)
				(+ n-primes (count (filter prime-number? (take 4 spirals))))
				(+ n-total 4)))))


(defn decrypt [ascii-values]
	(let [char-vals (range (int \a) (inc (int \z)))]  ; CORRECT \a --> \z
		(for [a char-vals, b char-vals, c char-vals
			:let [
				keys (cycle (list a b c))
				encrypted (map #(bit-xor %1 %2) ascii-values keys)]]
			(apply str (map char encrypted)))))

(defn euler-59 []
	"Solves Problem 59. Solution 107359"
	(with-open [
		rdr (clojure.java.io/reader "cipher1.txt")
		wrtr (clojure.java.io/writer "decrypted.txt")]
		(reduce + (map
			int
			(first (filter
				#(= (seq "(Th") (take 3 %))
				(decrypt (map
					#(Integer/parseInt %)
					(-> rdr line-seq first (.split ","))))))))))


(use 'clojure.set)
(defn gen-single-primes [up-to-n]
	(vec (map #(set (list %)) (take-while #(<= % up-to-n) primes-seq))))
(defn concat-candidates [concat-primes]
	(let [n (count concat-primes)]
		(for [i (range (dec n)), j (range (inc i) n)
			:let [
				a (get concat-primes i)
				b (get concat-primes j)
				c (intersection a b)
				]
			:when (= (count c) (dec (count a)))
			]
			[a b])))
(defn merge-concat [concat-primes max-sum]
	(vec (distinct (map
		#(union (first %) (second %))
		(filter
			(fn [[a b]]
				(let [
					cand-a (first (difference a b))
					cand-b (first (difference b a))
					concat-a (str cand-a cand-b)
					concat-b (str cand-b cand-a)]
					(and
						(<= (+ (reduce + a) cand-b) max-sum)
						(prime-number? (Integer/parseInt concat-a))
						(prime-number? (Integer/parseInt concat-b)))))
			(concat-candidates concat-primes))))))
(defn gen-concat-primes [n-primes max-prime max-sum]
	(if (= 1 n-primes)
		(gen-single-primes max-prime)
		(let [conpri (gen-concat-primes (dec n-primes) max-prime max-sum)]
			(merge-concat conpri max-sum))))

(defn euler-60 []
	"Solves Problem 60, very slowly (18 min). Solution 26033. (13, 5197, 5701, 6733, 8389)"
	(gen-concat-primes 5 10000 27000)
)


;(defn polygonal-numbers-old
;	"Returns a lazy sequence of polygonal numbers."
;	([n-sides]
;		(let [func (cond
;			(= n-sides 3) (fn [n] (/ (* n (inc n)) 2))  ;triangle
;			(= n-sides 4) (fn [n] (* n n))  ;square
;			(= n-sides 5) (fn [n] (/ (* n (- (* 3 n) 1)) 2))  ;pentagonal
;			(= n-sides 6) (fn [n] (* n (- (* 2 n) 1)))  ;hexagonal
;			(= n-sides 7) (fn [n] (/ (* n (- (* 5 n) 3)) 2))  ;heptagonal
;			(= n-sides 8) (fn [n] (* n (- (* 3 n) 2)))  ;octagonal
;			:else (throw (Exception. "unexpected number of sides")))]
;			(polygonal-numbers func 1)))
;	([func n] (cons (func n) (lazy-seq (polygonal-numbers func (inc n))))))

(defn polygonal-numbers
	"Returns a lazy sequence of polygonal numbers."
	([n-sides] (polygonal-numbers n-sides 1))
	([n-sides n]
		(let [f (fn [^long k, ^long n] (/ (- (* (- k 2) n n) (* (- k 4) n)) 2))]
			(cons (f n-sides n) (lazy-seq (polygonal-numbers n-sides (inc n)))))))
(defn last-2-numbers [num] (rem num 100))
(defn first-2-numbers [num] (loop [n num] (if (zero? (quot n 100)) n (recur (quot n 10)))))

(def poly-triangle (polygonal-numbers 3))
(def poly-square (polygonal-numbers 4))
(def poly-penta (polygonal-numbers 5))
(def poly-hexa (polygonal-numbers 6))
(def poly-hepta (polygonal-numbers 7))
(def poly-octa (polygonal-numbers 8))
(def polys [poly-triangle poly-square poly-penta poly-hexa poly-hepta poly-octa])

(defn restrict-consecutive [sequ prev]
	(let [start (* 100 (last-2-numbers prev)),
		end (+ start 99)]
		(take-while #(<= % end) (drop-while #(< % start) sequ))))

(defn find-cyclic-set [seq-idx-vec]
	(for [
		poly-1 (take-while #(< % 10000) (polygonal-numbers 8))
		poly-2 (restrict-consecutive (get polys (get seq-idx-vec 0)) poly-1)
		poly-3 (restrict-consecutive (get polys (get seq-idx-vec 1)) poly-2)
		poly-4 (restrict-consecutive (get polys (get seq-idx-vec 2)) poly-3)
		poly-5 (restrict-consecutive (get polys (get seq-idx-vec 3)) poly-4)
		poly-6 (restrict-consecutive (get polys (get seq-idx-vec 4)) poly-5)
		:when (and
			(every? #(>= % 1000) (list poly-1 poly-2 poly-3 poly-4 poly-5 poly-6))
			(= (last-2-numbers poly-6) (first-2-numbers poly-1)))]
		[poly-1 poly-2 poly-3 poly-4 poly-5 poly-6]))

(use 'clojure.math.combinatorics)
(defn euler-61 []
	"Solves Problem 61. Solution 28684. (1281 8128 2882 8256 5625 2512)"
	(reduce + (first (some
		#(if (not (empty? %)) %)
		(map #(find-cyclic-set (vec %)) (permutations (range 5)))))))



;; Not used in problems:
;;;;;;;;;;;;;;;;;;;;;;;;
(defn newton-raphson [f derf init-guess]  ;slow for unknown reason
	(let [max-iter 100]
		(loop [x (double init-guess), prev (double 0), n-iter (int 1)]
			(let [d (derf x)]
				(if (or (zero? x) (= x prev) (zero? d) (> n-iter max-iter))
					x
					(recur (- x (/ (f x) d)), x, (inc n-iter)))))))
(defn square-root-guess ^double [^double n]  ;Babylonian method, start with 1.
	(let [x (/ (+ 1 n) 2)] (* 0.5 (+ x (/ n x)))))
(defn square-root-general ^double [^double n]  ;slow
	(let [
		func (fn [^double x] (- (* x x) n)),
		derf (fn [^double x] (* 2 x)),
		guess (square-root-guess n)]
		(newton-raphson func derf guess)))
(defn cubic-root-guess ^double [^double n]  ;Babylonian method, start with 1.
	(let [x (double (/ (+ 2 n) 3))] (/ (+ x x (/ n (* x x))) 3)))
(defn cubic-root-general ^double [^double n]
	(let [
		func (fn [^double x] (- (* x x x) n)),
		derf (fn [^double x] (* 3 x x)),
		guess (cubic-root-guess n)]
		(newton-raphson func derf guess)))
(defn square-root ^double [^double n]  ;just an exercise: Math/sqrt should be used instead.
	(let [max-iter (int 100)]
		(loop [x (double (square-root-guess n)), prev (double 0), n-iter (int 1)]
			(if (or (= x prev) (> n-iter max-iter))
				x
				(recur (* 0.5 (+ x (/ n x))), x, (inc n-iter))))))
(defn cubic-root ^double [^double n]  ;optimized Halley's method (no division-by-zero checks)
	(let [max-iter (int 100)]
		(cond (zero? n) 0, (neg? n) (throw (IllegalArgumentException. "negative")) :else
		(loop [x (double (cubic-root-guess n)), prev (double 0), n-iter (int 1)]
			(let [x3 (* x x x), x3plus (+ x3 n)]
				(if (or (= x prev) (> n-iter max-iter))
					x
					(recur (* x (/ (+ x3plus n) (+ x3plus x3))), x, (inc n-iter))))))))
(defn is-cubic? [n]
	(let [m (long n)]
		(if (= n m)
			(let [k (long (Math/round (cubic-root m)))]
				(= (* k k k) m))
			(throw (IllegalArgumentException. "not integer")))))

;;;;;;;;;;;;;;;;



(use 'clojure.math.combinatorics)
(defn cubic-numbers
	([] (cubic-numbers 1))
	([^long n] (cons (* n n n) (lazy-seq (cubic-numbers (inc n))))))
(defn find-perm-cubics [cub n]
	(let [freq (frequencies (map #(sort (digits %)) cub))]
		(some
			(fn [[ke va]] (if (<= n va) (some #(if (= ke (sort (digits %))) %) cub)))
			freq)))
(defn drop-take [dropfn takefn coll] (take-while takefn (drop-while dropfn coll)))
(defn euler-62 []
	"Solves Problem 62. Solution 127035954683. (4 cubes: 1006012008)"
	(let [
		high 1000000000000,
		low (quot high 10),
		cub (drop-take #(< % low) #(< % high) (cubic-numbers))]
		(find-perm-cubics cub 5)))


(defn euler-63 []
	"Solves Problem 63. Solution 49"
	"Number k^n is subject to 10^(k-1) <= n^k < 10^k, which gives the following restrictions:"
	(count (distinct
		(for [
			k (range 1 10),
			n (range 1 (inc (Math/floor (- 1 (/ (Math/log k) (Math/log (/ k 10)))))))]
			(Math/pow k n)))))

(defn euler-64 []
	"Solves Problem 64. Solution ?"
	"Note: 1/(sqrt(n) - k) = (sqrt(n) + k)/((sqrt(n) - k)*(sqrt(n) + k)) = (sqrt(n) + k)/(n - k^2)"
)


















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; X, Y ~ Unif(0, 1)
; X and Y are independent from each other
; Z ~ min{X, Y}
; pdf_Z(x) = 2 - 2x
; cdf_Z(x) = 2x - x^2
; Random numbers via inversed cdf: 1-sqrt(1-R), where R ~ Unif(0, 1)
; Inverse of cumulative probability function
(defn gen-rand-pie-fast [] (- 1 (Math/sqrt (- 1 (Math/random)))))
(defn gen-rand-pie-simple [] (Math/min (Math/random) (Math/random)))

(defn eat-pie ^long [^double threshold]
	(loop [pie (double 1.0), n (int 0)]
		(if (< pie threshold)
			n
			(recur (* pie (gen-rand-pie-fast)) (inc n)))))

(defn sample-pie [^double threshold, ^long sample-size]
	(loop [cumu (double 0.0), s (long 1)]
		(if (> s sample-size)
			(double (/ cumu sample-size))
			(recur (+ cumu (eat-pie threshold)) (inc s)))))

(defn euler-394 []  ;accuracy is not enough!
	"Solves Problem 394. Solution ?"
	(time (sample-pie (/ 40) 10000000))
)
