import java.util.ArrayList;
import java.util.List;

/**
 * Main Program
 * @author Joseph Pecoraro
 */
public class Driver {

// Single Argument Functions

	class AddOne implements Function<Integer, Integer> {
		public Integer call(Integer n) {
			return n + 1;
		}
	}

	class Square implements Function<Integer, Integer> {
		public Integer call(Integer n) {
			return n * n;
		}
	}


// Double Argument Functions

	class Add implements DoubleFunction<Integer, Integer, Integer> {
		public Integer call(Integer in1, Integer in2) {
			return in1 + in2;
		}
	}

	class Mult implements DoubleFunction<Integer, Integer, Integer> {
		public Integer call(Integer in1, Integer in2) {
			return in1 * in2;
		}
	}


// Static Methods

	/**
	 * take
	 */
	static<T> List<T> take(int n, final Sequence<T> lst) {
		List<T> elems = new ArrayList<T>();
		if (n == 0) return elems;
		Pair<T> pair = lst.f();
		elems.add(pair.head);
		elems.addAll( take(n-1, pair.tail) );
		return elems;
	}

	/**
	 * repeat 1 => [1,1..]
	 */
	static<T> Sequence<T> repeat(final T n) {
		return new Sequence<T>() {
			public Pair<T> f() {
				return new Pair<T>(n, repeat(n));
			}
		};
	}

	/**
	 * climb 1 2 => [1,3..]
	 */
	static Sequence<Integer> climb(final Integer start, final Integer step) {
		return new Sequence<Integer>() {
			public Pair<Integer> f() {
				return new Pair<Integer>(start, climb(start + step, step));
			}
		};
	}


	/**
	 * map (+1) ints
	 */
	static <A,B> Sequence<B> map(final Function<A, B> func, final Sequence<A> lst) {
		return new Sequence<B>() {
			public Pair<B> f() {
				Pair<A> pair = lst.f();
				return new Pair<B>( func.call(pair.head), map(func, pair.tail) );
			}
		};
	}

	/**
	 * zipWith (\a b -> a*b) ints ints
	 */
	static <A,B,C> Sequence<C> zipWith(final DoubleFunction<A, B, C> func, final Sequence<A> lst1, final Sequence<B> lst2) {
		return new Sequence<C>() {
			public Pair<C> f() {
				Pair<A> pair1 = lst1.f();
				Pair<B> pair2 = lst2.f();
				return new Pair<C>( func.call(pair1.head, pair2.head), zipWith(func, pair1.tail, pair2.tail) );
			}
		};
	}

	/**
	 * (total) scanl (\a b -> a*b) 1 ints
	 */
	static <A,B> Sequence<A> total(final DoubleFunction<A, B, A> func, final A startingValue, final Sequence<B> lst) {
		return new Sequence<A>() {
			public Pair<A> f() {
				Pair<B> pair = lst.f();
				A nextValue = func.call(startingValue, pair.head);
				return new Pair<A>( nextValue, total(func, nextValue, pair.tail) );
			}
		};
	}

	/**
	 * iterate (\a -> a+1) 1
	 */
	static <A> Sequence<A> iterate(final Function<A, A> func, final A startingValue) {
		return new Sequence<A>() {
			public Pair<A> f() {
				return new Pair<A>( startingValue, iterate(func, func.call(startingValue)) );
			}
		};
	}

	/**
	 * powers
	 */
	static Sequence<Integer> powers(final Integer n) {
		Function<Integer, Integer> lambda = new Function<Integer, Integer>() {
			public Integer call(Integer x) {
				return x * n;
			}
		};
		return iterate(lambda, n);
	}

	/**
	 * square
	 */
	static Sequence<Integer> square(final Integer n) {
		Function<Integer, Integer> lambda = new Function<Integer, Integer>() {
			public Integer call(Integer n) {
				return n * n;
			}
		};
		return iterate(lambda, n);
	}

	/**
	 * seq
	 */
	static <A> Sequence<A> seq(final A elem, final Sequence<A> lst) {
		return new Sequence<A>() {
			public Pair<A> f() {
				return new Pair<A>(elem, lst);
			}
		};
	}

	/**
	 * mapl
	 */
	static <A,B> Sequence<B> mapl(final DoubleFunction<A, A, B> func, final Sequence<A> lst) {
		return new Sequence<B>() {
			public Pair<B> f() {
				Pair<A> pair = lst.f();
				A first = pair.head;
				A second = pair.tail.f().head;
				return new Pair<B>( func.call(first, second), mapl(func, pair.tail) );
			}
		};
	}

	/**
	 * fibs
	 */
	static Sequence<Integer> fibs(final Integer a, final Integer b) {
		return fibsHelper(a,b);
	}
	private static Sequence<Integer> fibsHelper(final Integer x, final Integer y) {
		return new Sequence<Integer>() {
			public Pair<Integer> f() {
				return new Pair<Integer>(x, fibsHelper(y,x+y));
			}
		};
	}

	/**
	 * factorial
	 */
	static Sequence<Integer> factorial() {
		return factorialHelper(1,1);
	}
	private static Sequence<Integer> factorialHelper(final Integer mem, final Integer n) {
		return new Sequence<Integer>() {
			public Pair<Integer> f() {
				final Integer curr = mem*n;
				return new Pair<Integer>(curr, factorialHelper(curr,n+1));
			}
		};
	}


	/**
	 * Driver's Main
	 */
	public Driver() {

		// Common Functions
		Function<Integer, Integer> addOne = new AddOne();
		Function<Integer, Integer> sq = new Square();
		DoubleFunction<Integer, Integer, Integer> add = new Add();
		DoubleFunction<Integer, Integer, Integer> mult = new Mult();

		// Generic Sequences
		Sequence<Integer> ones    = repeat(1);
		Sequence<Integer> ints    = climb(1, 1);
		Sequence<Integer> evens   = climb(2, 2);
		Sequence<Integer> odds    = climb(1, 2);
		Sequence<Integer> squares = map(sq, ints);
		Sequence<Integer> facts   = factorial();
		Sequence<Integer> fibseq  = fibs(1,1);

		// Test Cases
		System.out.println( "Running Tests..." );
		System.out.println( "-------------------------------------------------------------------------");
		System.out.println( "RESULT  INPUT                                OUTPUT    : EXPECTED");
		System.out.println( "-------------------------------------------------------------------------");
		runTest( "take(5, ones)                    ", take(5, ones)                    , "[1, 1, 1, 1, 1]");
		runTest( "take(5, map(add1, ones))         ", take(5, map(addOne, ones))       , "[2, 2, 2, 2, 2]");
		runTest( "take(5, zipWith(add, ones, ones))", take(5, zipWith(add, ones, ones)), "[2, 2, 2, 2, 2]");
		runTest( "take(5, ints)                    ", take(5, ints)                    , "[1, 2, 3, 4, 5]");
		runTest( "take(5, iterate(add1, 1))        ", take(5, iterate(addOne, 1))      , "[1, 2, 3, 4, 5]");
		runTest( "take(5, evens)                   ", take(5, evens)                   , "[2, 4, 6, 8, 10]");
		runTest( "take(5, odds)                    ", take(5, odds)                    , "[1, 3, 5, 7, 9]");
		runTest( "take(5, squares)                 ", take(5, squares)                 , "[1, 4, 9, 16, 25]");
		runTest( "take(5, powers(2))               ", take(5, powers(2))               , "[2, 4, 8, 16, 32]");
		runTest( "take(5, square(2))               ", take(5, square(2))               , "[2, 4, 16, 256, 65536]");
		runTest( "take(5, seq(0, ints))            ", take(5, seq(0, ints))            , "[0, 1, 2, 3, 4]");
		runTest( "take(5, mapl(add, seq(0, ints))) ", take(5, mapl(add, seq(0, ints))) , "[1, 3, 5, 7, 9]");
		runTest( "take(5, fibs(1,2))               ", take(5, fibs(1,2))               , "[1, 2, 3, 5, 8]");
		runTest( "take(5, total(add, 0, ints))     ", take(5, total(add, 0, ints))     , "[1, 3, 6, 10, 15]");
		runTest( "take(5, facts)                   ", take(5, facts)                   , "[1, 2, 6, 24, 120]");
		runTest( "take(5, zipWith(mult, fibseq, squares))", take(5, zipWith(mult, fibseq, squares)), "[1, 4, 18, 48, 125]");
		System.out.println( "Tests Complete..." );

	}


// Testing

	/**
	 * Test Harness
	 */
	static void runTest(String str, List actual, String expected) {
		String actualString = actual.toString();
		String passString = (actualString.equals(expected)) ? "PASS" : "FAIL";
		System.out.println(passString + ":   " + str + " => " + actualString + " : " + expected);
	}


// Main

	/**
	 * main method
	 */
	public static void main(String[] args) {
		new Driver();
	}

}
