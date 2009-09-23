/*
 * C# version by Will Johansson
 */
using System;
using System.Collections.Generic;

class Program
{
	public static void Main (string[] args)
	{
		Console.WriteLine ("Take<int>(5,Ones()) = {0}", Take<int>(5, Ones()).ToString());
		Console.WriteLine ("Take<int>(5,Ints()) = {0}", Take<int>(5, Ints()).ToString());
		Console.WriteLine ("Take<int>(5,Evens()) = {0}", Take<int>(5, Evens()).ToString());
		Console.WriteLine ("Take<int>(5,Odds()) = {0}", Take<int>(5, Odds()).ToString());
		Console.WriteLine ("Take<int>(5,Map (x => x+1, Ones())) = {0}", Take<int>(5, Map (x => x+1, Ones())).ToString());
		Console.WriteLine ("Take<int>(5,ZipWith ((x,y) =>  x + y, Ones(),Ones())) = {0}", Take<int>(5,ZipWith ((x,y) =>  x + y, Ones(),Ones())).ToString());
		Console.WriteLine ("Take<int>(5,Iterate(1, x => x+1)) = {0}", Take<int>(5,Iterate(1, x => x+1)).ToString());
		Console.WriteLine ("Take<int>(5,Squares() = {0}", Take<int>(5, Squares()).ToString());
		Console.WriteLine ("Take<int>(5,Powers(2) = {0}", Take<int>(5, Powers(2)).ToString());
		Console.WriteLine ("Take<int>(5,Square(2) = {0}", Take<int>(5, Square(2)).ToString());
		Console.WriteLine ("Take<int>(5,Seq(0,Ints()) = {0}",Take<int>(5,Seq(0,Ints())).ToString());
		Console.WriteLine ("Take<int>(5,Mapl((x,y) => x+y, Seq(0, Ints()))) = {0}", Take<int>(5,Mapl( (x,y) => x + y, Seq(0, Ints()))));
		Console.WriteLine ("Take<int>(5,Fibs(1,2)) = {0}", Take<int>(5,Fibs(1,2)).ToString());
		Console.WriteLine ("Take<int>(5,Total((x,y) => x+y, 0, Ints())) = {0}", Take<int>(5,Total((x,y) => x+y, 0, Ints())).ToString());
		Console.WriteLine ("Take<int>(5,Facts()) = {0}", Take<int>(5, Facts()).ToString());
	}
	
	/* Essentially a function pointer for a map to perform. */
	public delegate O Function<I,O> (I input);
	
	public delegate O Function<I,J,O> (I input1, J input2);
	
	public static Sequence<T> EmptySequence<T>()
	{
		return new Sequence<T> (() => null);
	}
	
	public static Sequence<int> Ones()
	{
		return Repeat<int>(1);
	}
	
	public static Sequence<int> Ints()
	{
		return Climb(1,1);
	}
	
	public static Sequence<int> Evens()
	{
		return Climb(2,2);
	}
	
	public static Sequence<int> Odds()
	{
		return Climb(1,2);
	}
	
	public static Sequence<T> Take<T> (int n, Sequence<T> sequence)
	{
		if (n <= 0)
			return EmptySequence<T>();
		
		Pair<T> somePair = sequence.Func();
		
		if (somePair == null)
			return EmptySequence<T>();
		
		return new Sequence<T> (() => new Pair<T> (somePair.car, Take<T> (n-1, somePair.cdr)));
	}
	
	public static Sequence<T> Repeat<T>(T n)
	{		
		return new Sequence<T> (() => new Pair<T> (n, Repeat(n)));
	}
	
	public static Sequence<int> Climb (int start, int step) {
		return new Sequence<int> (() => new Pair<int>(start, Climb(start + step, step)));
	}
	
	public static Sequence<O> Map<I,O> (Function<I,O> func, Sequence<I> sequence) {
		Pair<I> somePair = sequence.Func();
		
		if (somePair == null)
			return EmptySequence<O>();
			
		return new Sequence<O> (() => new Pair<O>( func (somePair.car), Map (func, somePair.cdr)));
	}
	
	public static Sequence<O> ZipWith<I,J,O> (Function<I,J,O> func, Sequence<I> a, Sequence<J> b)
	{
		Pair<I> aPair = a.Func();
		Pair<J> bPair = b.Func();
		
		return (aPair == null || bPair == null ? EmptySequence<O>() :
			new Sequence<O> (() => new Pair<O> (func (aPair.car, bPair.car), ZipWith (func, aPair.cdr, bPair.cdr))));
	}
	
	public static Sequence<T> Iterate<T> (T start, Function<T,T> func)
	{
		return new Sequence<T> (() => new Pair<T> (start, Iterate( func(start), func)));
	}
	
	public static Sequence<int> Powers (int n)
	{
		return Map (x => (int)Math.Pow (n,x), Ints());
	}
	
	public static Sequence<int> Squares ()
	{
		return Map (x => x*x, Ints());
	}
	
	public static Sequence<int> Square (int n)
	{
		return Iterate (n, x => (int)Math.Pow (x,2));
	}
	
	public static Sequence<T> Seq<T> (T t, Sequence<T> sequence)
	{
		return new Sequence<T> (() => new Pair<T> (t, sequence));
	}
	
	public static Sequence<T> Mapl<T> (Function<T,T,T> func, Sequence<T> sequence)
	{
		Pair<T> somePair = sequence.Func();
		
		if (somePair == null)
			return EmptySequence<T>();

		T first = somePair.car;
		
		Pair<T> cdr = somePair.cdr.Func();
		
		T second = cdr.car;
		
		if (second == null)
			return EmptySequence<T>();
		
		return new Sequence<T> (() => new Pair<T> (func (first,second), Mapl (func, somePair.cdr)));
	}
	
	public static Sequence<int> Fibs (int a, int b)
        {
            return new Sequence<int> (() => new Pair<int> (a, Fibs (b, (a+b))));
        }
        
        public static Sequence<T> Total<T>( Function<T,T,T> func, T t, Sequence<T> sequence )
        {
            Pair<T> somePair = sequence.Func();
            if ( somePair == null )
                return EmptySequence<T>();

            return new Sequence<T>( () => { T x = func (t, somePair.car); return new Pair<T> (x,Total (func, x, somePair.cdr)); } );
        }
        
	public static Sequence<int> Facts()
        {
            return Total ((l,r) => l * r, 1, Ints());
        }
}
