/*
 * C# version by Will Johansson
 */
 
using System.Collections.Generic;

public class Pair<T>
{
	/* "Head", like car in Scheme */
	public T car;

	/* "Tail", or the rest of the sequence, aka cdr in Scheme */
	public Sequence<T> cdr;

	/* Pair ctor */
	public Pair(T car, Sequence<T> cdr)
	{
		this.car = car;
		this.cdr = cdr;
	}

}
