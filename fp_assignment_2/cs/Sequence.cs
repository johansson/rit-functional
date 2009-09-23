/*
 * C# version by Will Johansson
 */
 
using System.Collections.Generic;

public class Sequence<T>
{
	public delegate Pair<T> GeneratorFunction();

	public GeneratorFunction Func
	{
		get;
		private set;
	}
	
	public Sequence (GeneratorFunction f)
	{
		this.Func = f;
	}
	
	public override string ToString()
	{
		string val = "[";
		
		for (Pair<T> cur = Func(); cur != null; cur = cur.cdr.Func())
		{
			val += cur.car.ToString();
			
			if (cur.cdr.Func() != null)
				val += ",";
		}
		
		return val + "]";
	}
}
