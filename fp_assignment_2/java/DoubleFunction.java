/**
 * Generic Double Argument Function
 * @author Joseph Pecoraro
 *
 * @param <A> First Input Type
 * @param <B> Second Input Type
 * @param <O> Output Type
 */
public interface DoubleFunction<A,B,O> {
	public O call(final A in1, final B in2);
}
