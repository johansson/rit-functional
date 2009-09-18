/**
 * Generic Single Argument Function
 * @author Joseph Pecoraro
 *
 * @param <I> Input Type
 * @param <O> Output Type
 */
public interface Function<I,O> {
	public O call(final I in);
}
