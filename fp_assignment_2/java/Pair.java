/**
 * Node in an Infinite List
 * @author Joseph Pecoraro
 *
 * @param <T> List's element type
 */
public class Pair<T> {

	/** Current Element */
	public final T head;

	/** Next Elements */
	public final Sequence<T> tail;

	/**
	 * Constructor
	 * @param head immediate element
	 * @param tail generator for the next elements
	 */
	public Pair(T head, Sequence<T> tail) {
		this.head = head;
		this.tail = tail;
	}

}