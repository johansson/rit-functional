import java.util.List;

/**
 * A Function that is the Negation of another Function
 * @author Joseph Pecoraro
 */
public class NotPredicate extends Predicate {

// Members

	/** The real function */
	private Predicate m_func;

	/** Hidden Constructor **/
	private NotPredicate(Predicate func) {
		m_func = func;
	}

// Static Factory Method

	/**
	 * How to Create a NotFunction
	 */
	public static Predicate not(Predicate func) {
		return new NotPredicate(func);
	}

// Public Methods

	/**
	 * Call this function, return the opposite
	 * of the wrapped function.
	 */
	public Boolean call(List record) {
		return !m_func.call(record);
	}

}
