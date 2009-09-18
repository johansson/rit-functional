import java.util.List;

/**
 * Idea of a Predicate, is a Function that returns a boolean
 * @author Joseph Pecoraro
 */
public abstract class Predicate implements Function<Boolean> {
	public abstract Boolean call(List args);
}
