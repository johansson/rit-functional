import java.util.List;

/**
 * A Generic Function
 * @author Joseph Pecoraro
 * @param <T> Return Value of the Function
 */
public interface Function<T> {
	public T call(List args);
}
