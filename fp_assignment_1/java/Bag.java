import java.util.ArrayList;
import java.util.List;

/**
 * A Bag Simply Stores a list of records (ArrayLists)
 * @author Joseph Pecoraro
 */
public class Bag {

// Members

	/** The general list */
	private List<List> m_list;

	/**
	 * Constructor
	 */
	public Bag() {
		m_list = new ArrayList<List>();
	}

	/**
	 * From a List
	 */
	public Bag(List l) {
		m_list = l;
	}

// Fundamental Interface

	/**
	 * Add an Element to the Bag
	 * @param elem Element to add
	 */
	public void add(List elem) {
		m_list.add(elem);
	}

	/**
	 * The size of the Bag
	 * @return the number of elements in the bag
	 */
	public int size() {
		return m_list.size();
	}

	/**
	 * Get the Actual List
	 * @return the list
	 */
	public List getList() {
		return m_list;
	}

// Public Methods

	/**
	 * Filter a Bag with a Predicate Function
	 * @param pred function returning a boolean when given a record
	 * @return a filtered Bag of references to the selected records
	 */
	public Bag where(Predicate pred) {
		Bag result = new Bag();
		for (List record : m_list) {
			if (pred.call(record)) {
				result.add(record);
			}
		}

		return result;
	}

	/**
	 * Apply a function to each element in the List
	 * @param func Function to apply to each record
	 * @return a list of results, list.size == bag.size
	 */
	public List map(Function func) {
		List result = new ArrayList();
		for (List record : m_list) {
			result.add( func.call(record) );
		}
		return result;
	}

	/**
	 * Join with another Bag by applying a function to each record
	 * in this Bag, and comparing that result to the result of
	 * funcOther applied to each element in the other bag.
	 * @param other the Bag to Join with
	 * @param funcHere the Function to apply to these elements
	 * @param funcOther the Function to apply to the other elements
	 * @return
	 */
	public Bag join(Bag other, Function funcHere, Function funcOther) {
		Bag results = new Bag();
		for (List recordHere : m_list) {
			Object valueHere= funcHere.call(recordHere);
			if (valueHere != null) {
				for (List recordOther : other.m_list) {
					Object valueOther = funcOther.call(recordOther);
					if (valueOther != null && valueHere.equals(valueOther)) {
						ArrayList joined = new ArrayList();
						joined.addAll(recordHere);
						joined.addAll(recordOther);
						results.add(joined);
					}
				}
			}
		}

		return results;
	}


	/**
	 * Join with another Bag with a single function that takes both
	 * records and compares them and returns a result.  A non-null
	 * result means we will keep it.
	 * @param other the other Bag to join with
	 * @param func the function that does all the work
	 * @return a list of results
	 */
	public Bag join(Bag other, Function<List> func) {
		Bag results = new Bag();
		for (Object objectHere : m_list) {
			for (Object objectOther : other.m_list) {
				ArrayList args = new ArrayList();
				args.add(objectHere);
				args.add(objectOther);
				List result = func.call(args);
				if (result != null) {
					results.add(result);
				}
			}
		}

		return results;
	}


// Debugging

	/**
	 * Useful for Debugging
	 */
	public String toString() {
		StringBuffer str = new StringBuffer();
		str.append("Size: " + m_list.size() + "\n");
		for (List record : m_list) {
			str.append(record.toString() + "\n");
		}
		return str.toString();
	}

}
