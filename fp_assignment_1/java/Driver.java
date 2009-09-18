import java.util.ArrayList;
import java.util.List;

/**
 * Test Harness
 * @author Joseph Pecoraro
 */
public class Driver {

// Data Types

	class Person extends ArrayList {
		public Person(String first, String last, int phone, boolean isMale) {
			add(first); add(last); add(phone); add(isMale);
		}
	}

	class Room extends ArrayList {
		public Room(int room, int phone) {
			add(room); add(phone);
		}
	}

// Runner

	public Driver() {

		// Predicate to Detect if a Record is a Person [in any Bag]
		Predicate personFilter = new Predicate() {
			public Boolean call(List record) {
				return (record instanceof Person);
			}
		};

		// Predicate to Detect a Male Person [in a Bag of Persons]
		Predicate maleFilter = new Predicate() {
			public Boolean call(List record) {
				return (Boolean)(record.get(3));
			}
		};

		// All the Data
		Bag data = generateData();

		// Get the Persons
		Bag persons = data.where(personFilter);
		output("Persons", persons);

		// Get the Males
		Bag males = persons.where(maleFilter);
		output("Males", males);

		// Get the Non-Persons
		Predicate nonpersonFilter = NotPredicate.not(personFilter);
		Bag nonpersons = data.where(nonpersonFilter);
		output("Non-Persons (Rooms)", nonpersons);

		// Test a Mapping to get a List of Names of the persons [in a Bag of Persons]
		List<String> names = (List<String>) persons.map(new Function<String>() {
			public String call(List record) {
				String first = (String)record.get(0);
				String last = (String)record.get(1);
				return (first + " " + last);
			}
		});
		output("Names of Persons", names);


		// Persons (x) Rooms => [FirstName, LastName, Room#]
		Function<List> joinOnPhone = new Function<List>() {
			public List call(List args) {
				Person person = (Person)args.get(0);
				Room room = (Room)args.get(1);
				int personPhone = (Integer)person.get(2);
				int roomPhone = (Integer)room.get(1);
				if (personPhone != roomPhone) {
					return null;
				} else {
					List joined = new ArrayList();
					joined.add(person.get(0)); // first name
					joined.add(person.get(1)); // last name
					joined.add(room.get(0)); // room number
					return joined;
				}
			}
		};

		Bag thePersons = data.where(personFilter);
		Bag theRooms = data.where( NotPredicate.not(personFilter) );
		Bag theJoin = thePersons.join(theRooms, joinOnPhone);
		output("Join On Phone Number", theJoin);

	}


// Helpers

	/**
	 * Create Sample Data
	 * @return a Bag of sample data
	 */
	public Bag generateData() {
		Bag data = new Bag();
		data.add( new Person("Sandy", "Ferrara", 55178, false) );
		data.add( new Person("Tina", "Sturgis", 57905, false) );
		data.add( new Person("Joanne", "Catan", 56084, false) );
		data.add( new Person("Eileen", "Wilczak", 57146, false) );
		data.add( new Person("Jason", "Harrison", 52529, true) );
		data.add( new Person("Christina", "Rohr", 52995, false) );
		data.add( new Person("Liane", "Fitzgerald", 52994, false) );
		data.add( new Person("James", "Craig", 55254, true) );
		data.add( new Person("Sam", "Waters", 54934, true) );
		data.add( new Room(3021, 55178) );
		data.add( new Room(3012, 57905) );
		data.add( new Room(3671, 57905) );
		data.add( new Room(3008, 56084) );
		data.add( new Room(3005, 57146) );
		data.add( new Room(3005, 52529) );
		data.add( new Room(3022, 52995) );
		data.add( new Room(3022, 52994) );
		data.add( new Room(3599, 55254) );
		data.add( new Room(3596, 54934) );
		return data;
	}

	/**
	 * Clean Output
	 */
	public void output(String title, Object o) {
		System.out.println(title + ":");
		System.out.println(o);
		System.out.println();
	}


// Main

	public static void main(String[] args) {
		new Driver();
	}

}
