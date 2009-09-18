/*
 * bag.c
 *
 * C implementation by Will Johansson
 *
 * Compile with any C compiler:
 *		gcc -o bag bag.c
 *		cl.exe bag.c /out:bag.exe
 *
 * Major modification on 15/16 Sept 2009, for
 * betterment of memory management, and I
 * realized, technically I shouldn't be doing
 * is_person (person* data) and the like,
 * because a person* is not a "record" in the
 * functional sense, rather, the linked_list*
 * would be.
 *
 * Also added map function, yay! Pretty wicked
 * stuff going on. Only huge negative is that
 * can't do not() :( :( :( so map_not is included
 * as well in this API.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef _WIN32
#include <execinfo.h>
#endif

#define TRUE 1
#define FALSE 0

/* Type definitions */
typedef enum _type
{
	Person,
	Room,
	Join,
	String,
	HopeCallerKnows	/* :) This is very dangerous. No type safety.
			 * BE SURE TO KNOW WHAT YOU'RE DOING WITH THIS
			 * AND WHAT THE REAL DATA TYPE IS
			 */
} Type;

/* gender, person, room should be self documenting */
typedef enum _gender
{
	Male,
	Female
} gender;

typedef struct _person
{	gender sex;
	int phone;
	char *first;
	char *last;
} person;

typedef struct _room
{
	int number;
	int phone;
} room;

/* for the equijoin. this is kind of cheating, since i'm keeping all the fields
 * rather than just what we need for the list
 */
typedef struct _person_room
{	person *p;
	room *r;
} person_room;

/* Opaque linked list. Type information included to help
 * alleviate type safety.
 */
typedef struct _linked_list
{
	Type type;
	struct _linked_list *next;
	void *data;
} linked_list;

/* function pointers for predicates, comparing, mapping and processing */
typedef int(*predicate)(linked_list *ptr);
typedef int(*compare)(linked_list *a, linked_list *b);
typedef void*(*mapped)(linked_list *ptr);
typedef void(*process)(linked_list *ptr);

#ifndef _WIN32
/* Obtain a backtrace and print it to stdout.
 * Code STOLEN from http://www.gnu.org/software/libc/manual/html_node/Backtraces.html
 * VERY USEFUL!!!!!
 */
void print_trace (void)
{
	void *array[10];
	size_t size;
	char **strings;
	size_t i;
	
	size = backtrace (array, 10);
	strings = backtrace_symbols (array, size);

	printf ("Obtained %zd stack frames.\n", size);
     
	for (i = 0; i < size; i++)
		printf ("%s\n", strings[i]);

	free (strings);
}
#endif

/* generic "where" function */
linked_list *where_is (linked_list **head, predicate p, int is)
{
	linked_list *trav = *head;
	linked_list *where_list = NULL;
	
	while (trav != NULL)
	{
		if (p (trav) == is)
		{
		        /* predicate matched, make a new list with matched items */
			insert_list (&where_list,trav->data,trav->type);
		}
		
		trav = trav->next;
	}
	
	return where_list;
}

linked_list *where (linked_list **head, predicate p)
{
	return where_is (head, p, TRUE);
}

/* negation. can we some how do this in two calls
 * like where( not( ... ) )
 * or just somehow figure out that void *ptr for the predicate argument
 * is also a predicate, but this would require us to have
 * a full scale object orientation kit in C! nothing too complicated,
 * but just a lot of boilerplate code... and more abuse of the C language :)
 */
linked_list *where_not (linked_list **head, predicate p)
{
	return where_is (head, p, FALSE);
}

/* insert an opaque data in the list */
int insert_list (linked_list **head, void *data, Type type)
{
	#ifdef DEBUG
	printf ("insert_list:\n\tvoid *data: %p\n---\n", data);
	
	if (type == Person)
	{
		printf("PERSON INSERTING %s %d\n", ((person*)data)->first, ((person*)data)->phone);
	} else
	
	if (type == Room)
	{
		printf("ROOM INSERTING: %d %d\n", ((room*)data)->number, ((room*)data)->phone);
	} else
	
	if (type == String)
	{
		printf ("STRING INSERTING: (char *)data: %s\n", (char *)data);
	}
	#endif
	
	linked_list *current = *head;

	for (; current != NULL && current->next != NULL; current = current->next);

	if (current == NULL)
	{
		*head = calloc (sizeof(linked_list),1);

		if (*head == NULL)
		{
			fprintf (stderr, "calloc() failed for *head.\n");
			return 0;
		}

		(*head)->type = type;
		(*head)->data = data;
	}
	else
	{
		current->next = calloc (sizeof(linked_list),1);

		if (current->next == NULL)
		{
			fprintf (stderr, "calloc() failed for current->next.\n");
			return 0;
		}

		current->next->type = type;
		current->next->data = data;
	}
	
	return 1;
}

/* (define person? (lambda (record) (= (length record) 4))) */
int is_person (linked_list *ptr)
{
	return (ptr->type == Person ? TRUE : FALSE);
}

/* (define male?   (lambda (record) (list-ref record 3))) */
int is_male (linked_list *ptr)
{
	return (ptr->type == Person && (((person *)(ptr->data))->sex == Male) ? TRUE : FALSE);
}

/* (define name:   (lambda (record) (string-append (car record) " " (cadr record)))) */
char *get_persons_name (linked_list *ptr)
{
	person *p = (person*)(ptr->data);
	char *str = malloc (strlen(p->first) + strlen(p->last) + 2);
	sprintf (str, "%s %s", p->first, p->last);
	
	return str;
}

void print_person (person *p)
{
	printf ("%s %s %s %d\n",p->first, p->last, (p->sex == Male ? "MALE" : "FEMALE"), p->phone);
}

void print_room (room *r)
{
	printf ("%d %d\n",r->number, r->phone);
}

/* didn't work at one point. have not tested it since then, don't really use it. */
void print_person_room (person_room *pr)
{
	printf ("%s %s %s %d %d\n",pr->p->first, pr->p->last, (pr->p->sex == Male ? "MALE" : "FEMALE"), pr->p->phone, pr->r->number);
}

/* (define persons      (where person? cs)) */
linked_list *persons (linked_list **list)
{
	return where (list, is_person);
}

/* (define room?   (lambda (record) (= (length record) 2))) */
int is_room (linked_list *ptr)
{
	return (ptr->type == Room ? TRUE : FALSE);
}

/* (define person_room?   (lambda (record) (= (length record) 2))) */
int is_person_room (linked_list *ptr)
{
	return (ptr->type == Join ? TRUE : FALSE);
}

/* (define string? ...) */
int is_string (linked_list *ptr)
{
	return (ptr->type == String ? TRUE : FALSE);
}

/* (define string? ...) */
int is_unknown (linked_list *ptr)
{
	return (ptr->type == HopeCallerKnows ? TRUE : FALSE);
}

/* (define room:   (lambda (record) (car record))) */
int* get_room_number (linked_list *ptr)
{
	room *r = (room*)(ptr->data);
	int *num = malloc (sizeof(int));
	*num = r->number;
	
	return num;
}

/* print the linked list */
void print_list (linked_list **head)
{
	linked_list *trav = *head;
	
	while (trav != NULL)
	{
		if (is_person (trav))
		{
			print_person ((person*)(trav->data));
		}
		else if (is_room (trav))
		{
			print_room ((room*)(trav->data));
		}
		else if (is_person_room (trav))
		{
		        /* not tested, none of the lists except joins have this
		         * and they already have a print function anyway
		         */
			print_person_room ((person_room *)(trav->data));
		}
		else if (is_string (trav))
		{
			printf ("%s\n", (char *)(trav->data));
		}
		
		trav = trav->next;
	}
}

/* boiler plate instance stuff */
person *new_person(char *first, char *last, gender sex, int phone)
{
	person *p = NULL;
	int first_len;
	int last_len;
	
	if (first == NULL || last == NULL || phone < 0)
		return NULL;
		
	first_len = strlen (first);
	last_len = strlen (last);
	
	if ((p = calloc (sizeof(person), 1)) != NULL) {
		p->first = calloc (first_len + 1, 1);
		p->last = calloc (last_len + 1, 1);
		p->first = strncpy (p->first, first, first_len + 1);
		p->last = strncpy (p->last, last, last_len + 1);
		p->sex = sex;
		p->phone = phone;
	}
	else
	{
		fprintf(stderr, "calloc() failed for new_person\n");
	}
	
	return p;
}

room *new_room (int number, int phone)
{
	room *r = NULL;
	
	if((r = calloc (sizeof(room), 1)) != NULL)
	{
		r->number = number;
		r->phone = phone;
	}
	else
	{
		fprintf(stderr, "calloc() failed for new_room()\n");
	}
	
	return r;
}

person_room *new_person_room (person *p, room *r)
{
	person_room *pr = NULL;
	
	if ((pr = calloc (sizeof(person_room), 1)) != NULL)
	{
		pr->p = p;
		pr->r = r;
	}
	else
	{
		fprintf(stderr, "calloc() failed for new_person_room()\n");
	}
	
	return pr;
}

/* boiler plate delete stuff */
void free_person (person *p)
{
	#ifdef DEBUG
	printf ("free_person:\n\tperson *p: %p\n---\n", p);
	#endif

	if (p != NULL)
	{
		if (p->first != NULL)
			free (p->first);
			
		if (p->last != NULL)
			free (p->last);

		free(p);
	}
}

void free_room (room *r)
{
	if (r != NULL)
	{
		free(r);
	}
}

void free_list (linked_list **head, int nodes_only)
{
	linked_list *current = *head;
		
	while (current != NULL)
	{
		linked_list *tmp = current->next;
		
		if (!nodes_only)	
			if (is_person(current))
				free_person((person*)(current->data));
			else if (is_room(current))
				free_room((room*)(current->data));
			else if (is_string(current) || is_unknown(current) || is_person_room(current))
				free(current->data); /* sure as hell we don't have a memory leak from unknown data :D */
				
		free(current);

		current = tmp;
	}
}

/* mapping function. apply map to list, with mapping function function and predicate! */
linked_list *map_is (linked_list **list, mapped function, predicate p, int is)
{	
	linked_list *pred_application = where_is (list, p, is);
	linked_list *current = pred_application;
	linked_list *newlist = NULL;
	
	while (current != NULL)
	{
		insert_list (&newlist, function(current), HopeCallerKnows);
		current = current->next;
	}
	
	free_list (&pred_application, TRUE);
	
	return newlist;
}

linked_list *map (linked_list **list, mapped function, predicate p)
{
	return map_is (list, function, p, TRUE);
}

linked_list *map_not (linked_list **list, mapped function, predicate p)
{
	return map_is (list, function, p, FALSE);
}

/* (define male-names   (map name: (where male? persons))) */
linked_list *male_names (linked_list ** list)
{
	return map (list, (void *)get_persons_name, is_male);
}

/* (define female-names (map name: (where (not? male?) persons))) */
linked_list *female_names (linked_list ** list)
{
	return map_not (list, (void *)get_persons_name, is_male);
}

/* (define rooms   (map room: (where room? cs))) */
linked_list *rooms (linked_list **list)
{
	return map (list, (void *)get_room_number, is_room);
}

/* for all list elements, apply process p to it */
void for_all(linked_list **ptr, process p)
{
	linked_list *current = *ptr;
	
	while (current != NULL)
	{
		p(current);
		current = current->next;
	}
}

/* processes */
void print_data_as_char (linked_list *ptr)
{
	printf ("%s\n", (char*)(ptr->data));
}

void print_data_as_int (linked_list *ptr)
{
	printf ("%d\n", *((int *)(ptr->data)));
}

void print_first_name_and_room (linked_list *ptr)
{
	person_room *pr = (person_room*)(ptr->data);
	printf ("%s %d\n", pr->p->first, pr->r->number);
}

void print_last_name_and_room (linked_list *ptr)
{
	person_room *pr = (person_room*)(ptr->data);
	printf ("%s %d\n", pr->p->last, pr->r->number);
}
/* end processes */

/* Get room belonging to the person */
/* people, rooms */
int does_room_belong_to_person (linked_list *a, linked_list *b)
{
	person *p = (person *)(a->data);
	room *r = (room *)(b->data);
	
	return (p->phone == r->phone);
}

/* The EQUI-JOIN function. Returns a list of person_room objects
 * from which we can apply a process function to print or use
 * only what we want to use. kind of cheating.
 */
linked_list* join (linked_list** a, linked_list** b, compare cmp)
{
	linked_list *list = NULL;
	linked_list *current = *a;
	linked_list *i = NULL;
	int index = 0;
	
	while (current != NULL) {
		i = *b;
		
		while (i != NULL)
		{
			if (cmp(current, i)) {

				person_room *pr = calloc (sizeof(person_room), 1);
				pr->p = current->data;
				pr->r = i->data;
				
				printf( "inserting index: %d\n", ++index);
				
				insert_list (&list, pr, Join);
			}
			
			i = i->next;
		}
		
		printf ("IS NEXT NULL? %s\n", (current->next == NULL ? "YES" : "NO"));
		
		current = current->next;
	}

	return list;
}

/* this code should be largely self documenting
 * I didn't go with a function call to return a list every time
 * I wanted Persons() or whatever, because this way I simplify
 * memory management in C a *LOT*. If I had another week, I
 * probably would have implemented reference counting
 * and automatic memory management and maybe a full object
 * orientation stack necessary for more sugar
 * but heck, this works well.
 */
int main(int argc, char *argv[])
{
	linked_list *ll = NULL;

	linked_list *peeps = NULL;
	linked_list *office_numbers = NULL;
	linked_list *offices = NULL;
	linked_list *males = NULL;
	linked_list *females = NULL;
	linked_list *mnames = NULL;
	linked_list *fnames = NULL;
	linked_list *mnames_room = NULL;
	linked_list *fnames_room = NULL;
	
	insert_list (&ll, new_person ("Sandy", "Ferrara", Female, 55178), Person);
	insert_list (&ll, new_person ("Tina", "Sturgis", Female, 57905), Person);
	insert_list (&ll, new_person ("Joanne", "Catan", Female, 56084), Person);
	insert_list (&ll, new_person ("Eileen", "Wilczak", Female, 57146), Person);
	insert_list (&ll, new_person ("Jason", "Harrison", Male, 52529), Person);
	insert_list (&ll, new_person ("Christina", "Rohr", Female, 52995), Person);
	insert_list (&ll, new_person ("Liane", "Fitzgerald", Female, 52994), Person);
	insert_list (&ll, new_person ("James", "Craig", Male, 55254), Person);
	insert_list (&ll, new_person ("Sam", "Waters", Male, 54934), Person);
	insert_list (&ll, new_room (3021, 55178), Room);
	insert_list (&ll, new_room (3012, 57905), Room);
	insert_list (&ll, new_room (3671, 57905), Room);
	insert_list (&ll, new_room (3008, 56084), Room);
	insert_list (&ll, new_room (3005, 57146), Room);
	insert_list (&ll, new_room (3005, 52529), Room);
	insert_list (&ll, new_room (3022, 52995), Room);
	insert_list (&ll, new_room (3022, 52994), Room);
	insert_list (&ll, new_room (3599, 55254), Room);
	insert_list (&ll, new_room (3596, 54934), Room);
	
	peeps = persons (&ll);
	offices = where (&ll, is_room);
	office_numbers = rooms (&ll);
	males = where (&peeps, is_male);
	puts( "males:");
	print_list (&males);
	females = where_not (&peeps, is_male);
	mnames = male_names (&peeps);
	fnames = female_names (&peeps);
	mnames_room = join (&males,&offices,does_room_belong_to_person); 
	fnames_room = join (&females,&offices,does_room_belong_to_person);
	
	puts ("PEOPLE:");
	print_list (&peeps);

	puts ("\nROOMS:" );
	for_all (&office_numbers, print_data_as_int);

	puts ("\nMALE PEOPLE:");
	for_all (&mnames, print_data_as_char);

	puts ("\nFEMALE PEOPLE:");
	for_all (&fnames, print_data_as_char);

	puts ("\nEQUIJOIN MALE PEOPLE + ROOMS:");
	for_all (&mnames_room, print_first_name_and_room);
	
	puts ("\nEQUIJOIN FEMALE PEOPLE + ROOMS:");
	for_all (&fnames_room, print_last_name_and_room);
	
	free_list (&ll,FALSE);
	free_list (&peeps,TRUE);
	free_list (&office_numbers,FALSE);
	free_list (&offices,TRUE);
	free_list (&males,TRUE);
	free_list (&females,TRUE);
	free_list (&mnames,FALSE);	free_list (&fnames,FALSE);
	free_list (&mnames_room,FALSE);	free_list (&fnames_room,FALSE);

	return 0;
}
