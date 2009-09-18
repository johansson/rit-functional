// Javascript
// Author: Joseph Pecoraro
// Date: Thursday, September 10, 2009
// Description: Functional Programming Assignment #1

// ------
//  Data
// ------

var data = [
    ["Sandy", "Ferrara", 55178, false],
    ["Tina", "Sturgis", 57905, false],
    ["Joanne", "Catan", 56084, false],
    ["Eileen", "Wilczak", 57146, false],
    ["Jason", "Harrison", 52529, true],
    ["Christina", "Rohr", 52995, false],
    ["Liane", "Fitzgerald", 52994, false],
    ["James", "Craig", 55254, true],
    ["Sam", "Waters", 54934, true],
    [3021, 55178], [3012, 57905],
    [3671, 57905], [3008, 56084],
    [3005, 57146], [3005, 52529],
    [3022, 52995], [3022, 52994],
    [3599, 55254], [3596, 54934]
];

// ---------------------
//  Scheme Fundamentals
// ---------------------

function car(lst) {
    return lst[0];
}

function cdr(lst) {
    var copy = []; // lame, I know
    for (var i=0, len=lst.length; i<len; ++i)
        copy[i] = lst[i];
    return copy.splice(1);
}

if (!Array.prototype.map) {
    Array.prototype.map = function(func) {
        for (var i=0, len=this.length; i<len; ++i) {
            this[i] = func(this[i]);
        }
        return this;
    }
}

// ----------------
//  Query Language
// ----------------

function where(pred, data) {
    if (!data)
        return null;

    var row = car(data);
    if (!row)
        return null;

    var theRest = where(pred, cdr(data));
    if (!pred(row))
        return theRest;

    if (!theRest)
        return [row];

    return [row].concat(theRest);
}

function not(pred) {
    return function(row) {
        return !pred(row);
    };
}

function join(func, data1, data2) {
    results = [];
    for (var i=0; i<data1.length; ++i) {
        for (var j=0; j<data2.length; ++j) {
            var value = func(data1[i], data2[j]);
            if (value) {
                results.push(value);
            }
        }
    }
    return results;
}

// --------------
//  Clean Output
// --------------

Array.prototype._toString = Array.prototype.toString;
Array.prototype.toString = function() {
    return "[" + this._toString() + "]";
}

function output(str, data) {
    print(str + ':');
    print('Size: ' + data.length);
    print('[' + data.join(",\n ") + ']');
    print();
}

// ---------
//  Queries
// ---------

function personFilter(row) {
    return row.length === 4;
}

function maleFilter(row) {
    return row[3];
}

function nameMapping(row) {
    return row[0] + ' ' + row[1];
}

function joinOnPhone(person, room) {
    return (person[2] === room[1]) ? [person[0], person[1], room[0]] : null;
}


var persons = where(personFilter, data);
var nonpersons = where(not(personFilter), data);
var males = where(maleFilter, data);
var names = persons.map(nameMapping);
var joined = join(joinOnPhone, persons, nonpersons);

output('Persons', persons);
output('Males', males);
output('Non-Persons (Rooms)', nonpersons);
output('Names', names);
output('Joined', joined);
