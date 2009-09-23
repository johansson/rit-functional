// Author: Joseph Pecoraro
// Date: Thursday, September 17, 2009
// Description: Function Programming Assignment #2
// JavaScript

// ----------
//  Plumbing
// ----------

Array.prototype.empty = function() {
    return this.length === 0;
}

Array.prototype.dup = function() {
    return this.slice(0);
}

Array.prototype.getHead = function() {
    return evaluateIfNeeded(this.dup())[0];
}

Array.prototype.getTail = function() {
    var tail = evaluateIfNeeded(this).slice(1);
    return evaluateIfNeeded(tail);
}

function evaluateIfNeeded(lst) {
    return (typeof lst[0] === 'function') ? lst[0]() : lst;
}


// -----------
//  Consumers
// -----------

function take(n, lst) {
    if (n <= 0) return [];
    if (lst.empty()) return [];
    var head = lst.getHead();
    var tail = lst.getTail();
    return [head].concat(take(n-1, tail));
}


// -------------------
//  Basic Enumerators
// -------------------

function repeat(n) {
    return [n, function() { return repeat(n); }];
}

function climb(start, step) {
    step = (step) ? step : 1;
    return [start, function() { return climb(start+step, step); }];
}


// -----------------
//  Basic Iterators
// -----------------

function map(func, lst) {
    if (lst.empty()) return [];
    var head = lst.getHead();
    var tail = lst.getTail();
    return [func(head), function() { return map(func, tail); } ];
}

function zipWith(func, lst1, lst2) {
    if (lst1.empty() || lst2.empty()) return [];
    var head1 = lst1.getHead();
    var head2 = lst2.getHead();
    var tail1 = lst1.getTail();
    var tail2 = lst2.getTail();
    return [func(head1, head2), function() { return zipWith(func, tail1, tail2) }];
}

function scanl(func, mem, lst) {
    if (lst.empty()) return [];
    var head = lst.getHead();
    var tail = lst.getTail();
    var x = func(mem, head);
    return [x, function() { return scanl(func, x, tail); }];
}


// -----------------
//  Assignment Work
// -----------------

function iterate(func, start) {
    return [start, function() { return iterate(func, func(start)); }];
}

function powers(n) {
    var lambda = function(a) { return a*n; }
    return iterate(lambda, n);
}

function square(n) {
    return iterate(sq, n);
}

function seq(n, lst) {
    return [n].concat(lst);
}

function mapl(func, lst) {
    var first = lst.getHead();
    var tail = lst.getTail();
    var second = tail.getHead();
    return [func(first, second), function() { return mapl(func, tail) }];
}

function fibs(a,b) {
    return helper(a,b);
    function helper(x, y) {
        return [x, function() { return helper(y, x+y) }]
    }
}

function factorial() {
    return helper(1,1);
    function helper(mem, n) {
        var curr = mem*n;
        return [curr, function() { return helper(curr, n+1); }];
    }
}


// ---------
//  Aliases
// ---------

var total = scanl;


// ----------------
//  Infinite Lists
// ----------------

var ones = repeat(1);
var ints = climb(1);
var evens = climb(2,2);
var odds = climb(1,2);
var squares = map(sq, ints);
var facts = factorial();
var fibseq = fibs(1,1);


// -------------------
//  Generic Functions
// -------------------

function add(a,b) { return a+b; }
function mult(a,b) { return a*b; }
function add1(a) { return a+1; }
function sq(a) { return a*a; }


// ------------------
//  Terminal Helpers
// ------------------

function r() {
    load('seq.js');
}


// ------------
//  Test Cases
// ------------

// Allow output on Browsers (Firefox) as well as command line interpreters
// NOTE: I have to wrap console.log for WebKit / Safari / Chrome
// https://bugs.webkit.org/show_bug.cgi?id=20141
var printFunction = (this.window && this.console ?
    function() { console.log.apply(console, arguments); } :
    print);

function compareArrays(a1, a2) {
    if (a1.length !== a2.length)
        return false;
    for (var i=0, len=a1.length; i<len; ++i)
        if (a1[i] !== a2[i])
            return false;
    return true;
}


function runTest(str, actual, expected) {
    var pass = compareArrays(actual, expected);
    var passString = (pass ? "PASS" : "FAIL");
    printFunction(passString + ":   " + str + " => " + actual + " : " + expected);
}


printFunction( "Running Tests..." );
printFunction( "-------------------------------------------------------------------------");
printFunction( "RESULT  INPUT                                OUTPUT    : EXPECTED");
printFunction( "-------------------------------------------------------------------------");
runTest( "take(5, ones)                    ", take(5, ones)                    , [1,1,1,1,1]);
runTest( "take(5, map(add1, ones))         ", take(5, map(add1, ones))         , [2,2,2,2,2]);
runTest( "take(5, zipWith(add, ones, ones))", take(5, zipWith(add, ones, ones)), [2,2,2,2,2]);
runTest( "take(5, ints)                    ", take(5, ints)                    , [1,2,3,4,5]);
runTest( "take(5, iterate(add1, 1))        ", take(5, iterate(add1, 1))        , [1,2,3,4,5]);
runTest( "take(5, evens)                   ", take(5, evens)                   , [2,4,6,8,10]);
runTest( "take(5, odds)                    ", take(5, odds)                    , [1,3,5,7,9]);
runTest( "take(5, squares)                 ", take(5, squares)                 , [1,4,9,16,25]);
runTest( "take(5, powers(2))               ", take(5, powers(2))               , [2,4,8,16,32]);
runTest( "take(5, square(2))               ", take(5, square(2))               , [2,4,16,256,65536]);
runTest( "take(5, seq(0, ints))            ", take(5, seq(0, ints))            , [0,1,2,3,4]);
runTest( "take(5, mapl(add, seq(0, ints))) ", take(5, mapl(add, seq(0, ints))) , [1,3,5,7,9]);
runTest( "take(5, fibs(1,2))               ", take(5, fibs(1,2))               , [1,2,3,5,8]);
runTest( "take(5, total(add, 0, ints))     ", take(5, total(add, 0, ints))     , [1,3,6,10,15]);
runTest( "take(5, facts)                   ", take(5, facts)                   , [1,2,6,24,120]);
runTest( "take(5, zipWith(mult, fibseq, squares))", take(5, zipWith(mult, fibseq, squares)), [1, 4, 18, 48, 125]);
printFunction( "Tests Complete..." );
""; // Suppress Output from load();
