#!/usr/bin/env ruby
# Author: Joseph Pecoraro
# Date: Thursday, September 10, 2009
# Description: Functional Programming Assignment #1

# Pretty Printing Lib, Makes the Output Nicer
require 'pp'

# ------
#  Data
# ------

data = [
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


# -----------------
#  The Assignment
# -----------------

def where(pred, bag)
  bag.select(&pred)
end

def not?(pred)
  lambda { |e| !pred.call(e) }
end

def join(func, bag1, bag2)
  results = []
  bag1.each do |a|
    bag2.each do |b|
      val = func.call(a, b)
      results.push(val) unless val.nil?
    end
  end
  results
end

# --------------
#  Test Harness
# --------------

def output(str, data)
  puts "#{str}:"
  puts "Size: #{data.size}" if data.respond_to? :size
  pp data
  puts
end

# NOTE:
# lambda { |x| ... } is syntactic sugar for
# Proc.new do |x| ... end
personFilter = lambda { |x| x.length == 4 }
maleFilter = lambda { |x| x[3] }
nameMapping = lambda { |x| "#{x[0]} #{x[1]}" }
joinFunc = Proc.new do |p, r|
  p[2] == r[1] ? [p[0], p[1], r[0]] : nil
end

persons = where(personFilter, data)
males = where(maleFilter, persons)
notpersons = where(not?(personFilter), data)
names = persons.map(&nameMapping)
joined = join(joinFunc, persons, notpersons)

output("Persons", persons)
output("Non-Persons", notpersons)
output("Males", males)
output("Names", names)
output("Join on Phone Number", joined)
