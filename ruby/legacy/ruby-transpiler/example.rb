# test_file.rb

# Modules
module MyModule
  def module_method
    puts "This is a method from MyModule"
  end
end

# Classes and inheritance
class Animal
  def speak
    puts "The animal makes a sound"
  end
end

class Dog < Animal
  include MyModule

  attr_accessor :name, :age

  def initialize(name, age)
    @name = name
    @age = age
  end

  def speak
    puts "The dog barks"
    super
  end
end

# Method definition
def greet(name)
  "Hello, #{name}!"
end

# Loops and conditionals
3.times do |i|
  if i.even?
    puts "#{i} is even"
  else
    puts "#{i} is odd"
  end
end

[1, 2, 3].each { |n| puts "Number: #{n}" }

# Blocks
def call_block
  yield
end

call_block { puts "Inside the block" }

# Lambdas and Procs
my_lambda = ->(x) { x * 2 }
my_proc = Proc.new { |x| x * 2 }

puts my_lambda.call(5)
puts my_proc.call(5)

# Exception handling
begin
  1 / 0
rescue ZeroDivisionError => e
  puts "Caught an exception: #{e.message}"
ensure
  puts "This will always run"
end

# Strings, arrays, and hashes
str = "Hello, world!"
arr = [1, 2, 3, 4, 5]
hsh = { a: 1, b: 2, c: 3 }

# Using the classes, methods, and modules defined earlier
dog = Dog.new("Buddy", 4)
dog.speak
dog.module_method
puts greet("John")
