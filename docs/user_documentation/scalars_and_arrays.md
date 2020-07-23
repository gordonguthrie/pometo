# Scalars And Arrays

The basic data structures of `Pometo` are scalars and arrays.

A scalar is not implemented as a vector of length 1.

# Scalar Types

The basic scalar types are:

* `number` - the individual element here might be an integer or a float - the casting is done as per normal `Erlang`, `Elixir` rules
* `boolean` - each element is the integer `0` or `1` - will be cast to `number` seemlessly if appropriate
* `array` - it is possible to have an `array` as a `scalar`

# Array Types

Array's take the type of their scalars (if all the scalars are of the same type)

* `number`
* `boolean`
* `array`

An array can also take the type `mixed` when it is not obvious at write time what they types of the `scalar`s are - an example might be an array of numbers and arrays of numbers.

Attempts to do type-invalid on `mixed` arrays might result in a runtime error.

# Vectors

Vectors are just `array`s with a single dimension. They have no implementation specifities. They are important though for some functions. The `shape` or `rho` function `⍴` can only take a vector on the LHS not a generalised `array`.
