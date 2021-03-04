# Daison
Daison (DAta lISt comprehensiON) is an experiment to create a database
which natively stores Haskell data types, instead of using 
the traditional SQL tables. The backend storage is SQLite from which 
I have stripped all SQL related features. The result is a 
simple key-value storage. On top of that there is a Haskell API 
which replaces the SQL language. For instance, Queries are simple 
list comprehensions generalized to a special monad.

For more details read the [tutorial](doc/tutorial.md).
