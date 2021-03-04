# The Daison Tutorial

Daison (DAta lISt comprehensiON) is a database where the data management language is Haskell instead of SQL. The main idea is that instead of SELECT statements, you use Haskell's List Comprehensions generalized to monads by using the Monad Comprehension extension. The other benefit is that the database can store any serializable data type defined in Haskell. This avoids the need to convert between Haskell types and SQL types for every query. An added benefit is that Daison naturally supports algebraic data types which is problematic in relational databases.

The backend storage is SQLite from which I have stripped all SQL related features. The result is a simple key-value storage,  on top of which there is a Haskell API which replaces the SQL language. This gives us the efficiency and the reliability of an established database, but without the intermediatry of the SQL interpreter.

The following tutorial will introduce how to perform different operations in Daison, sometimes in comparison with SQL.

## Opening and closing a database

Opening and closing a database is simply:
```haskell
main = do db <- openDB "student_database.db"
          closeDB db
```
If a file with that name does not exist then a new database with that name will be created. The new database will not have any tables by default. Like in SQL you have to create the tables by executing certain operations.

## Transactions

In order to do anything with a Daison database, you first need to start a transaction. This applies both to read-only as well as read-write operations. The backend allows multiple reader - single writer access. This means that even if you only read from the database, the engine must know when you are done in order to allow changes. What kind of access you need is described with the `AccessMode` data type:
```haskell
data AccessMode = ReadWriteMode | ReadOnlyMode
```

The transaction itself is started by using:
```haskell
runDaison :: Database -> AccessMode -> Daison a -> IO a
```
This function takes a database and an access mode and performs an operation in the `Daison` monad. The operation can create/drop tables, query the data or modify the data. If the operation is performed without any exceptions, then the transaction will be automatically committed. In case of exceptions the transaction will be rolled back.

## Defining and creating tables

Although this is a NoSQL database it is still based on the concept of tables. A table however doesn't have any columns, it is just a sequence of rows, where each row stores one Haskell value. The value, of course, could be of record type, so in that sense Daison tables could still have columns.

Before we create a table, we need to define the data type for the rows:
```haskell
data Student
  = Student 
     { name   :: String
     , code   :: Int
     , grades :: [Int]
     }
     deriving Data
```
Note that you need to derive an instance of `Data`, since Daison uses it for generic serialization/deserialization.

Once you have all necessary data types, you can declare the associated tables and indices:
```haskell
students :: Table Student
students = table "students"
           `withIndex` students_name
           `withIndex` students_grade
           `withIndex` students_avg_grade

students_name :: Index Student String
students_name = index students "name" name

students_grade :: Index Student Int
students_grade = listIndex students "grade" grades

students_avg_grade :: Index Student String
students_avg_grade = index students "avg_grade" avg_grade

avg_grade :: Student -> Double
avg_grade s = fromIntegral (sum (grades s)) / fromIntegral (length (grades s))
```
As you can see the tables/indices are just Haskell functions defined by using primitives from the Daison DSL. Here:
```haskell
table :: String -> Table a
```
takes a table name and returns a value of type Table. By default every table has an `Int64` primary key and arbitrary value as a content. If you need indices, then they must be added with the `withIndex` primitive:
```haskell
withIndex :: Data b => Table a -> Index a b -> Table a
```
The index itself is defined in one of three possible ways:

- The simplest way is by using the `index` primitive:
  ```haskell
  index :: Table a -> String -> (a -> b) -> Index a b
  ```
  It takes a table, an index name and an arbitrary function which from a row value computes the value by which the row must be indexed. In the example above there two indexes of this kind - `students_name` and `students_avg_grade`. When the row value is of record type, it is natural that some of the indices will be over a particular field. This is the case with `students_name`, but it does not have to be the case. For example `students_avg_grade` indexes over the average grade which is not stored, but is computed every time when a row is inserted or updated. In the SQL terminology this is called "computed index" and is supported by some SQL databases but not all.

- You can also index a row by more than one value by using the primitive:
  ```haskell
  listIndex :: Table a -> String -> (a -> [b]) -> Index a b
  ```
  In the above example we have the index `students_grade` which lets you to search for students who got a particular grade. This kind of indices does not have correspondence in SQL since in (most) relational databases you cannot store lists.

- A special case is when you want to index some rows but not all. This is possible with the primitive:
  ```haskell
  maybeIndex :: Table a -> String -> (a -> Maybe b) -> Index a b
  ```
  when the indexing function returns `Nothing` then the current row will be skipped from the index. This is equivalent to an index over a nullable column in SQL.

Finally, when a table is defined, you must also create it. The definitions above are just Haskell functions and they do not do anything with the database. You should call `createTable` explicitly:
```haskell
runDaison db ReadWriteMode $ do
  createTable students
```
This creates both the table and the indices which are associated with it. Note that since this operation changes the database, it must be executed within a read-write transaction.

If a table with that name already exists, `createTable` will fail. If you want to create a table only if it is not created yet then use `tryCreateTable` instead.

After a table is created you can rename it:
```haskell
runDaison db ReadWriteMode $ do
  bar <- renameTable foo "bar"
  ...
```
Once this is done, using the old table name `foo` will result in an error. In order to access the data you should use `bar` instead.

Changing the type of the table is possible with `alterTable`:
```haskell
runDaison db ReadWriteMode $ do
  alterTable foo bar modify
where
  modify = ...
```
Here you need the definitions of two tables `foo :: Table a` and `bar :: Table b`, and a function `modify :: a -> b` which transforms the old values into the new ones.

Finally if you want to remove a table just use `dropTable` or `tryDropTable`.


## Access the Data

### Insert

The simplest way to insert data in a table is the primitive:
```haskell
insert_ :: Data a => Table a -> a -> Daison (Key a)
```
It just takes a table and a value and inserts the new value in the table. The results is the primary key number. The type `Key a` is just a synonym for a 64-bit integer:
```haskell
type Key a = Int64
```

A more advanced way is to use the equivalent of `INSERT-SELECT` in SQL:
```haskell
insert :: Data a => Table a -> Query a -> Daison (Key a, Key a)
```
Instead of a single value, this primitive takes a query which can extract data from other tables in order to prepare the values to be inserted in the target table. The query itself is a monadic function which is often conveniently expressed as a monad comprehension. The result from `insert` is a pair of the initial and final primary keys of the newly inserted rows. Here is an example:
```haskell
runDaison db ReadWriteMode $ do
  (start,end) <- insert foo [f x | x <- from bar]
  ...
```
where we take all values from table `bar`, we apply a transformation function `f`, and finally we insert the results in table `foo`.

More details about queries follow.

### Select

The main primitive for extracting data from the database is:
```haskell
select :: QueryMonad m => Query a -> m [a]
```
Its only argument is a query which often is written as a monad comprehension. Note that the functon is overloaded over the return monad which allows us to use `select` both on the top-level within a transaction as well as nested inside another query.

#### Queries

Now we should go deeper into how queries are built. The main querying primitive is:
```haskell
from :: From s r => s -> r (K s) -> Query (V s r)
```
which has a complicated type but the only purpose of that type is to overload the function over four possible types:
```haskell
from :: Data a => Table a -> At (Key a) -> Query a
from :: Data a => Table a -> Restriction (Key a) -> Query (Key a, a)
from :: Data b => Index a b -> At b -> Query (Key a)
from :: Data b => Index a b -> Restriction b -> Query (b, Key a)
```
Obviously the first argument of `from` can be either a table or an index. The second argument must be of type `At`, if you search for a particular value. For example if you want to get the name of the student with primary key `1` then you should do:
```haskell
runDaison db ReadMode $ do
  select [name s | s <- from students (at 1)]
```
the corresponding SQL query would be:
```SQL
SELECT name FROM students WHERE id=1
```

If you don't know the particular value but you want to search in given range then instead you have:
```haskell
runDaison db ReadMode $ do
  select [(id,avg_grade) | (avg_grade,id) <- from students_avg_grade (everything ^> 4 ^< 5)]
```
which in SQL is:
```SQL
SELECT id,avg_grade FROM students WHERE avg_grade > 4 AND avg_grade < 5
```
In this example we used an index instead of a table. The `from` function works just as well but now it returns not the table row, but only the primary key. As in the relational databases, the tables contain the data, while the indices contain a mapping from a value to the list of primary keys whose rows are indexed under that value. This is reflected accordingly in the type signatures for `from`.

Another thing to note is that when `from` is used with a restriction then it also returns the actual matching key when it is used with a table, or the matching value when it is used with an index. This makes sense. When you use the primitive  `at` then you already know the right key/value, but when you use restriction then it only acts as a filter, and therefor it is useful to get the actual key or value.

A bit more about the `Restriction` type. A restriction can be either `everything`, `asc` or `desc`. All the three doesn't put any constraint on the selection. They basically say give me all rows. In addition `asc`/`desc` says that the result must be sorted in asceding/descending order. Note that ordering is done by a generic ordering function which is based on the `Data` instance. Even if you define a custom `Ord` instance this would not affect the ordering in Daison. The generic ordering follows the strategy used for the automatic derivation of `Ord`, so if you use `deriving Ord` you will get consistent results. The reason for that choice is that it lets us to avoid deserializing all key values when we do search in an index.

Lastly, a restriction can be modified with zero or more of the (^<), (^<=), (^>) or (^>=) operators. This lets so you to specify an open or closed interval of allowed values. Any other constraint should be placed as an ordinary guard, for example:
```haskell
runDaison db ReadMode $ do
  select [n | n <- from numbers everything ^> 1, isPrimeNumber n]
```

Finally, as we said using `from` with an index only gives you the primary key. It is quite frequent, however, that you also want the full table row. You can do it as follows:
```haskell
runDaison db ReadMode $ do
  select [(id,row,v) | (v,id) <- from foo_index everything, row <- from foo (at id)]
```
but you can also use the shorthand:
```haskell
runDaison db ReadMode $ do
  select [(id,row,v) | (id,row,v) <- fromIndex foo_index everything]
```
where just like `from`, `fromIndex` is overloaded:
```haskell
fromIndex :: (Data a, Data b) => Index a b -> At b -> Query (Key a, a)
fromIndex :: (Data a, Data b) => Index a b -> Restriction b -> Query (Key a, a, b)
```

#### Aggregation

The result from `select` is always the list of selected rows. In SQL on the other hand, the rows can be aggregated by using aggregate functions like `SUM` and `AVERAGE`. You can of course select all relevant rows in a list and then post-process them in Haskell, but this might mean that you have to collect a lot of data, just in order to transform it after that. A better alternative is to use the primitive `query`  instead of `select`:
```haskell
query :: QueryMonad m => Aggregator a b -> Query a -> m b
```
Here you can think of the `Aggregator` as a function which transforms a sequence of rows of type `a` (the query) into a result of type `b` but in incremental fashion. There are several built-in aggregators:
- ```haskell
  listRows :: Aggregator a [a]
  ```
  just collects the rows into a list. In fact `select` is defined as:
  ```haskell
  select = quert listRows
  ```
  
- ```haskell
  distinctRows :: Ord a => Aggregator a (Set a)
  ```
  retains only the unique rows and returns a `Set` instead of list.

- ```haskell
  firstRow :: Aggregator a a
  ```
  returns the first row and ignores the rest.

- ```haskell
  lastRow :: Aggregator a a
  ```
  skips all initial rows and returns the last one.

- ```haskell
  topRows :: Int -> Aggregator a [a]
  ```
  returns the first few rows.

- ```haskell
  bottomRows :: Int -> Aggregator a [a]
  ```
  returns the last few rows.

- ```haskell
  sumRows :: Num a => Aggregator a a
  ```
  sums all the rows and returns the result.

- ```haskell
  averageRows :: Fractional a => Aggregator a a
  ```
  computes the average of the selected numbers

- ```haskell
  countRows :: Fractional a => Aggregator a a
  ```
  counts the selected rows

- ```haskell
  foldRows :: (b -> a -> b) -> b -> Aggregator a b
  ```
  does a top-to-bottom fold of the rows.

- ```haskell
  foldRows1 :: (b -> a -> b) -> Aggregator a b
  ```
  does the same as `foldRows` but uses the first row as the initial value.

- ```haskell
  groupRows :: Ord a => Aggregator (a,b) (Map.Map a [b])
  ```
  groups rows of pairs by the first element in the pair.

- ```haskell
  groupRowsWith :: Ord a => (b -> c -> c) -> c -> Aggregator (a,b) (Map.Map a c)
  ```
  the same as `groupRows` but also uses a function to combine the grouped values.

- ```haskell
  groupRowsBy :: Ord b => (a -> b) -> Aggregator a (Map.Map b [a])
  ```
  the same as `groupRows` but the rows can be of arbitrary type. A function is used
  to compute the value by which the grouping is done.

- ```haskell
  groupRowsByWith :: Ord b => (a -> b) -> (a -> c -> c) -> c -> Aggregator a (Map.Map b c)
  ```
  a combination of `groupRowsBy` and `groupRowsWith`.

- ```haskell
   sortRows :: Ord a => Aggregator a [a]
  ```
  the same as `listRows` but also sorts the values

- ```haskell
  sortRowsBy :: (a -> a -> Ordering) -> Aggregator a [a]
  ```
  the same as `sortRows` but uses an ordering function.

#### Nested queries

Both `select` and `query` are parametrized by the result monad:
```haskell
select :: QueryMonad m => Query a -> m [a]
query  :: QueryMonad m => Aggregator a b -> Query a -> m b
```
This allows us to to execute queries both on the top-level as well as nested in another query.

Let's suppose that we also have a table for courses: 
```haskell
data Course
  = Course
      { title       :: String
      , student_ids :: [Key Student]
      }
     deriving Data
      
courses :: Table Course
courses = table "courses"
          `withIndex` course_student
          
course_student :: Index Course (Key Student)
course_student = listIndex courses "student" student_ids
```
then we can join it with the students table:
```haskell
runDaison db ReadMode $ do
  select [(student_id,student,course_id,course)
              | (student_id,student) <- from students everything
              , (course_id, course ) <- fromIndex course_student (at student_id)]
```

The problem with this query is that if a student is enrolled in several courses then its data will be returned several times. The way to avoid this in SQL is to split the query in two queries where the second query will be executed for each row in the first one. Since Daison supports arbitrary algebraic data types, this can be done in a better way with nested queries:
```haskell
runDaison db ReadMode $ do
  select [(student_id,student,courses)
              | (student_id,student) <- from students everything
              , courses <- select (fromIndex course_student (at student_id))]
```
The result from this query will be a sequence of rows where each row will be about one student and will include the list of courses where he/she is enrolled.

### Update

Updates can be made with the `update` primitive:
```haskell
update :: Data a => Table a -> Query (Key a, a) -> Daison [(Key a, a)]
```
It takes a query which returns pairs of key and value and then updates the corresponding rows in a table with those new values. For example:
```haskell
runDaison db ReadWriteMode $ do
  update student
         [(student_id,student{code=2})
              | (student_id,student,_) <- fromIndex students_avg_grade (everything ^>= 4.5)]
```
will change the codes for all students whose average grade is more than or equal to 4.5.

The result from `update` is the list of key/value pairs that were updated. If you don't need that result then you can also use:
```haskell
update_ :: Data a => Table a -> Query (Key a, a) -> Daison ()
```

The update primitives always assume that several rows are changed based on the result from a query. If you only want to change one row then you simply use `return`:
```haskell
update_ students (return (1,Student {name="Nils Holgersson", code=1, grades=[5]}))
```

### Store

The primitive `store` is a combination of `insert` and `update`. Imagine that you edit a document which has to be stored in a database. In SQL, if the document is new then it must be added with `INSERT`, otherwise the old document must be updated with `UPDATE` statement. For that scenario Daison provides the combined primitive:
```haskell
store :: Data a => Table a -> Maybe (Key a) -> a -> Daison (Key a)
```
If it is called with `Nothing` then the value is inserted and the new primary key is returned. Otherwise, when it is called with `Just key` then update happens and the same key is again returned as a result.

### Delete

Deletion is possible with the primitives:
```haskell
delete  :: Data a => Table a -> Query (Key a) -> Daison [Key a]
delete_ :: Data a => Table a -> Query (Key a) -> Daison ()
```
Like with `update` the deletion is based on a query which returns the keys to be deleted. The primitive `delete` also returns the list of deleted keys. If you want to delete only one row, use `return`:
```haskell
delete_ students (return 1)
```

## Foreign Keys

In relational databases, the only way to store complex data structures is to decompose them into tables which are joined with each other. In order to ensure consistencies, the possible joins are described with foreign keys. Daison supports algebraic data types and this means that the need for joined tables is lower but it is still useful. For example, if there are several references to one and the same data, then it is better to store it in one table and then refer to it only by key. For that reason Daison also supports foreign keys.

Here is a modified version of the example with the students and the courses:
```haskell
data Student
  = Student 
     { name   :: String
     , code   :: Int
     , grades :: [(Key Course,Int)]
     }
     deriving Data

data Course
  = Course
      { title :: String
      }
     deriving Data
      
students :: Table Student
students = table "students"
           `withIndex` students_name
           `withIndex` students_course
           `withIndex` students_grade

students_name :: Index Student String
students_name = index students "name" name

students_course :: Index Student (Key Course)
students_course = listIndex students "course" (map fst . grades)

students_grade  :: Index Student Int
students_grade  = listIndex students "grade"  (map snd . grades)

courses :: Table Course
courses = table "courses"
          `withForeignKey` students_course
```
In this version, for each student we store not just a list of grades but also the ids of the corresponding courses. This means that by loading the data for a student, we immediately see in which courses he/she is involved. There is still an index `students_course` which lets us to quickly do the opposite, i.e. find which students are involved in a given course.

The problem is that if someone deletes a course, the data for all involved students will hold a dangling key to a non existent course. This is solved by using the primitive:
```haskell
withForeignKey :: Table a -> Index b (Key a) -> Table a
```
on the `courses` table. This tells Daison that whenever someone deletes a course it must check in the given index whether there is a student refering to that course. If there are reference, an exception will be thrown which will also rolls back the transaction.

Sometimes we don't want to disable the deletion and another way to keep the consistancy is to change the references. For that purpose there are two more primitives:
```haskell
withCascadeDelete :: Data b => Table a -> Index b (Key a) -> Table a
withCascadeUpdate :: Data b => Table a -> (Index b (Key a), Key a -> b -> b) -> Table a
```
If we had used cascade deletion, then, together with the deleted course, Daison will also automatically delete all students who are enrolled in the course. Cascade update on the other hand will update the rows for all enrolled students with the provided function. For instance the function can remove the reference to that course together with the grade.

Neither the cascade delete nor the cascade update scenario are good designs for the students example, but they are useful in other cases. Consider for instance that we store books and chapter titles. Each chapter belongs to a given book, so if we delete a book then it makes sense to delete the associated chapters as well.
