# Exercise 3-4: Database Handling Using Lists

Write a module `db.erl` that creates a database and is able to store, retrieve, and delete elements in it. The `destroy/1` function will delete the database. Considering that Erlang has garbage collection, you do not need to do anything. Had the `db` module stored everything on file, however, you would delete the file. We are including the `destroy` function to make the interface consistent. You may **not** use the `lists` library module, and you have to implement all the recursive functions yourself.

Hint: use lists and tuples as your main data structures. When testing your program, remember that Erlang variables are single-assignment:

Interface:
```
db:new()                   => Db.
db:destroy(Db)             => ok.
db:write(Key, Element, Db) => NewDb.
db:delete(Key, Db)         => NewDb.
db:read(Key, Db)           => {ok, Element} | {error, instance}.
db:match(Element, Db)      => [Key1, ..., KeyN].
```

Example:
```
1> c(db).
{ok, db}
2> Db = db:new().
[]
3> Db1 = db:write(fabio, rotterdam, Db).
[{fabio, rotterdam}]
4> Db2 = db:write(lelle, stockholm, Db1).
[{lelle, stockholm},{fabio, rotterdam}]
5> db:read(fabio).
{ok,rotterdam}
6> Db3 = db:write(joern, stockholm, Db2).
[{joern, stockholm},{lelle, stockholm},{fabio, rotterdam}]
7> db:read(ola, Db3).
{error,instance}
8> db:match(stockholm, Db3).
[joern,lelle]
9> Db4 = db:delete(lelle, Db3).
[{joern, stockholm},{fabio, rotterdam}]
10> db:match(stockholm, Db4).
[joern]
11>
```