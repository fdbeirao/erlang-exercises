# Exercise 5-1: A Database Server

Write a database server that stores a database in its loop data. You should register the server and access its services through a functional interface. Exported functions in the `my_db.erl` module should include:
```erlang
my_db:start() => ok.
my_db:stop() => ok.
my_db:write(Key, Element) => ok.
my_db:delete(Key) => ok.
my_db:read(Key) => {ok, Element} | {error, instance}.
my_db:match(Element) => [Key1, ..., KeyN].
```

**Hint:** use the `db.erl` module as a backend and use the server skeleton from the echo server from `Exercise 4-1` in Chapter 4. Example:
```
1> my_db:start().
ok
2> my_db:write(foo, bar).
ok
3> my_db:read(baz).
{error, instance}
4> my_db:read(foo).
{ok, bar}
5> my_db:match(bar).
[foo]
```