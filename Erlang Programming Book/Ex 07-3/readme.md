# Exercise 7-3: The db.erl Exercise Revisited

Revisit the database example `db.erl` that you wrote in `Exercise 3-4` in Chapter 3. Rewrite it using records instead of tuples. As a record, you could use the following definition:
```
-record(data, {key, data}).
```

You should remember to place this definition in an include file. Test your results using the database server developed in `Exercise 5-1` in Chapter 5.