# Step 1: a simple echo server

Start the `echo_handler` on a specific port, connect to this port with your favourite telnet client and have the most boring conversation with yourself.

### Make command (Windows)

```
rmdir /S /Q ebin & mkdir ebin & werl -eval "make:all([load])"
```

### Start & stop command

```
1> echo_handler:start(3333).
{ok,<0.67.0>}
2> echo_handler:stop().
ok
```

### Features

* Once a client connects, his connection remains active and valid even if the echo_handler is stopped.