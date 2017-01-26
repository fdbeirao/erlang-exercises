# Step 2: handling simple commands

Start the `mire_server` on a specific port, connect to this port with your favourite telnet client and
enjoy the fun.

### Make command (Windows)

```
rmdir /S /Q ebin & mkdir ebin & werl -eval "make:all([load])"
```

### Start & stop command

```
1> mire_server:start(3333).
{ok,<0.67.0>}
2> mire_server:stop().
ok
```

### Features

* Once a client connects, his connection remains active and valid even if the echo_handler is stopped;
* The user now gets a prompt before his command;