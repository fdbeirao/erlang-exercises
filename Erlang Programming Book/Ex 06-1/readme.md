# Exercise 6-1: The Linked Ping Pong Server

Modify processes A and B from `Exercise 4-1` in Chapter 4 by linking the processes to each other. When the `stop` function is called, instead of sending the `stop` message, make the first process terminate abnormally. This should result in the `EXIT` signal propagating to the other process, causing it to terminate as well.