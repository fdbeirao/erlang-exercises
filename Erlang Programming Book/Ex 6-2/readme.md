# Exercise 6-2: A Reliable Mutex Semaphore

Suppose that the mutex semaphore from the section "Finite State Machines" on page 126 in Chapter 5 is unreliable. What happens if a process that currently holds the semaphore terminates prior to releasing it? Or what happens if a process waiting to execute is terminated due to an exit signal? By trapping exits and linking to the process that currently holds the semaphore, make you mutex semaphore reliable.

In your first version of this exercise, use `try...catch` when calling `link(Pid)`. You have to wrap it in a `catch` just in case the process denoted by `Pid` has terminated before you handle its request.

In a second version of the exercise, use `erlang:monitor(type, Item)`. Compare and contrast the two solutions. Which one of them do you prefer?