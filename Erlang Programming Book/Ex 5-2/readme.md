# Exercise 5-2: Changing the Frequency Server

Using the frequency server example in this chapter, change the code to ensure that only the client who allocated a frequency is allowed to deallocate it. Make sure that deallocating a frequency that has not been allocated does not make the server crash.

**Hint:** use the self() BIF in the `allocate` and `deallocate` functions called by the client.

Extend the frequency server so that it can be stopped only if no frequencies are allocated.

Finally, test your changes to see whether they still allow individual clients to allocate more than one frequency at a time. This was previously possible by calling `allocate_frequency/0` more than once. Limit the number of frequencies a client can allocate to three.