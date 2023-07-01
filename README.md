A collection of multithreading & memory utilities.

##Pool
A pool of dynamically allocated objects. After acquiring an object from pool, the object is managed by pooled_ptr. 
pooled_ptr is similar to unique_ptr, but returns the object to the pool when the ptr leaves the scope.
pooled_ptr can be converted to shared_ptr. 
The pool is designed to be thread safe.

##Queue
A simple queue with lock.
