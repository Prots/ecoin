#Script

The bitcoin script language is a stack based language where the stack items are
binaries but may be interpreted also as variable length integers or booleans.

Scripts are parsed and internally represented by a somewhat smaller subset of the whole
scripting language. The parser also checks so that the structure of control flow statements
are correct. 

A script is a list of operations. If/nif operations takes two separate scripts
as inputs. A script is valid if it terminates without failure and the top stack
item is true. 

##Data types
Stack items are binaries

##Operations

###Primary stack-based

####{push, Item :: (-1)..16 | boolean() | binary()}

Push an item onto the stack.

The binary may be as large as 2^32-1 bytes but the actual limit will be that of
the maximum limit of the script length which is 520 bytes.

####verify

Check if the top value is true, if it is pop the top else crash.

####ifdup

Duplicate the top value if it is true else do nothing.

####depth

Push the size of the stack onto the stack.

####{drop, N :: 1..2}

Drop N items from the stack.

####{dup, N :: 1..3}

Duplicate the N top items of the stack.

####nip

Remove the item under the top.

####{over, N :: 1..2}

Copies N items N items back to the top.

####pick

Pop an integer N and copy the item N items back to the top.

####roll

Pop an integer N and move the item N items back to the top.

####{rot, N :: 1..2}

The top N\*3 items are rotated downwards N steps wrapping around to
the top.

####{swap, N :: 1..2}

Swaps the top N items with the N items under them.

####tuck

The top item is copied and inserted under the second item.

####cat *Disabled*

Concatenates the second item with the top item and push the result.

####substr *Disabled*

Pop a size S, a position P and a subject binary B. Push the sub binary S bytes
big starting at position P in B.

####left *Disabled*

Pop an index Ix and a subject binary B. Push the sub binary to the left if Ix
in B.

####right *Disabled*

Pop an index Ix and a subject binary B. Push the sub binary to the right of Ix
in B.

####size

Push the size of the top item.

####invert *Disabled*

Bitwise invert the top item.

####bitwise_and *Disabled*

Bitwise and between the top pair of items.

####bitwise_or

Bitwise or between the top pair of items.

####bitwise_xor

Bitwise xor between the top pair of items.

####equal

Binary equality of the top pair of items.

####add1

Increment the top item.

####sub1

Decrement the top item.

####mul2 *Disabled*

Double the top item.

####div2 *Disabled*

Half the top item.

####negate

Flip sign of top item.

