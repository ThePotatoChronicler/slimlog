# Commands

Commands optionally return a single value.
Commands can be separated into four distinct categories:
- Operators
- Basic
- Mlog
- Compound

Syntax of command documentation:
```
command
  subcommand <required argument> [optional argument] [repeating argument...]
```

## Operators

Operators are commands that take one or two arguments, and always return
a value. They correspond directly to the `Op` mlog instruction, although
with a potentially differing name.
```
+ 2 2
< 5 10
=== 4 4
- a b
```

## Basic

Basic commands are the primitives of slimlog, doing what you couldn't do
without otherwise, and generally making your life easier over mlog

### do
Because of how the slimlog parser works, this function is necessary
to chain multiple statements together, and will probably be the start
of every slimlog program.
```
do [statement...]
```

### if
An improvement over using mlog jumps, slimlog has C-like `if`.
elif can be repeated many times.
```
if <condition> <statement>
[elif <condition> <statement>...]
[else <statement>]

```

### while
Our good 'ol friend, the C-like while loop, running until the condition isn't 0
```
while <condition> <statement>
```

for example, printing from 0 to 10 exclusive
```
( do
  (set i 0)
  (while (< i 10) (println i))
  (printflush)
)
```

### _raw
`_raw` passes a string literally, straight to the output code.
```
_raw <code>
```

## Compound

Compound commands make your life easier, they're common utilies or patterns,
that you would otherwise have to use multiple basic commands to do

### iterlinks
Sets up a basic while loop, iterating over all links.
```
iterlinks (
  ...
)
```
is equivalent to
```
(set i 0)
(while (< i @links) (
  (set link (getlink i))
  ...
  (set i (+ i 1))
))
```

### println
```
println value
```
is equivalent to
```
print value
print "\n"
```

### sleep
Waits for `duration` seconds.

```
sleep <duration>
```

## mlog

Mlog commands mimic the behaviour of their mlog counterpart, potentially
improving upon it by leaving out unnecessary arguments, or having
their return argument be instead an anonymous slimlog return, for example
the `sensor` command.

### bind
```
bind <@type>
```

### control
```
control <target>
  enabled <enabled>
  shoot <x> <y> <shoot>
  shootp <unit> <shoot>
  configure <configuration>
  color <r> <g> <b>
```

### draw
```
draw
  clear <r> <g> <b>
  color <r> <g> <b> <a>
  stroke <stroke>
  line <x1> <y1> <x2> <y2>
  rect <x> <y> <width> <height>
  linerect <x> <y> <width> <height>
  poly <x> <y> <sides> <radius> <rotation>
  linepoly <x> <y> <sides> <radius> <rotation>
  triangle <x1> <y1> <x2> <y2> <x3> <y3>
  image <x> <y> <image> <size> <rotation>
```

### drawflush
If no arguments are received, `display` defaults to `display1`
```
drawflush [display]
```

### end
`end`

### getlink
Getlink returns the obtained link, which can be stored in a variable using `set`
```
getlink <link>
```

### noop
```
noop
```

### printflush
If no arguments are received, `message` defaults to `message1`
```
printflush [message]
```

### radar
Returns what was found
`from` defaults to `@this`
`sort` defauls to `distance`
`order` defaults to `1`
Conditions default to `any`.
```
radar [from] [sort] [order] [cond] [cond] [cond]
```

### print
```
print <value>
```

### read
Returns the result

```
read <cell> <at>
```

### set
Returns the `value`, allowing for C-like chaining

```
set <variable> <value>
```
Example of chaining:
```
(set c (set b (set a 10)))
```
Which will result in a, b and c being set to 10

### sensor
Returns the result

```
sensor <target> <sensable>
```

### ucontrol
The `within` has a named return value. It could potentially be
turned into a slimlog return in the future.

```
ucontrol
  idle
  stop
  move <x> <y>
  approach <x> <y> <radius>
  boost <boost>
  pathfind
  target <x> <y> <shoot>
  targetp <unit> <shoot>
  itemdrop <to> <amount>
  itemtake <from> <item> <amount>
  paydrop
  paytake <takeunits>
  mine <x> <y>
  flag <value>
  build <x> <y> <block> <rotation> <config>
  getblock <x> <y> <building_type> <building>
  within <x> <y> <radius> <result>
```

### ulocate
`ulocate` is full of named returns, because most instructions right now
only return a single value.
`ulocate` returns the common `found` result, to be able to use it
as a condition to `if`, `while`, etc.
```
ulocate <outx> <outy>
  ore <ore>
  spawn <spawn>
  damaged <damaged>
  building <group> <enemy> <building>
```

### uradar
Returns what was found
`sort` defauls to `distance`
`order` defaults to `1`
Conditions default to `any`.
```
uradar [sort] [order] [cond] [cond] [cond]
```

### write
Write returns the `value`, allowing for chaining
```
write <to> <at> <value>
```
