Title: Apocrypha
Date: 2018-02-10
Category: Programming
Tags: programming, database
Status: published
Summary: Lightweight, flexible, persistent database

# The problem

When creating applications, it's handy to be able to store data between runs.
Maybe you have a server that can restart when your laptop comes out of sleep.
Or maybe you have a script that you want to run periodically, so it needs to
check when it ran last. Writing out text files seems like a quick, simple
answer.

Except flat text files are a pain. They work for simple applications where the
information isn't likely to change structure, but are rigid and brittle.
Suppose each line is a record (some information you want to keep track of) and
you want to add another detail; now you now have to rewrite all the parsing in
your application to support the new format. Additionally, there isn't any
metadata to tell you what's what. Are columns important? Lines? Do you have to
look at multiple columns to get a value?

Let's take
[devbot](https://github.com/Gandalf-/DotFiles/blob/master/lib/devbot.sh) as an
example. It runs commands at intervals, similar to cron but with more
information on when it last ran and when it's going to run next. We need to
keep track of:

1. how often to run the command
2. when the command should be run next
3. the command to run

In a flat text file, we might represent this information in the following
format. The first column is the interval, the second column is when next to run
the command, and all following columns are the command.
```
300  1518288629 bash ~/server.sh
1800 1518288629 sudo apt update && sudo apt upgrade
```

Now suppose we want to support two different types of commands. Things we want
to just run (tasks) and things we want to run and save the output (reports).
We'll want to indicate whether a command is a task or report in the
configuration file. How do we do this sanely? Add a column? Then all of our
parsing has to shift; interval = column 2, ..., command = column 4-n.

What if we wanted to add another variable length field to the configuration,
say a `requires` command; something we run to see if we should try to run the
command (eg: check network connectivity before updating). Yikes. We could
switch to CSV (comma separated values) but maybe there's an easier way...

Turns out databases are great at storing data, enforcing structure and allowing
easy access to that data. Great! Let's build a relational schema for devbot.

Or maybe not. Does a 200 line shell script really need a SQL database? Perhaps
not. Something in between would be nice, and JSON is pretty nifty.

# The solution

[Apocrypha](https://github.com/Gandalf-/apocrypha)
is a flexible, JSON based, network attached, key value database I wrote to
support client applications - particularly scripts. It's written in Python3 
and allows arbitrary indexing, something similar key value databases like
Redis don't support. What does that mean?

If we query Apocrypha for `devbot events update-apt`, it translates to the
following internally:

```python
result = database['devbot']['events']['update-apt']
```

which produces the following
```python
{'action': 'sudo apt update && sudo apt upgrade',
 'interval': '1800',
 'require': '!devbot requirements network',
 'type': 'task',
 'when': '1518328360'}
```

Neat! We say we're only interested in when the command is supposed to be run
next, we can query directly for `when` with `devbot events update-apt when`,
resulting in `1518328360`, the Unix time stamp of when next to run the command.

We'll use the following format for future examples; `$` indicates the query and
the line immediately after is the database response (if there would be one).
```python
$ devbot events update-apt when
1518328360
```

# [Database](https://github.com/Gandalf-/apocrypha/blob/master/apocrypha/database.py)

So, what's supported? Dictionaries, lists, strings and any combination of the
previous data types; just like JSON. Lists and dictionaries may be nested
within themselves or each other (eg: dictionaries or dictionaries, dictionaries
of lists, etc). While indexing, if the provided key does not exist, it's
created. If at the end of the operation, the new keys don't have a value,
they're deleted. Requesting a key that doesn't exist in the database isn't an
error. It just returns nothing, since that key doesn't have a value.

The following operations are supported:

| operation                 | example                                        | note                                |
|---------------------------|------------------------------------------------|-------------------------------------|
| *indexing*                | devbot events update-apt interval &nbsp;&nbsp; |                                     |
| *assignment*              | devbot started = yes                           | string assignment                   |
| *assignment*              | colors = red yellow green                      | list assignment                     |
| *deletion*                | colors -d                                      |                                     |
| *list append*&nbsp;&nbsp; | colors + blue                                  | allows multiple values &nbsp;&nbsp; |
| *list remove*             | colors - green                                 | allows multiple values              |
| *show keys*               | colors -k                                      |                                     |
| *search*                  | devbot @ 600                                   | show k, k[x] = 600                  |
| *dump json*               | devbot -e                                      |                                     |
| *set json*                | colors -s "red"                                |                                     |
<br>

Examples are usually more enlightening than tables of commands, so here are
some examples with queries and output.

### Assignment & Indexing
```python
$ animals octopus legs = 8
$ animals octopus legs
8

$ animals chicken legs = 2
$ animals
{'chicken': {'legs': '2'}, 'octopus': {'legs': '8'}}

$ animals --keys
chicken
octopus

$ animals whale
# no output, key does not exist
```

### Lists
```python
$ colors = red 'light green' blue
$ colors
red
light green
blue

$ colors + yellow
$ colors - red 'light green'
$ colors
blue
yellow
```

### Working with JSON
```python
$ animals --set '{"chicken":{"legs":"2"},"octopus":{"legs":"8"}}'
$ animals --keys
chicken
octopus

$ animals octopus --edit
{
    "legs": "8"
}
```

### References

Reducing data duplication is important for consistency and ease of management.
Having the same information scattered around makes it easy for values to fall
out of sync. Apocrypha addresses this by allowing values to be references to
other keys. An example is given above, `devbot events update-apt require` will
evaluate to the value of `devbot requirements network` when queried.

```python
$ fire = hot
$ apple = tasty
$ red examples = fire apple

$ red examples
fire
apple

# take the values of 'red examples' and treat them likes keys
$ red !examples
hot
tasty
```

# [Client](https://github.com/Gandalf-/DotFiles/apocrypha/apocrypha/client.py)

Apocrypha clients are very simple. The length of the message in bytes is sent,
then each element of the query is sent, delimited by newlines. This allows
multiple queries to share the same socket.

- [Apocrypha client in C](https://github.com/Gandalf-/DotFiles/blob/master/bin/d.c)

- [Apocrypha client Python API](https://github.com/Gandalf-/apocrypha/blob/master/apocrypha/client.py)

Here's what using the Python API looks like:
```python
from apocrypha.client import Client
db = Client()

# apocrypha supports lists, strings, and dictionaries
for i in range(0, 100):
    db.append('numbers', value=i)

print(db.get('numbers')[:10])

# nested dictionaries are allowed!
customers = {
    'alice': {
      'age': 30
    },
    'bob' : {
      'age': 20
    }
}

db.set('customers', value=customers)
print(db.keys('customers'))

# query for sub keys with a simple syntax
print(db.get('customers', 'alice', 'age'))

for customer in db.keys('customers'):
    print(db.get('customers', customer, 'age'))
```

# [Server](https://github.com/Gandalf-/apocrypha/blob/master/apocrypha/server.py)

Communication with the database is managed by a network server. The server
handles client connections and translates queries into operations the database
understands. For performance, the server maintains a cache of database
responses and handles invalidating cache values when they're modified by other
database operations.

Key modification times are also kept, so instead of querying the value of a
key, you can ask for the timestamp of it's last change. Any change to a child
key updates the modification time for all parent keys.
```python
# show when devbot events was last modified

$ -t devbot events
1518324872
```

You can also request additional context in replies. This will print out the
keys traversed to get to the result, which is mostly useful during searching.
```python
# show all keys where key[x] = value

$ -c @ 86400
devbot = events = update-pip = interval
devbot = events = update-vim = interval
```

# Conclusion

Apocrypha makes persistently storing data easy. It's simple interface and
client requirements make it easy to integrate into any programming language.
With it, you can focus on the interesting parts of your applications instead of
fighting with flat files, rolling your own lightweight database each time, or
setting up a heavyweight server for an expensive SQL/NoSQL database.
