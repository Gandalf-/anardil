Title: Dynamic Shell Scripting
Date: 2018-07-30
Category: Programming
Tags: programming, bash, article, computers
Status: published
Summary: Manipulating functions in Bash for fun and profit

## Summary

We can use `declare`, `eval` and text manipulation tools like `awk` and `sed`
to dynamically redefine shell functions based on their content or name. This
allows us to add lines or generate entirely new functions.


## Introduction

Bash and other shell languages provide syntax for defining functions. Just like
other languages, functions reduce repetition and make understanding the program
easier by breaking things down into component parts.

```shell
greeting() {
  local out="Hello there ${1:-nobody}!"
  echo "$out"
}

greeting "$@"
```
```shell
$ bash greeting.sh Internet
Hello there Internet!
```

## `declare`

Bash provides a couple utilities for working with functions. We can get the
names, or names and content, of all currently defined functions with `declare`.
You can try this in Bash right now:

```shell
$ example() { echo "hello"; }
$ declare -F
declare -f example
```

If you have other functions defined (perhaps from your `.bashrc` or if you run
this in a script), you'll see all the other functions named as well. We can
restrict the output to just a single name with `-p. `With a bit of awk we can
pull out just the interesting part, the name:

```shell
$ declare -F -p example | awk '{print $3}'
example
```

We can also get the full function definition with `-f`:
```shell
$ declare -f -p example
example ()
{
  echo "hello"
}
```

Note that Bash reformatted the function for us. This means that that the output
of `declare -f` will always have the same structure. Try this with a couple
more functions if you're not convinced. This will be important later!

Another useful behavior is that redefining a function isn't an error, and
overwrites the pre-existing definition. We can check this out for ourselves:

```shell
$ apple() { echo 'a'; }
$ apple() { echo 'b'; }
$ declare -f -p apple
apple ()
{
  echo 'b'
}
```

The last piece of the puzzle is `eval`, which takes a string and evaluates it
in the current shell. This is different than using a subshell `$( "..." )`,
because modifications we make to variables and functions persist in the current
shell. Changes made in subshells don't make it back out to the parent shell.
Note that the third definition of `apple` doesn't persist.

```shell
$ apple() { echo 'a'; }                 # 1, in our shell
$ eval "apple() { echo 'b'; }"          # 2, in our shell, with eval
$ bash -c "apple() { echo 'c'; }"       # 3, in a subshell
$ declare -f -p apple
apple ()
{
  echo 'b'      # the 2nd definition
}
```

## Putting it all together

We can use these tools together to redefine functions on the fly. For example,
we could use this to add `echo starting` and `echo ending` to a function. We
could apply this to every function in a longer script to get a better idea of
how it's running. Let's look at an example:

```shell
$ important() {
@>   # complete some task
@>   read -r data
@>   echo "(date) data" >> log.txt
@> }

$ declare -f -p important
important ()
{
    read -r data;
    echo "(date) data" >> log.txt
}
```

The output of `declare -f` is just text, we can manipulate it any way we like
using all the usual tools like `sed` or `awk`. To start, let's add a line to
our `important` function.

```shell
$ declare -f -p important | sed -e 's/^}$/    echo "done";\n}/'
important ()
{
    read -r data;
    echo "(date) data" >> log.txt
    echo "done";        # added by sed
}
```

We can now use `eval` to overwrite the original definition.

```shell
$ eval "$(declare -f -p important | sed -e 's/^}$/    echo "done";\n}/')"
$ declare -f -p important
important ()
{
    read -r data;
    echo "(date) data" >> log.txt;
    echo "done"
}
```

We've dynamically redefined our function. How might this be useful?

- create tools that generate shell code, like
  [autocli.sh](https://github.com/Gandalf-/DotFiles/blob/master/lib/autocli.sh)
- add tracing to all functions in a script
- add cleanup calls to the ends of functions
- whatever else you can think of!

## Tracing example

The `$FUNCNAME` variable tells us the name the current function. We can use
this with the function redefinition above to add tracing to all the functions
defined in a script.

```shell
#!/bin/bash

add-tracing-to-all-functions() {
  while read -r name; do
    eval "$(
      declare -f -p "$name" \
        | sed \
          -e 's/^{ $/{ echo "starting \$FUNCNAME"/' \
          -e 's/^}$/   echo "ending   \$FUNCNAME";}/'
    )"
  done < <(declare -F | awk '{print $3}')
}

apple() {
  echo "apple says $*"
}

blueberry() {
  echo "blueberry says $*"
}

add-tracing-to-all-functions

apple hello             # call apple
echo
blueberry there         # call blueberry
```

```shell
$ bash example.sh
starting apple
apple says hello
ending apple

starting blueberry
blueberry says there
ending blueberry
```
