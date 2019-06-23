Title: Practical Coprocesses
Date: 2017-11-13
Category: Programming
Tags: programming, bash, article, computers
Status: published
Summary: A practical example using Bash coprocesses

## Introduction

Coprocesses expose a way to run a subshell asychronously, while maintaining
full communication through pipes. In a typical script, you can run commands in
the background (asychronously) with the `&` operator. This allows tasks to be
worked in parallel.

```shell
#!/bin/env bash

for url in Linux Linux_kernel Free_software; do
  wget https://en.wikipedia.org/wiki/"$url" &
done

wait
echo "All done!"
```

The problem here is that once the child processes fork, you don't have any way
to communicate with them. Why would we want to do this? Maybe you don't want
`wget` to write to files, but pass the results back up to the parent process so
you can search for links; maybe you just wanted to see if those pages are
available.

We could do this with FIFOs, but it would be a bit of a mess, and we're still
missing out on sending information **to** the child processes.

```shell
#!/bin/env bash

mkfifo child2parent

for url in Linux Linux_kernel Free_software; do
  wget https://en.wikipedia.org/wiki/"$url" >> child2parent &
done

wait
cat child2parent

echo "All done!"
```

## Coprocesses

```shell
coproc subproc {
  python -u -c '
import time

while True:
    try:
        line = raw_input()
        exec(line)
    except EOFError:
        time.sleep(0.1)'
}

py-run() {
  echo "$@" >&${subproc[1]}
}

py-read() {
  IFS= read -ru ${subproc[0]} $1
}
```

What's going on here? We're creating a Python interpreter in the background,
but maintaining full communication with it. The interpreter's stdin is linked
to `${subproc[1]}`, which allows `py-run()` to work. We send a line to the
child process, it reads it from it's stdin, executes it, and waits for another
command.

We get results out of the child process through it's stdout `${subproc[1]}`.
`py-read` takes a line of output from the interpreter and reads it into a
variable you provide. Here's some example usage:

```Shell
py-print() {
  local x; py-read x; echo $x
}

py-run "x = 'hello'"
py-run "print x[::-1]"
py-read result; echo $result

py-run "y = { 'apple' : '$result', '$(whoami)' : '$(uname)', } "
py-run "print y.items()"

py-print
```

With `coproc` and a snippet of Python, we can interleave Bash and Python code
that persists between calls. Between calls to `py-run` and `py-read`, the
Python interpreter remains running, so all of the variables we've defined and
the changes already made persist.

```
$ bash pybash.sh                                               
olleh                  
[('apple', 'olleh'), ('chronos', 'Linux')] 
```
