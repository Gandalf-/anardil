Title: Stream Programming
Date: 2018-09-09
Category: Programming
Tags: programming, bash, article, computers
Status: published
Summary: Defining data in terms of streams to simplify programming

# Introduction

Most simple iterative programming tasks can be rephrased in terms of a 'map' or
'reduce' operation. The advantage here is that if we can work our problem into
a map - reduce problem, we can easily increase parallelism.

# A simple example

Let's check a couple Wikipedia pages for mentions of 'Unix'. The most straight
forward, imperative way, to do this is something along these lines:

```shell
for url in Linux ANSI_escape_code Shell; do

  page="$( curl -s "https://en.wikipedia.org/wiki/$url" )"

  if [[ $page =~ Unix ]]; then
    echo "$url mentions Unix!"
  fi
done
```

If you're familiar with shell scripts, you may already see some immediate
improvements we could make. If all we care about is whether the page contains
'Unix', we could just pipe the `curl` output to `grep`.

```shell
for url in Linux ANSI_escape_code Shell; do

  if curl -s "https://en.wikipedia.org/wiki/$url" | grep -q 'Unix'; then
    echo "$url mentions Unix!"
  fi
done
```

With this approach, it's not even necessary to download the rest of the article
if we encounter 'Unix' early. `grep` will return success and stop reading the
input from `curl`, so then `curl` will stop downloading any more of the
article. If we allow error reporting from `curl`, we can see this is the case:

```
$ curl -sS "https://en.wikipedia.org/wiki/Linux" | grep -q Unix && echo 'Found it!'
curl: (23) Failed writing body (161 != 1300)
Found it!
```

We can hit this even sooner with the `-N` 'no buffering' option.
```
$ curl -sSN "https://en.wikipedia.org/wiki/Linux" | grep -q Unix && echo 'Found it!'
curl: (23) Failed writing body (0 != 397)
Found it!
```

# Pipelines

This works for a single article at a time, but what about all of them?

It would be nice to break this down to a single Pipeline, and we can with
`xargs`. `echo` allows multi line strings, `xargs` expects separate inputs to
come on separate lines, so we can use them together to pass an arbitrary list
of elements.

```
$ echo 'Linux
Shell
ANSI_escape_code' | xargs -i echo curl "https://en.wikipedia.org/wiki/{}"
curl https://en.wikipedia.org/wiki/Linux
curl https://en.wikipedia.org/wiki/Shell
curl https://en.wikipedia.org/wiki/ANSI_escape_code
```

Let's clean this up and search for mentions of 'Unix' again.

```shell
list() {
  for item in "$@"; do
    echo "$item"
  done
}

map() {
  xargs -i "$@"
}

list Linux ANSI_escape_code Shell \
  | map curl -s "https://en.wikipedia.org/wiki/{}" \
  | grep -q 'Unix' \
  && echo 'Found it!'
```

This looks nice, and is completing the same task as everything above. Helper
functions aside, this solution is half as many lines as the original solution,
and gives us the option of early stopping. It also gives us the option to
parallelize - all of the articles could be downloaded concurrently since `grep`
can easily handle more input at once.

# A more interesting example

Let's try something more interesting. On these three pages, what are the most
common words around mentions of 'Unix'? The steps we need to take are:

1. Download the files
2. Pick out the letters around mentions of 'Unix', using space as a delimiter
3. Remove duplicates and count instances
4. Sort on the number of instances
5. Grab the top 5

```shell
list Linux ANSI_escape_code Shell \
  | map curl -s "https://en.wikipedia.org/wiki/{}" \
  | grep -o '[^ ]\+Unix[^ ]\+' \
  | sort | uniq -c \
  | sort -r -n -k 1 \
  | head -n 5
```
```
$ bash example.sh
      6 href="/wiki/Unix-like"
      5 title="Unix-like">Unix-like</a>
      4 href="/wiki/Unix"
      3 title="Unix">Unix</a>
      2 href="/wiki/File:Unix_timeline.en.svg"
```

How about the most linked articles on the Linux page?

- Download the article

```shell
download-article() {
  curl -s "https://en.wikipedia.org/wiki/"$1"
}
```

- Grab references to other Wikipedia pages. Clean them up.

```shell
page-to-links() {
  # page -> href=/wiki/Slackware, href=/wiki/Austrumi_Linux

  grep -o 'href="/wiki/[^"]\+"' \
    | tr -d '"'
}
```

- Break up the URL, take the last element, which is the name

```shell
links-to-articles() {
  # href=/wiki/Slackware -> Slackware

  tr '/' ' ' \
    | awk '{print $NF}'
}
```

- Get the top 'n' unique instances

```shell
top-unique() {
  sort | uniq -c \
    | sort -r -n -k 1 \
    | head -n "$1"
}
```

Assemble the pieces, and we have an answer!

```shell
download-article "Linux" \
  | page-to-links \
  | links-to-articles \
  | top-unique 15
```
```
$ bash example.sh
     13 Linux_kernel
     10 Android_(operating_system)
      9 Free_software
      8 Ubuntu_(operating_system)
      8 Linux_distribution
      7 Linux
      7 GNU
      7 Debian
      7 C_(programming_language)
      6 Linus_Torvalds
      6 IBM
      6 Chrome_OS
      5 Wayland_(display_server_protocol)
      5 Unix-like
      5 Smartphone
```

# Parallelism

Let's write our own version `xargs` with parallelism built in. We can either
let each subprocess write to `stdout` whenever it has a result, which will get
us results more quickly and without blocking, but will also interleave results
from other processes.

Alternatively, we can redirect the output of each subprocess somewhere else,
wait for them all to end, then write the results to `stdout` in order. Either
way, writing our own `xargs` function in our shell lets us 'map' our own
functions. `xargs` can't use shell functions.

```shell
faster-map() {

  while read -r line; do
    "$@" "$line" &
  done

  wait
}

strict-map() {

  output=()

  while read -r line; do
    tmp="$( mktemp /dev/shm/strict-map.XXXXXX )"
    output+=( "$tmp" )

    # could keep separate files for stdout + stderr if we wanted
    # but not necessary here
    "$@" "$line" >"$tmp" 2>&1 &
  done

  wait

  for item in "${output[@]}"; do
    cat "$item"
    rm -f "$item"
  done
}
```

`faster-map` should be slightly faster than `strict-map`, but both will be much
faster than running each command + argument pair serially like `xargs` does.

# Summary

Treating input data as a list of items and then progressively filtering or
transforming the data atomically is a powerful way to describe a problem. It
allows for easy extension or refinement, and parallelism.
