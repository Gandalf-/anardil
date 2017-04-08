Title: Hal
Date: 2017-04-02
Category: Programming
Tags: programming, minecraft, article
Status: published
Summary: An in depth look into Hal, a Minecraft AI

## Introduction

Hal is a artificial intelligence I wrote for the Minecraft server I run. Here I
hope to shed some light onto what he does and how he works. I'm going to run
with the anthropomorphized 'he' even though Hal is really just a bunch of shell
code. If you want to try him out anytime, you can through my online demo
[here](http://hal.anardil.net:2095/).

**Let's dive right in!**

## What is Hal?

Hal is a text based artificial intelligence intended to make playing Minecraft
on a server easier and more fun. 

### How does he work?

Whenever a player types something in chat or certain events happen, some text
is written to a log file. Basically, Hal monitors this log file for changes,
grabs those changes, and then looks for something he can act on. In the context
of Minecraft, these are commands for changing the weather, time of day, moving
the player around, telling jokes, or just chatting. For example:

```
      <Steve> Hal tell me a joke
      [Hal] Whats Cobblestones favorite music? Rock music.
```

Hal saw "<Steve\> Hal tell me a joke" in the log file, recognized his name and
tried to figure out what to do by looking for additional words of phrases he
recognizes. If you have some familiarity with programming, you've probably
encountered string comparison and regular expressions before. If you're not,
don't worry; they're pretty straight forward. We all have some experience with
regular expressions, even if you didn't know it. When you see something like

```
      Take the child(ren) to the park
```

You know that this really means

```
      Take the child OR the children to the park
```

If you check out Hal's [source code](https://github.com/Gandalf-/hal), you'll
see he's basically a regular expression machine. Did the player say 'hello'? If
yes, then I reply 'Hey there player!'. Otherwise, keep trying to find a match.
One of Hal's neat features is that he can key off multiple phrases in the same
line. For example

```
      <Steve> Hey hal, take me home. Oh, and make it day
      [Hal] Hello there Steve!
      [Hal] Sorry Steve, either you never told me where home was or I forgot!
      [Hal] Sunshine on the way!
      /time set day
```

### What can he do?
- Hold a basic conversation
```
  Hey Hal, how are you?
  Howdy Hal, tell me a joke
  whatever Hal
```

- Change weather or time of day
```
  Hal make it sunny and make it daytime
```

- Change player gamemode
```
  Hal put me in creative mode
```

- Apply status effects to players
```
  Hal make me healthy
  Hal make me invisible
```

- Send messages to other players, and save them for the next time they log in
  if they're not currently in game
```
  Hal tell Notch to add more features
```

- Teleport the player to other players, locations specified in a configuration
  file or a user specified home
```
  Hal take me to the castle
  Hal take me to Steve
  Hal set home as 465 132 798
  take me home Hal
```

- Remember, recall and forget arbitrary information
```
  Hal remember my favorite color is blue
  tell me about my favorite color Hal
  Hal forget about my favorite color
```

- Answer math problems
```
  Hal what's 5 * 5 + 36 ^ 1.27
```

- Answer questions about various topics
```
  tell me about math Hal
```

### How is he organized?

Hal's functionality is broken up into independent modules. For instance, Hal is
capable of remembering things for the players. 

```
      <Steve> Hal remember that my favourite color is green
      [Hal] Okay Steve, I'll remember!
      <Steve> Hal tell me about my favourite color
      [Hal] Okay Steve, here's what I know about "my favourite color":
      [Hal] "my favourite color is green"
```

The code that handles this logic is in
[memories.sh](https://github.com/Gandalf-/hal/blob/master/modules/memories.sh).
Likewise, there's seperate modules for
[chatting](https://github.com/Gandalf-/hal/blob/master/modules/chatting.sh),
handling future events through
[intentions](https://github.com/Gandalf-/hal/blob/master/modules/intent.sh),
and
[teleporting](https://github.com/Gandalf-/hal/blob/master/modules/teleport.sh)
the players around the map. When a new line from the player is seen, you can
think of Hal handing the line to each of the modules in turn to see if they can
do anything with it.


## Bash as a programming language

Hal is written entirely in Bash, a Unix (and now Windows?) scripting language.
This might seem like an odd choice, and in many ways it is, but it has afforded
a couple advantages over other programming languages.

For one, regular expressions are very easy to work with. In Bash, you have
direct access to `grep`, `sed` and friends. Hal is essentially a regular
expression matching engine, so easy access to and simple syntax for expressions
help quite a bit. 

Other advantages include platform independent code, easy access to file system
objects, and small size. Hal requires no libraries, has no compile time, and
will run on any Linux system with Bash v4 or higher. 

Most of Hal's matching is done in `case` statements, which aren't true regular
expressions, but "glob" matching. 

There are a number of common complaints about shell code; hard to read,
difficult to test, and error prone. These may be valid points, but there are
certainly steps that can be taken to address them. 

- Coding standards exist for shell. Following them makes your code safer and
  easier to read

- Unit testing is a breeze if you adopt a functional programming paradigm.
  Moving related functions into modules and ensuring that you have unit tests
  for each function makes identifying regressions and edge cases possible.

- There are a number of security features builtin to Bash that we can utilize.
  The `set` command contains a number of restricting options, forcing you to
  adopt more safe programming practices. Use of the `local`, `readonly` and
  `export` builtins allow more control over variables.

- A lot of problems can be avoided simply by correct programming practices.
  Minimize side effects in functions, have clear control flow, and validate all
  inputs, whether they come from the user or the system.

Snippet from `modules/utility.sh`
```shell
  69 player_left(){
  70   # : ' none -> none
  71   # Say goodbye, comment on player count
  72   # '
  73   say "Goodbye ${USER}! See you again soon I hope!"
  74   let NUM_PLAYERS--
  75
  76   if (( NUM_PLAYERS < 0 )); then
  77     say "I seem to have gotten confused..."
  78     NUM_PLAYERS=0
  79
  80   elif (( NUM_PLAYERS == 0 )); then
  81     say "All alone..."
  82     QUIET=0
  83
  84   elif (( NUM_PLAYERS == 1 )); then
  85     say "I guess it's just you and me now!"
  86   fi
  87
  88   ran_command
  89 }
```


## The web demo

The demo `demo/web_server.sh` is one of the most difficult parts of Hal to
understand. It's an HTTP web server in Bash for one, but also handles the task
of allowing clients to communicate with Hal through HTTP POST.

This snippet from `demo/web_server.sh:main()` shows the key functionality that
allow the web server to work. 

- Line 162 starts Hal in debug mode, Hal is only communicated with through
  `HAL_INPUT_FILE` and `HAL_OUTPUT_FILE`, which represent `latest.log` and
  `stdout` when Hal is working off of a real Minecraft server.

- Lines 165 to 167 is how the demo communicates with clients. `nc` really
  wasn't designed for this kind of set up, and has a terrible habit of closing
  unexpectedly. We can get around this with `while true; do ... done`. Another
  problem is that input redirects, e.g. `<` may block. We solve this by using
  process substitution and `cat`. `cat` is always trying to read from the
  `server2client` fifo and does not block. Lastly, what the client sends the
  server is redirected to the `client2server` fifo, which is read from in other
  places.

- Getting these 3 lines right took just about as long as the rest of the
  web_server combined. Tricky!

Snippet from `/demo/web_server.sh`
```shell
 157   touch ${HAL_OUTPUT_FILE} ${HAL_INPUT_FILE}
 158   mkfifo ${server2client} ${client2server}
 159
 160   # start hal
 161   echo "Starting hal..."
 162   bash ../hal.sh ${HAL_INPUT_FILE} ../ ${ROOT_DIR} ${HAL_OUTPUT_FILE} &
 163   readonly HAL_PID=$!
 164
 165   while true; do
 166     nc -l -p ${PORT} < <(cat "${server2client}") > "${client2server}"
 167   done &
 168
 169   # start http server
 170   echo "Starting server..."
```

The complete flow of information from client to hal looks something like this.
```
  Client Browser -> HTTP POST -> web_server.sh -> File System -> Hal
```

And the responses (if Hal decides to respond, look like this
```
  Hal -> File System -> web_server.sh -> HTTP Reply -> Client Browser
```


## Advanced features

Hal would still be neat if all he did was give preset replies to Minecraft
chat, but there's more than that built in.

### Function callbacks and intentions

Although not used much currently, Hal supports registering callback functions
that will be triggered when a regular expression is matched on the input line. 

For example, the following code in `modules/chatting.sh`, allows Hal to ask for
confirmation after the user says something like `Hal be quiet`. Once the
callback is set, Hal will look for `yes` or `sure` in the next 3 lines that
appear. If a match is made, the `intent_be_quiet` function will be called.
Otherwise, Hal will forget and move on.

```shell
  16   case "$CLINE" in
  17     *'be quiet'*)
  18       say 'Oh... Are you sure?'
  19       set_intent 'yes|sure' 'intent_be_quiet'
  20       ran_command
  21       ;;
```

This same functionality allows richer interaction with the Minecraft server as
well. For instance, when teleporting a player to another player, Hal set's an
intention on the error message the server returns when the player doesn't
exist. This way, Hal can tell the player what happened.

Hal can also recognize and answer math questions, and query Wikipedia for
answers if the user asks him about a topic he doesn't know about. 


### ([vV]ery )\*Fuzzy matching

Hal is very permissive in how inputs are matched to expressions. Even something
simple like `what's up`  can be tricky to match correctly due to the number of
possible variations in user input.

- `whats up`
- `what's up`
- `what is up`
- `what up`

And `Hal` has to be in there somewhere too, either before `whats up` or after
it in each of those cases. What if there's more text after `whats up`, or
before? What if there isn't? It turns out Bash `case` statements and globs do a
great job here. The order of the `case` statements allow us to describe
matching priorities, and the globs handle the uncertainty. As an added bonus,
glob matching is faster than regular expression matching!

For example, consider the following snippet from `modules/chatting.sh`

```shell
  39     *'tell '*' about everything'*)
  40       recall_everything
  41       ;;
  42     *'tell '*' about '*)
  43       recall_phrase
  44       ;;
  45     *'tell '*)
  46       tell_player
  47       ;;
```

This produces the following desired outcomes

- `Hal, tell me about everything`    -> list all memories for current player
- `tell me about math Hal`           -> look through player memories for
  'math'. if it doesn't occur anywhere, search Wikipedia for 'math'
- `Hal tell Steve to come over here` -> send message "come over here" to player
  Steve

**And that's the basic gist!**
