Title: Hal
Date: 2016-09-23
Category: Programming
Tags: programming, minecraft, article
Status: draft
Summary: An in depth look into Hal, a Minecraft AI

Hal is a artificial intelligence I wrote for the Minecraft server I run. Here I
hope to shed some light onto what he does and how he works. I'm going to run
with the anthropomorphized 'he' even though Hal is really just a bunch of shell
code. If you want to try him out anytime, you can through my online demo
[here](http://hal-demo.anardil.net:48000/).

Let's dive right in!

### What is Hal?

Hal is a text based artificial intelligence intended to make playing Minecraft
on a server easier and more fun. 

### How does he work?

Basically, Hal monitors a log file for changes, grabs those changes, and then
looks for something he can act on. In the context of Minecraft, these are
commands for changing the weather, time of day, moving the player around,
telling jokes, or just chatting. For example:

      <Steve> Hal tell me a joke
      [Hal] Whats Cobblestones favorite music? Rock music.

Hal saw this in the log file, recognized his name and tried to figure out what
to do by looking for additional words of phrases he recognizes. If you have
some familiarity with programming, you've probably encountered string
comparison and regular expressions before. If you're not, don't worry; they're
pretty straight forward. We all have some experience with regular expressions,
even if you didn't know it. When you see something like

      Take the child(ren) to the park

You know that this really means

      Take the child OR the children to the park

If you check out Hal's [source code](https://github.com/Gandalf-/hal), you'll
see he's basically a regular expression machine. Did the player say 'hello'? If
yes, then I reply 'Hey there player!'. Otherwise, keep trying to find a match.
One of Hal's neat features is that he can key off multiple phrases in the same
line. For example

      <Steve> Hey hal, take me home. Oh, and make it day
      [Hal] Hello there Steve!
      [Hal] Sorry Steve, either you never told me where home was or I forgot!
      [Hal] Sunshine on the way!
      /time set day

### How is he organized?

Hal's functionality is broken up into independent modules. For instance, Hal is
capable of remembering things for the players. 

      <Steve> Hal remember that my favourite color is green
      [Hal] Okay Steve, I'll remember!
      <Steve> Hal tell me about my favourite color
      [Hal] Okay Steve, here's what I know about "my favourite color":
      [Hal] "my favourite color is green"

The code that handles this logic is in
[memories.sh](https://github.com/Gandalf-/hal/blob/master/functions/memories.sh).
Likewise, there's seperate modules for
[chatting](https://github.com/Gandalf-/hal/blob/master/functions/chatting.sh),
handling future events through
[intentions](https://github.com/Gandalf-/hal/blob/master/functions/intent.sh),
and
[teleporting](https://github.com/Gandalf-/hal/blob/master/functions/teleport.sh)
the players around the map. When a new line from the player is seen, you can
think of Hal handing the line to each of the modules in turn to see if they
can do anything with it.
