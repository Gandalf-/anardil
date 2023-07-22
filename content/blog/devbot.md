Title: Devbot
Date: 2018-12-26
Category: Programming
Tags: programming, bash, automation
Status: published
Summary: Automate your workflow with Devbot

# Introduction

Some of using computers is inevitably repetitive: checking for updates, backing
up files, maybe clearing some logs. Scripts are a great way to automate some of
the logic around these tasks, but you have to remember to run them or they
don't do much good.

`cron` is a nice tool for running commands on a schedule, but it's opaque and
difficult to debug. Did your command run? When will it run next? Will your
command run or silently fail without your noticing? There isn't a nice way to
copy cron tab entries between machines either.

Devbot is an attempt to make running commands or scripts at particular
intervals more convenient and inspectable.

Given the following information:
1. how often to run
2. what commands to run
3. required preconditions

Devbot runs through all the tasks you've defined, checks if they're ready to be
run, and then runs them. Simple as that. All output produced by the commands
run is logged, so you can see what commands passed or failed. Failed
commands are retried, using a backoff.

```
leaf@home ~> devbot --help
usage: (<devbot> | list | status)

leaf@home ~> devbot status
✓

leaf@home ~> devbot list
update-apt
    wizard update apt
    every 7 days, took 6 seconds, requires network, next in 2 days

dotfiles-fetch
    cd ~/DotFiles
    git fetch
    every 24 hours, took 2 seconds, requires network, next in 5 hours
```

# Configuration

Here's an example `~/.devbot/config.yaml`.

```yaml
events:
  dotfiles-fetch:
    action:
      - cd ~/DotFiles
      - git fetch
    interval: daily
    require: network

  update-apt:
    action: sudo apt update && sudo apt upgrade
    interval: weekly
    require: network

requirements:
  network: nc -w 1 -z 8.8.8.8 53
```

It defines two tasks and a requirement. The requirement command must succeed
before the tasks will be run.  Actions can be a list or single shell command.
Each command requires the previous to succeed before continuing. You could
think of this as:

```python
command_to_run = ' && '.join(commands)
```

All the run time information is persisted to a database, so restarting devbot
doesn't affect when tasks are supposed to be run.

# Usage

Personally, I use devbot on all of my machines - servers, laptops and VMs. They
move data between hosts, backup files, fetch git repositories, update the
system, manage services.

I have `devbot status` plugged into my tmux status line, so I'll know right
away if it's not running. I don't have to worry about crashes unless the system
runs of out of resources - I've had `devbot` running on one host for more than
3 months.

I can easily see the status of a machine and get an idea of how it's been
running with `devbot list`. If some part of the automation is having trouble,
an error count will show up. The timing information gives me an idea of which
commands are doing more work than I expect.

# Implementation

There are two available implementations:

- [Haskell](https://github.com/Gandalf-/Haskell/tree/master/devbot)
- [Bash](https://github.com/Gandalf-/DotFiles/blob/master/lib/devbot.sh)

Both rely on a local key value
[database](https://github.com/Gandalf-/apocrypha) to store run time data. The
Bash implementation is tied into another command line tool,
[wizard](https://github.com/Gandalf-/DotFiles#wizard), while the Haskell
implementation is stand alone.

The main body of devbot is simple. Loop through all the configured tasks.

```haskell
-- https://public.anardil.net/share/code/Haskell/devbot/Devbot/Bot.hs
-- simplified

type State = [Task]

data Task = Task
          { _event   :: Event
          , _process :: Maybe ProcessHandle
          , _start   :: Integer
          }

runBot :: IO ()
runBot = do
        putStrLn "devbot starting up"
        hSetBuffering stdout LineBuffering

        forever $
          events >>= runner . startingState

runner :: State -> IO State
runner state =
        threadDelay $ 1 * second
        mapM handle state >>= runner
    where
        second = 1000000
```

If they're ready to run, run them.

```haskell
handle :: Task -> IO Task
handle task@(Task (Event _ _ d) Nothing _) = do
        -- not currently running
        time <- getTime

        if ready time d
            then run task
            else pure task
    where
        ready :: Integer -> Data -> Bool
        ready now (Data _ _when _) = now > _when
```

If they're already running, see if they're finished. If they're finished, see
if they failed or succeeded.

```haskell
handle task@(Task _ (Just h) _) = do
        code <- getProcessExitCode h
        case code of
            -- still running
            Nothing          -> pure task

            -- finished
            Just ExitSuccess -> success task
            Just _           -> failure task
```

# Conclusion

Devbot is a lightweight alternative to `cron`, with simple configuration and
inspectable run time information. It's features include

- Run arbitrary commands, using familar shell syntax
- Configure frequencies between every second to once a year or more. Devbot or
  system restarts don't affect the next time to run
- Easily inspect previous run duration, errors, and next time to run
- Safety, any command failure will immediately fail the entire task. This is
  the difference between `cd /tmp/dir && rm -rf *` and `cd /tmp/dir; rm -rf *`
