Title: AutoCLI
Date: 2018-07-15
Category: Programming
Tags: programming, scripting
Status: published
Summary: Manage large numbers of scripts with generated menus

# Summary
AutoCLI simplifies managing scripts by auto generating option menus, all in bash, without any dependencies. Sub menus, customization, and positional arguments are all supported. See [autocli.sh](https://github.com/Gandalf-/DotFiles/blob/master/lib/autocli.sh) on GitHub for the source and more examples.

# The problem
One of the best parts of using the command line is that you can automate your workflow with scripts. Commonly repeated commands can be turned into aliases and commonly repeated logical steps can be turned into scripts. However, at some point, it becomes difficult to manage a lot of aliases and scripts. 

- How are they named? 

- What have you already made?

- Where are they stored if you want to make changes?

Another problem, particularly with aliases, is portability. I prefer to write scripts in `bash`, but use `fish` as my primary shell, and `zsh` fairly often at work. Aliases written in bash may work with zsh, but there are some gotchas, and they're completely incompatible with fish. If I'm on a system that doesn't have fish, and all my scripts are written in fish, I'm out of luck.

# A partial solution
You can combine the logic and actions of several of your scripts together in to a "super script". Usually, this is wrapped with some kind of case statement that looks through the input arguments, determines what you're trying to do, then calls the appropriate sub script (function). A common, simple pattern: 
```shell
case $1 in
	s)
		git status
		;;
	a)
		git add .
		;;
	*)
		git --help
		;;
esac
```

A more advanced pattern, that passes arguments along to the sub scripts.
```shell
case $1 in
	something)
		shift	# remove "something"
		do_something "$@"
		;;
	another_thing)
		shift	# remove "another_thing"
		do_another_thing "$@"
		;;
	*)
		echo "Don't know what to do!"
		exit 1
		;;
esac
```
Neither of these examples is doing anything out of the ordinary. Unfortunately, managing these menus quickly becomes difficult: 

- Are you passing all the arguments along correctly? 
- Help text? How do you know what options are available?
- What if you want to use flags or positional arguments? `cook --with carrots cake`?
- What about sub menus? `cook dinner spaghetti -h`

# AutoCLI
AutoCLI is a script that, when sourced and ran, reads in all the functions that have been defined and generates intermediary menus to connect them. Menus show where sub menus are and include help text by default. Functions are grouped into *commands* and *library functions*. All commands start with the name of the output script, followed by an underscore and a name. For example:

- `wizard_update`

- `wizard_devbot_start`

- `wizard_devbot_stop`

These functions are defined in your source, they do the interesting thing you're trying to accomplish. AutoCLI will generate the following functions:

- `wizard`

- `wizard_devbot`

When we call the output script, we get a menu

```
leaf@home ~> ./wizard

wizard
  
  update
  devbot ...
```

And sub menu for the `devbot` commands
```
leaf@home ~> ./wizard devbot

wizard devbot
  
  start
  stop
```

Library functions are denoted with `::`, for example `devbot::setup`. This tells AutoCLI that the given function is for internal use only, and shouldn't be exposed as a command in a menu.

For very large scripts composed of multiple files, you can define the `sources` array variable with the paths to the files containing your commands and library functions. See [auto_wizard](https://github.com/Gandalf-/DotFiles/blob/master/bin/auto_wizard) on GitHub for an example. 

The output from AutoCLI is always a single Bash script; this makes it easy to copy to other machines and to install (one position independent file in your $PATH). To run, AutoCLI requires Bash 4.0, but the output should run on any version.

To make calling commands easier, AutoCLI does pattern matching on the input you provide to match it to a command. In many cases, this allows you to specify a long list of sub commands with a single letter. This are all equivalent:

- `wizard create file python`

- `wizard crea fi py`

- `wizard c f p`

# Customization
AutoCLI exposes a couple variables you can use to customize the generated functions.

- `meta_head` inject additional statements between the help text and the case statements. This is useful for defining variables used by sub commands.
```shell
meta_head[wizard_devbot]='

local where="$(hostname)"
local who="$(whoami)"
local force=0
'
```

- `meta_body` inject additional case statements into the menu. This is useful for flags or positional arguments.
```shell
meta_body[wizard_devbot]='

--force) force=1 ;;
'
```

# Appendix: Full example

## Input file
```shell
source autocli.sh

devbot::setup() {
  echo "setup devbot"
}

wizard_update() {
  sudo apt update
  sudo apt upgrade
}

wizard_devbot_start() {
  devbot::setup
  echo "starting devbot"
}

wizard_devbot_stop() {
  echo "stop devbot"
}

meta_head[wizard]='
local where="$(hostname)"
local force=0
'

meta_body[wizard]='
--force) force=1 ;;
'

# create the output script, named "wizard", 
# in this directory
autocli::create wizard .
```

## Output file
```
# this is an auto generated file. do not edit manually
{

devbot::setup ()
{
    echo "setup devbot"
}
wizard ()
{
    [[ -n $1 ]] || "${FUNCNAME[0]}" --help;
    local __shifts=0 __usage="
  devbot ...
  update" __name="wizard";
    local where="$(hostname)";
    local force=0;
    while [[ -n $1 ]]; do
        case $1 in
            --force)
                force=1
            ;;
            "d" | "de" | "dev" | "devb" | "devbo" | "devbot")
                wizard_devbot "${@:2}"
            ;;
            "u" | "up" | "upd" | "upda" | "updat" | "update")
                wizard_update "${@:2}"
            ;;
            __list)
                echo devbot update
            ;;
            *)
                if [[ -n $usage ]]; then
                    echo "$usage";
                    exit 0;
                else
                    printf '\n%s\n%s\n\n' "$__name" "$__usage";
                    exit 0;
                fi
            ;;
        esac;
        local __ret=$?;
        shift;
        shift $__ret;
        (( __shifts += __ret + 1 ));
    done;
    return $__shifts
    return $#;
}
wizard_devbot ()
{
    [[ -n $1 ]] || "${FUNCNAME[0]}" --help;
    local __shifts=0 __usage="
  start
  stop" __name="wizard devbot";
    while [[ -n $1 ]]; do
        case $1 in
            "s" | "st" | "sta" | "star" | "start")
                wizard_devbot_start "${@:2}"
            ;;
            "s" | "st" | "sto" | "stop")
                wizard_devbot_stop "${@:2}"
            ;;
            __list)
                echo start stop
            ;;
            *)
                if [[ -n $usage ]]; then
                    echo "$usage";
                    exit 0;
                else
                    printf '\n%s\n%s\n\n' "$__name" "$__usage";
                    exit 0;
                fi
            ;;
        esac;
        local __ret=$?;
        shift;
        shift $__ret;
        (( __shifts += __ret + 1 ));
    done;
    return $__shifts
    return $#;
}
wizard_devbot_start ()
{
    devbot::setup
    echo "starting devbot"
    return $#;
}
wizard_devbot_stop ()
{
    echo "stop devbot"
    return $#;
}
wizard_update ()
{
    sudo apt update;
    sudo apt upgrade
    return $#;
}

[[ "${BASH_SOURCE[0]}" == "${0}" ]] && wizard "$@"
true
} || exit

```
