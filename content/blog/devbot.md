Title: Devbot
Date: 2018-02-11
Category: Programming
Tags: programming, shell, scripting, automation
Status: draft
Summary: Automate your workflow and background tasks

# Introduction

Some of using computers is inevitably repetitive: checking for updates, backing
up files, maybe clearing some logs. Scripts are a great way to automate some of
the logic around these tasks, but you have to remember to run them or they
don't do much good.

`cron` is a nice tool for running commands on a schedule, but it's opaque and
difficult to debug. Did your command run? Will your command run or silently
fail without your noticing? There isn't a nice way to copy cron tab entries
between machines either.

Devbot is an attempt to make running commands or scripts at particular
intervals more convenient and auditable. Given the following information:

1. interval
2. action
3. requirement
4. when

Devbot runs through all the tasks you've told it about, checks if they're ready
to be run, and then runs them. Simple as that. All output produced by the
commands run is logged, so you can see what commands passed or failed.
