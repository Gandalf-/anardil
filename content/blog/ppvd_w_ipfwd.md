Title: Probabilistic Policy Violation Detection with IPFWD
Date: 2017-04-04
Category: Research
Tags: article, research, firewalls, security
Status: published

## Abstract

When using a firewall system like IPFW to detect
threats, the system can end up doing a lot of packet processing.
This can negatively impact performance-sensitive systems such as
storage nodes in data centers. This paper describes a practical
solution to this problem using a load-weighted probabilistic
mechanism that allows a trade-off between perfect visibility of
network packets and reduced impact to system load.


## Introduction

Firewalls shield networks and hosts from malicious traffic
by blocking network packets that do not match any of the
rules defined in a security policy. In high performance and
network throughput environments, extensive packet processing
by firewalls can limit the system’s ability to utilize the full
capacity of network links. This problem is typically addressed
by restricting the number and complexity of firewall rules,
disabling the firewall entirely, or sacrificing the additional
throughput so all the packet processing can be completed.

Our work uses a form of packet sampling to provide a
trade-off between complete application of a security policy and
increased performance. Other papers on packet sampling[1–
3] have been focused on detecting and classifying threats,
not performance enhancement. However, their approaches to
packet sampling and the implications addressed provided the
inspiration for the work done here. The BSD community[4, 5]
has produced several in depth investigations into network and
firewall performance.


## Full Paper

<iframe src="https://docs.google.com/gview?url=https://public.anardil.net/Projects/PPVDwIPFWD.pdf&embedded=true" style="width:100%; height:700px;" frameborder="0"></iframe>
