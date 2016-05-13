Title: Elite Proxies
Date: 2015-11-16
Category: Articles
Tags: computers, security, privacy
Summary: What they are, how they work, how they can be useful, and how you can use one yourself

### Abstract

In computer networks, a proxy server acts as an intermediary for requests between a client
and a server. Generally, proxies serve as load reducing agents by caching frequently
accessed web materials. Their access to message contents puts them in a unique position to
provide anonymity on the web. This document will explain the mechanisms behind general
purpose proxies and how their more advanced, anonymous counterparts can be used for
privacy.


### Review of HTTP

*HTTP* (Hyper Text Transfer Protocol) requests for web pages constitute the majority of
internet traffic. Requests for web pages are made through *HTTP GET*, responses through *HTTP
POST*. The protocol sits on top of *TCP* (Transmission Control Protocol), making the
transactions reliable in that clients and servers can be sure that all the data they send
arrives at its destination. All HTTP messages include a header. For this discussion, the
important fields are the following:

- **HTTP_Remote_Addr:** 
IP of the machine originally making the request. This is a user’s IP address, or that of their router.
- **HTTP_Via:**
A list of all web proxies that the HTTP request was forwarded through. When no proxies are used, this field is left blank.
- **HTTP_X_Forwaded_For:**
IP of the machine behind a proxy server that originally made the request. Not required by the official HTTP specifications, but is widely used anyways. Like *HTTP_Via*, this field is left blank or omitted if a proxy isn’t used.
- **HTTP_User-Agent:**
The client’s browser, window size, operating system, language, and other miscellaneous data. This information provides what’s needed to tailor web pages to different browser specifications or mobile devices.


### Proxy Servers

Web proxies use HTTP, and are generally used for reducing network load. All major proxy
software supports caching; this allows duplicate requests to be answered by the proxy
itself instead of going back to the original source of the data. This reduces the load on
the server fulfilling requests and the amount of traffic going into and out of the local
network.


In a restricted environment like a business, proxies allow administrators to limit the use
of the network to pre-approved sites and content. Even without imposing restrictions, all
traffic through the proxy can be logged. On the other hand, use of third party proxies can
allow users in restricted environments to access resources that would be otherwise blocked
by a firewall or other network filtering. This is possible when the connection to the
proxy server is unrestricted, which then requests the data on your behalf, and returns it
to you. The entire interaction from the network filter or firewall’s perspective is
between you and the proxy, not the restricted material.


### Transparent, Anonymous and Elite Proxy Servers

The *Remote_Addr*, *X_Forwarded_For*, *Via*, and *User-Agent* fields in the HTTP header
allow a server to determine exactly who is making requests. **Transparent proxy servers**
update the *Via* and *Forwarded_For* fields with their IP address, as per the official
HTTP specifications. This allows the end-point server to determine who the request
originated from, that it was routed through a proxy, and potentially deny access to
materials based on IP address (which can reveal geolocation). This type constitutes the
vast majority of web proxies and often includes caching, load-reducing proxies.  It’s
transparent in the sense that no data about the client is being hidden or manipulated.


In comparison, an **anonymous proxy** overwrites the *Remote_Addr* field with it’s own
IP address. However, they update the Via field which reveals that a proxy is in use. To
remain consistent with the *Via* field’s use, the *Forwarded_For* field is typically
filled with a random IP address. Thus the client’s real IP address is hidden, but it’s
clear on closer inspection that header manipulation has taken place because the
*Remote_Addr* and *Forwarded_For* fields do not match. Also, it appears that the proxy
is forwarding requests from itself, which is a violation of the HTTP standards. Some
secure websites check for header manipulation; the majority of such traffic comes from
malicious web bots and scrapers trying to hide their origin. Once mistakenly flagged, a
client using an anonymous proxy may be denied access to some websites. In addition, many
free anonymous proxies are known to be malicious in that they log your real HTTP header
information, as well as general web activity, to sell to others. 


 Lastly, an **elite proxy** overwrites all fields, making it appear that the proxy is
 generating all traffic as a client, without giving away that it’s a proxy to begin with.
 It blanks out or denies access to the *Forwarded_For* and *Via* fields, which denote whether
 the traffic has come through a proxy, and replaces the *Remote_Addr* address with it’s own
 IP address. By these means, the client’s IP address is protected and the end-point server
 has no way to determine that a proxy is being used. Depending on the quality of the
 header manipulation, some websites may still block access under the assumptions that the
 client is a bot or malicious, as was the case with an anonymous proxy.


 It’s important to note that the *Remote_Addr* and *Forwarded_For* fields are not the
 only ways for a snooping server to determine who you really are. The *User-Agent* field
 provides a potentially unique fingerprint of your machine; as well as a list of plugins
 that can be used to gather further information. Adobe Flash Player and JavaScript can be
 used to extract information from your browser to determine who and where you are in spite
 of the use of an elite proxy. These exploits can be as simple as client-side JavaScript
 that asks your browser for it’s real IP address, and then relays that information back to
 a remote server. Even the best elite proxies can’t do anything to stop JavaScript or
 Adobe Flash Player, but they do replace the *User-Agent* field with generic information.
 Manually disabling plugins unless you need them and the use of an elite proxy server make
 a fairly robust defense against snooping.


### Review

|             | HTTP_ Remote_Addr&nbsp;&nbsp; | HTTP_ Via&nbsp;&nbsp; | HTTP_X_ Forwarded_For&nbsp;&nbsp; | User-Agent&nbsp;&nbsp;|
|-------------|-------------------|-----------|-----------------------|-----------|
| No Proxy    | Client            |          -|                      -|     Client|
| Transparent |             Client|      Proxy|                 Client|     Client|
| Anonymous   |              Proxy|      Proxy|                 Random|     Client|
| Elite       |              Proxy|          -|                      -|      Proxy|


### An aside on VPNs

Virtual Private Networks are a direct competitor with elite proxies. They provide full end
to end encryption and typically route all traffic leaving your computer, not just HTTP
traffic. On the other hand, proxies are significantly less computationally intensive to
use (since there isn’t any encryption), for both the proxy server and client, and they
require very little configuration on the client side. Almost all devices support proxy
forwarding, the same cannot be said for VPN support. Elite proxies provide a fast,
inexpensive, and easy to configure alternative.


### How to do it yourself

You can host your own elite proxy server by using the open-source software provided by
Squid3. The basics are simple, install the software from a repository or from source on a
Linux server, then configure the squid3.conf file. The software is highly flexible and
configurable, with excellent documentation on their website. As an example, a basic elite
proxy configuration is provided on the next page. True anonymity cannot be obtained with
even an elite proxy, but it goes a long way in the right direction without sacrificing
efficiency or making the client’s configuration complex. Elite proxies provide a
lightweight, powerful alternative to VPNs for anonymity on the web.


### References
1. [W3 HTTP/1.1 Header Definitions, RFC 2616 Fielding, et al](http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html)
2. [IP Checker Field Descriptions, JonDynm](http://ip-check.info/description.php)
3. [Understanding User Agent Strings, Microsoft](https://msdn.microsoft.com/en-us/library/ms537503(v=vs.85).aspx)
4. [Squid3 Documentation, Squid3 Team]( http://www.squid-cache.org/Doc/config/ )
5. [Proxy Servers, Wikipedia](http://en.wikipedia.org/wiki/Proxy_server )

### /etc/squid3/squid.conf
        # Allow anyone with the IP address and port of the server to connect
        http_access                 allow all
        http_port                   19780 transparent


        # Remove forwarded_for, replace identifying data with generic information
        forwarded_for               off
        header_replace              Accept                 */*
        header_replace              Accept-Encoding         *
        header_replace              Accept-Language        en-us
        header_replace              User-Agent             Mozilla/6.0 (Windows NT 6.2;\WOW64; rv:16.0.1) Gecko/20121011 Firefox/16.0.1


        # Block requests for further header information
        request_header_access From                         deny all
        request_header_access Server                       deny all
        request_header_access Link                         deny all
        request_header_access Via                          deny all
        request_header_access X-Forwarded-For              deny all
        request_header_access Referer                      deny all


        # Crash and caching parameters
        coredump_dir         /var/spool/squid3


        refresh_pattern         ^ftp:                             1440  	20%         10080
		refresh_pattern         ^gopher:                          1440      0%          1440
		refresh_pattern         -i (/cgi-bin/|\?)                 0         0%          0        
		refresh_pattern         (Release|Packages(.gz)*)$         0         20%         2880
		refresh_pattern         .                                 0         20%         4320
