---
title: How to set up NTUVPN on Ubuntu
date: 2013-03-06 09:17:00
tags: ntu, vpn, setup, ubuntu
---
_For Ubuntu users, the NTU's [official instruction](http://www.ntu.edu.sg/cits/itnetworking/remoteaccess/Pages/quickstartguide.aspx#sslvpn) to setup a [VPN](http://en.wikipedia.org/wiki/Vpn "Virtual private network") client requires them to install a java plugin in order to connect to its server. However, the client can only run on a 32-bit JVM, and even if it is running on one, the client seems to hang. Therefore, to save you from all these hassles, this post will help you set up a VPN client that runs natively on Ubuntu._

## Installing the required package ##

Firstly, you need to install the [PPTP](http://en.wikipedia.org/wiki/Pptp) plugin for Network Manager, which provides a PPTP VPN plugin to connect to a Microsoft VPN server.

Open up your Terminal and type in

```bash
sudo apt-get install network-manager-pptp
```

Enter your password if required and wait until it finishes installing.

## Setting up a VPN connection ##

Click on the Network Manager icon, select `VPN Connections` and choose `Configure VPN...`

In the `Choose a VPN Connection Type` dialog, select `Point-to-Point Tunneling Protocol (PPTP)` and click `Create`

In the newly opened dialog, type in the following:

* `Connection name:` &rarr; `NTUVPN`
* `Gateway:` &rarr; `vpngate.ntu.edu.sg`
* `User name:` &rarr; your NTU user name, prefixed by `STUDENT\`, e.g. `STUDENT\name1`
* `Password:` &rarr; your password

Click on `Advanced...`, tick `Use Point-to-Point encryption (MPPE)`, and click `OK`

Finally, click `Save...`

## Connect to NTUVPN ##

Click on the Network Manager icon, select `VPN Connections` and choose the newly created `NTUVPN`. The connection should be established now.

## Disconnect from NTUVPN ##

To disconnect from NTUVPN, click on the Network Manager icon, select `VPN Connections` and choose `Disconnect VPN`
