# Instructions for compiling Rosette using Vagrant (Ubuntu 16.04)

1. vagrant up
2. vagrant ssh
3. cd /vagrant/
4. sudo apt update && sudo apt upgrade
5. sudo apt install g++ g++-multilib
6. make

Note the make command invokes the rbl.

```
$ lsb_release -a
No LSB modules are available.
Distributor ID:	Ubuntu
Description:	Ubuntu 14.04.5 LTS
Release:	14.04
Codename:	trusty
```