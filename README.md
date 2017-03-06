Link to original [README](README)

# Instructions for compiling Rosette using Vagrant (Ubuntu 16.04)

1. cd Rosette-VM/
2. [vagrant](https://www.vagrantup.com/) up
3. vagrant ssh
4. cd /vagrant/
5. sudo apt update && sudo apt upgrade
6. sudo apt install g++ g++-multilib
7. make

Note the make command invokes the rbl.

```
$ lsb_release -a
No LSB modules are available.
Distributor ID:	Ubuntu
Description:	Ubuntu 14.04.5 LTS
Release:	14.04
Codename:	trusty
```
