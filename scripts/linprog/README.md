# Installation

```sh
mkvirtualenv --python=`which python3.6` linprog
workon linprog
add2virtualenv $HOME/libs/cplex_studio129/cplex/python/3.6/x86-64_linux
add2virtualenv $HOME/libs/z3/bin/python
pip3 install -r requirements.txt
```
