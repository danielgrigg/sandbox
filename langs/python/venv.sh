pyvenv env
source env/bin/activate
#(env33)$ which python3.3

curl -O http://python-distribute.org/distribute_setup.py
python distribute_setup.py

easy_install pyramid nose webtest deform sqlalchemy
