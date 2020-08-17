# BHDL Developer and Administrator Guide

## Install for development

For development purpose, you might want to clone the repo and install the cloned
local package. This is known as "linking":

```
git clone https://
cd bhdl-lib
raco pkg install
```

This will place a link between the installed packages and this local
directory. Everytime this local code is modified, the update is immediately
available for the package `bhdl`.

## Jupyter Hub server

For demo purpose, it is desired to setup BHDL on a server, so that users can
just login to the server and run BHDL directly without the hassle of
installation. Also, the auto-placement engine requires GPU to run efficiently,
which may not be available for users' own machines. This section describes the
general process to setup a jupyterhub server capable of running BHDL.

### Install the jupyterhub

This section mostly follows [the official jupyterhub
guide](https://jupyterhub.readthedocs.io/en/latest/installation-guide-hard.html). It
is simplied a little (removed the conda part) and noted here for quick reference.


Create a virtual environment for jupyterhub:

    sudo python3 -m venv /opt/jupyterhub/

Install some packages. Note that if you want install any future packages for
jupyterhub server, like an extension, you must use this executable and install
it globally.

    sudo /opt/jupyterhub/bin/python3 -m pip install wheel
    sudo /opt/jupyterhub/bin/python3 -m pip install jupyterhub jupyterlab
    sudo /opt/jupyterhub/bin/python3 -m pip install ipywidgets

Some nodejs dependencies:

    sudo apt install nodejs npm
    sudo npm install -g configurable-http-proxy

Generate configuration:

    sudo mkdir -p /opt/jupyterhub/etc/jupyterhub/
    cd /opt/jupyterhub/etc/jupyterhub/
    sudo /opt/jupyterhub/bin/jupyterhub --generate-config

change one config in `jupyterhub_config.py`:

    c.Spawner.default_url = '/lab'

Add system service so that it can be started as a daemon and auto-start on boot:

    sudo mkdir -p /opt/jupyterhub/etc/systemd
    /opt/jupyterhub/etc/systemd/jupyterhub.service
    sudo ln -s /opt/jupyterhub/etc/systemd/jupyterhub.service /etc/systemd/system/jupyterhub.service

the service file content:

    [Unit]
    Description=JupyterHub
    After=syslog.target network.target

    [Service]
    User=root
    Environment="PATH=/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/opt/jupyterhub/bin"
    ExecStart=/opt/jupyterhub/bin/jupyterhub -f /opt/jupyterhub/etc/jupyterhub/jupyterhub_config.py

    [Install]
    WantedBy=multi-user.target

enable and start it:

    sudo systemctl daemon-reload
    sudo systemctl enable jupyterhub.service
    sudo systemctl start jupyterhub.service
    sudo systemctl status jupyterhub.service

The default port is `8000`, and it seems also to use `8001`. Chaning 8000 in
configuration does not change 8001, which in turns seems to make it unable to
start two instances of jupyterhub easily.

### The iracket kernel

```
sudo apt install libzmq5
git clone https://github.com/lihebi/iracket
cd iracket && git checkout dev
raco pkg install
raco iracket install
```

### Install BHDL and start the placement engine
Install BHDL:

```
raco pkg install git://github.com/lihebi/bhdl/?path=bhdl-lib
```

Start placement engine (and this will listen on port 8082 and shared for all
users):


```
git clone https://github.com/lihebi/bhdl
cd bhdl/placement
# NOTE: Need install Julia dependencies via:
# julia --project
# Julia> ]instantiate
julia --project server.jl
```


#### (Optional) Clone the bhdl libraries:

```
git clone --recursive https://github.com/lihebi/bhdl-footprints /opt/bhdl-footprints
export BHDL_LIBRARY_PATH=/opt/bhdl-footprints
```

Users probably should set up their own libraries in their home directory,
because the easyeda library is written to disk if not available locally, thus users need write access to the library.
