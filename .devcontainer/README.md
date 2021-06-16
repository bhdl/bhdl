# BHDL docker images

This folder contains Dockerfile specifying images for developing and deploying
BHDL.

To use:

```
docker-compose up -d --build
``

Or you can use the pre-built images available (TODO) at https://hub.docker.com/r/bhdl/bhdl. The docker-compose file specified three images:

- bhdl (port 8888): the jupyterlab interface to develop BHDL boards
- placer (inner port 8082): the placerment server
- placer-dev (port 8889): the jupyterlab interface for developing the placer itself

You could modify the volumes mounting options. The default behavior is to mount the parent directory (BHDL root) as `/root/bhdl` in the container.