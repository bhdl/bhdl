# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.177.0/containers/ubuntu/.devcontainer/base.Dockerfile

# [Choice] Ubuntu version: bionic, focal
# ARG VARIANT="focal"
# FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}

FROM nvidia/cuda:10.1-devel-ubuntu18.04
LABEL maintainer="Hebi Li <hebi@lihebi.com>"

# [Optional] Uncomment this section to install additional OS packages.
# RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
#     && apt-get -y install --no-install-recommends <your-package-list-here>

RUN chmod 1777 /tmp && chmod 1777 /var/tmp
ARG NB_USER="jovyan"
ARG NB_UID="1000"
ARG NB_GID="100"
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
USER root


# Install all OS dependencies for notebook server that starts but lacks all
# features (e.g., download as all possible file formats)
ENV DEBIAN_FRONTEND noninteractive


RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
    wget curl \
    bzip2 \
    ca-certificates \
    sudo \
    locales \
    fonts-liberation \
    run-one \
    build-essential \
    git tmux \
    vim emacs

RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen && \
    locale-gen

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends\
    jed \
    libsm6 \
    libxext-dev \
    libxrender1 \
    lmodern \
    netcat \
    python-dev \
    tzdata \
    unzip \
    nano \
    iputils-ping iproute2 htop locate libzmq5\
    silversearcher-ag


# Using Ubuntu, install nodejs
# https://github.com/nodesource/distributions/blob/master/README.md#debinstall
RUN curl -fsSL https://deb.nodesource.com/setup_16.x | sudo -E bash -
RUN apt-get update && \
    apt-get install -y --no-install-recommends nodejs


# Configure environment
ENV CONDA_DIR=/opt/conda \
    SHELL=/bin/bash \
    NB_USER=$NB_USER \
    NB_UID=$NB_UID \
    NB_GID=$NB_GID \
    LC_ALL=en_US.UTF-8 \
    LANG=en_US.UTF-8 \
    LANGUAGE=en_US.UTF-8
ENV PATH=$CONDA_DIR/bin:$PATH \
    HOME=/home/$NB_USER

# Copy a script that we will use to correct permissions after running certain commands
COPY fix-permissions /usr/local/bin/fix-permissions
RUN chmod a+rx /usr/local/bin/fix-permissions


# Enable prompt color in the skeleton .bashrc before creating the default NB_USER
RUN sed -i 's/^#force_color_prompt=yes/force_color_prompt=yes/' /etc/skel/.bashrc

# Create NB_USER wtih name jovyan user with UID=1000 and in the 'users' group
# and make sure these dirs are writable by the `users` group.
RUN echo "auth requisite pam_deny.so" >> /etc/pam.d/su && \
    sed -i.bak -e 's/^%admin/#%admin/' /etc/sudoers && \
    sed -i.bak -e 's/^%sudo/#%sudo/' /etc/sudoers && \
    useradd -m -s /bin/bash -N -u $NB_UID $NB_USER && \
    mkdir -p $CONDA_DIR && \
    chown $NB_USER:$NB_GID $CONDA_DIR && \
    chmod g+w /etc/passwd && \
    fix-permissions $HOME && \
    fix-permissions $CONDA_DIR

ARG PYTHON_VERSION=default

# Install conda as jovyan and check the md5 sum provided on the download site
ENV MINICONDA_VERSION=4.8.3 \
    MINICONDA_MD5=d63adf39f2c220950a063e0529d4ff74 \
    CONDA_VERSION=4.8.3


WORKDIR /tmp
RUN wget --quiet https://repo.continuum.io/miniconda/Miniconda3-py38_${MINICONDA_VERSION}-Linux-x86_64.sh && \
    echo "${MINICONDA_MD5} *Miniconda3-py38_${MINICONDA_VERSION}-Linux-x86_64.sh" | md5sum -c - && \
    /bin/bash Miniconda3-py38_${MINICONDA_VERSION}-Linux-x86_64.sh -f -b -p $CONDA_DIR && \
    rm Miniconda3-py38_${MINICONDA_VERSION}-Linux-x86_64.sh && \
    echo "conda ${CONDA_VERSION}" >> $CONDA_DIR/conda-meta/pinned && \
    conda config --system --prepend channels conda-forge && \
    conda config --system --set auto_update_conda false && \
    conda config --system --set show_channel_urls true && \
    conda config --system --set channel_priority strict && \
    if [ ! $PYTHON_VERSION = 'default' ]; then conda install --yes python=$PYTHON_VERSION; fi && \
    conda list python | grep '^python ' | tr -s ' ' | cut -d '.' -f 1,2 | sed 's/$/.*/' >> $CONDA_DIR/conda-meta/pinned && \
    conda install --quiet --yes conda && \
    conda install --quiet --yes pip && \
    conda update --all --quiet --yes && \
    conda clean --all -f -y && \
    rm -rf /home/$NB_USER/.cache/yarn && \
    fix-permissions $CONDA_DIR && \
    fix-permissions /home/$NB_USER


# Install Tini
RUN conda install --quiet --yes 'tini=0.18.0' && \
    conda list tini | grep tini | tr -s ' ' | cut -d ' ' -f 1,2 >> $CONDA_DIR/conda-meta/pinned && \
    conda clean --all -f -y && \
    fix-permissions $CONDA_DIR && \
    fix-permissions /home/$NB_USER

# Install Jupyter Notebook, Lab, and Hub
# Generate a notebook server config
# Cleanup temporary files
# Correct permissions
# Do all this in a single RUN command to avoid duplicating all of the
# files across image layers when the permissions change
#
# UPDATE upgrading to jupyterlab v3
# 'notebook=6.1.3' \
#     'jupyterhub=1.1.0' \
#     'jupyterlab=2.2.5' && \
#
# NOTE: Official jupyterlab
# conda install -c conda-forge jupyterlab
RUN conda install --quiet --yes jupyterlab && \
    conda clean --all -f -y && \
    # FIXME why npm here? npm is not installed thus caused error
    # npm cache clean --force && \
    jupyter notebook --generate-config && \
    rm -rf $CONDA_DIR/share/jupyter/lab/staging && \
    rm -rf /home/$NB_USER/.cache/yarn && \
    fix-permissions $CONDA_DIR && \
    fix-permissions /home/$NB_USER


# Install racket

ENV RACKET_VERSION=8.1
ENV RACKET_INSTALLER_URL=http://mirror.racket-lang.org/installers/${RACKET_VERSION}/racket-${RACKET_VERSION}-x86_64-linux-natipkg.sh
RUN wget --output-document=racket-install.sh -q ${RACKET_INSTALLER_URL} && \
    echo "yes\n1\n" | sh racket-install.sh --create-dir --unix-style --dest /usr/ && \
    rm racket-install.sh

# RUN apt-get install -y software-properties-common &&\
#     add-apt-repository ppa:plt/racket &&\
#     apt-get update && \
#     apt-get install -y racket

RUN git clone https://github.com/lihebi/iracket /tmp/iracket && \
    cd /tmp/iracket && git checkout dev &&\
    raco pkg install --auto &&\
    raco iracket install


ENV TINI_VERSION v0.19.0
ADD https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini /tini
RUN chmod +x /tini

# Julia
ENV JULIA_VERSION=1.6.1
WORKDIR /tmp

RUN mkdir "/opt/julia-${JULIA_VERSION}" && \
    wget -q https://julialang-s3.julialang.org/bin/linux/x64/$(echo "${JULIA_VERSION}" | cut -d. -f 1,2)"/julia-${JULIA_VERSION}-linux-x86_64.tar.gz" && \
    tar xzf "julia-${JULIA_VERSION}-linux-x86_64.tar.gz" -C "/opt/julia-${JULIA_VERSION}" --strip-components=1 && \
    rm "/tmp/julia-${JULIA_VERSION}-linux-x86_64.tar.gz"
RUN ln -fs /opt/julia-*/bin/julia /usr/local/bin/julia

##############################
RUN fix-permissions /home/$NB_USER
USER $NB_UID



# UPDATE I don't need these. I would just install per-user, and those persist
# with the mounted volume.  Even if these are set, they will be removed.
#
# CAUTION this is slow
RUN julia -e 'import Pkg; Pkg.update()' && \
    (test $TEST_ONLY_BUILD || julia -e 'import Pkg; Pkg.add("HDF5")') && \
    julia -e "using Pkg; pkg\"add IJulia\"; pkg\"precompile\""


# TODO clone bhdl repo?
# bhdl footprints
ENV BHDL_LIBRARY_PATH=$HOME/bhdl-footprints
RUN git clone --recursive https://github.com/bhdl/bhdl-footprints $BHDL_LIBRARY_PATH


USER $NB_UID
# jupyterlab extensions
RUN pip install --upgrade jupyterlab-git
RUN jupyter labextension install --no-build @jupyterlab/toc
RUN jupyter labextension install --no-build @ijmbarr/jupyterlab_spellchecker
RUN jupyter labextension install --no-build @aquirdturtle/collapsible_headings
# RUN pip install git+https://github.com/lihebi/jupyterlab-racket

# can use either one of the following to install
#
# FIXME jupyter labextension install is not working properly
# RUN jupyter labextension install --no-build https://github.com/lihebi/jupyterlab-racket
RUN pip install git+https://github.com/lihebi/jupyterlab-racket
# RUN jupyter lab build

# This is used in start.sh
# ENV GRANT_SUDO=1

USER root
RUN echo "$NB_USER ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/notebook



USER $NB_UID

# Files
# COPY start.sh start-notebook.sh start-singleuser.sh /usr/local/bin/
COPY start.sh /usr/local/bin/
COPY jupyter_notebook_config.py /etc/jupyter/

USER root
RUN chmod a+rx /usr/local/bin/start.sh
USER $NB_UID

EXPOSE 8888
WORKDIR $HOME

# TODO install jupyter

ENTRYPOINT ["/tini", "-g", "--"]
CMD [ "/usr/local/bin/start.sh", "jupyter", "lab" ]
