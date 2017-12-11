FROM continuumio/miniconda3

ARG UID
ARG GID

SHELL ["/bin/bash", "-c"]

# RUN groupadd -r flapydisaster && useradd -r -g flapydisaster flapydisaster
RUN conda update conda

RUN groupadd -g $GID flapydisaster
RUN useradd -u $UID -g $GID -ms /bin/bash flapydisaster

USER flapydisaster

WORKDIR /home/flapydisaster

# RUN conda update conda
RUN conda create -n FlaPyEnv python=3.5

COPY ../conda_requirements_explicit_linux.txt /src/conda_requirements_explicit_linux.txt

RUN source activate FlaPyEnv && conda install --file /src/conda_requirements_explicit_linux.txt

RUN git clone -b dockerIntergration --single-branch  https://github.com/cliftbar/FlaPyDisaster.git

# ADD --chown=flapydisaster:flapydisaster user_config.ini FlaPyDisaster/FlaPyDisaster/users/user_config

WORKDIR /home/flapydisaster/FlaPyDisaster/FlaPyDisaster

# ENTRYPOINT [ "/bin/bash", "-c" ]

CMD source activate FlaPyEnv && python app.py