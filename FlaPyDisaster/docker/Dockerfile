FROM continuumio/miniconda3

SHELL ["/bin/bash", "-c"]

# RUN groupadd -r flapydisaster && useradd -r -g flapydisaster flapydisaster
RUN conda update conda

# RUN groupadd -g 1000 flapydisaster
# RUN useradd -u 1000 -g 1000 -ms /bin/bash flapydisaster
# 
# USER flapydisaster

# WORKDIR /home/flapydisaster

# RUN conda update conda
RUN conda create -n FlaPyEnv python=3.5

COPY conda_requirements_explicit_linux.txt /src/conda_requirements_explicit_linux.txt

RUN source activate FlaPyEnv && conda install --file /src/conda_requirements_explicit_linux.txt

ENTRYPOINT [ "/bin/bash", "-c" ]

CMD [ "/bin/bash" ]