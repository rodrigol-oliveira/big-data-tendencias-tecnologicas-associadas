FROM colares07/docker-ln-plumber:latest

COPY ./src /usr/local/src/scripts

WORKDIR /usr/local/src/scripts

RUN echo "source activate LN" > ~/.bashrc
ENV PATH /opt/conda/envs/LN/bin:$PATH

ENTRYPOINT []
CMD ["Rscript", "run_server.R"]