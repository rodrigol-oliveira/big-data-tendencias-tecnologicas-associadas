FROM colares07/docker-ln-plumber:latest

COPY ./src /usr/local/src/scripts

WORKDIR /usr/local/src/scripts

RUN echo "source activate LN" > ~/.bashrc
ENV PATH /opt/conda/envs/LN/bin:$PATH

RUN conda install --name LN -c conda-forge r-curl 
RUN conda install --name LN -c conda-forge r-httr 
RUN conda install --name LN -c conda-forge r-xml2
RUN conda install --name LN -c conda-forge r-digest
RUN conda install --name LN -c conda-forge r-base64enc
RUN conda install --name LN -c conda-forge r-aws.s3

ENTRYPOINT []
CMD ["Rscript", "run_server.R"]