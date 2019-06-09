FROM uribo/tokyor74

## Declares build arguments
ARG NB_USER
ARG NB_UID

# Copy repo into ${HOME}, make user own $HOME
USER root
COPY . ${HOME}
COPY binder ${HOME}
RUN chown -R ${NB_USER} ${HOME}


#COPY . ${HOME}
#RUN chown -R ${NB_USER} ${HOME}
#USER ${NB_USER}

## Run an install.R script, if it exists.
RUN if [ -f install.R ]; then R --quiet -f install.R; fi
