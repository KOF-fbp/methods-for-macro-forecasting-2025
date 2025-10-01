FROM mambaorg/micromamba:2.3.0 

# Copy and install from environment file 
COPY --chown=$MAMBA_USER:$MAMBA_USER env.yaml /tmp/env.yaml 
RUN micromamba clean --all --yes
RUN micromamba install -y -n base -f /tmp/env.yaml

# Activate the environment for subsequent commands
ARG MAMBA_DOCKERFILE_ACTIVATE=1

# Set project root
WORKDIR /app

COPY --chown=$MAMBA_USER:$MAMBA_USER submission/renv/ /app/renv/
COPY --chown=$MAMBA_USER:$MAMBA_USER submission/renv.lock /app/renv.lock
COPY --chown=$MAMBA_USER:$MAMBA_USER submission/.Rprofile /app/.Rprofile
RUN Rscript -e "install.packages('renv', repos='https://cloud.r-project.org')"
RUN Rscript -e "renv::restore(project = '/app')"

# Copy files
COPY --chown=$MAMBA_USER:$MAMBA_USER submission/*.R /app/
COPY --chown=$MAMBA_USER:$MAMBA_USER ./.check_renv.R /app/

USER root
RUN mkdir -p /app/output && chown -R mambauser:mambauser /app/output
USER $MAMBA_USER

# Activate coda env
CMD ["/usr/local/bin/_entrypoint.sh", "Rscript", "/app/main.R"]
