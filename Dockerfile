FROM rocker/shiny-verse

RUN R -e 'install.packages("markdown")'

RUN rm -r /srv/shiny-server/*
COPY ./* /srv/shiny-server/

USER shiny
EXPOSE 3838

ENV APPLICATION_LOGS_TO_STDOUT=true
ENV SHINY_LOG_STDERR=1
CMD ["/usr/bin/shiny-server"]