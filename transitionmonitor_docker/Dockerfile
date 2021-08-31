FROM 2dii/r-packages:latest

COPY PACTA_analysis /bound
COPY create_interactive_report /create_interactive_report
COPY pacta-data /pacta-data
COPY r2dii.climate.stress.test /r2dii.climate.stress.test
COPY r2dii.stress.test.data /r2dii.stress.test.data

RUN chmod -R a+rwX /bound
RUN chmod -R a+rwX /create_interactive_report
RUN chmod -R a+rwX /pacta-data
RUN chmod -R a+rwX /r2dii.climate.stress.test
RUN chmod -R a+rwX /r2dii.stress.test.data

RUN echo "options(tinytex.tlmgr.path = '/root/.TinyTeX/bin/x86_64-linux/tlmgr')" > .Rprofile
RUN chmod -R a+rwX /root
RUN /root/.TinyTeX/bin/*/tlmgr path add

RUN exit 0
