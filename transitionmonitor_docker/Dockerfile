FROM 2dii/r-packages@sha256:2e99aaf36141cade6bc6f51023dfdf6dba87c464ee17a062207a37707553d69a

COPY pacta-data /pacta-data
COPY r2dii.stress.test.data /r2dii.stress.test.data
COPY create_interactive_report /create_interactive_report
COPY r2dii.climate.stress.test /r2dii.climate.stress.test
COPY PACTA_analysis /bound

RUN chmod -R a+rwX \
      /bound \
      /create_interactive_report \
      /pacta-data \
      /r2dii.climate.stress.test \
      /r2dii.stress.test.data

RUN echo "options(tinytex.tlmgr.path = '/root/.TinyTeX/bin/x86_64-linux/tlmgr')" > .Rprofile
RUN chmod -R a+rwX /root
RUN /root/.TinyTeX/bin/*/tlmgr path add

RUN exit 0
