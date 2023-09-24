top_srcdir ?= ..
srcdir = $(top_srcdir)/tests
top_builddir ?= ..
builddir = $(top_builddir)/tests

MODULES_RUN=basic_run cobol_data_run sql_data_run sqlca_run misc_run
MODULES=$(srcdir)/basic $(srcdir)/cobol_data $(srcdir)/sql_data $(srcdir)/sqlca $(srcdir)/misc
LOCAL_HELPERS=atlocal embed_db_info.sh cobol_runner.sh

.PHONY: clean distclean all help $(MODULES_RUN) check

all: $(MODULES)

help:
	@cat README

clean:
	rm -rf *.dir *.log

distclean: clean
	rm -f $(MODULES)

check: $(MODULES) atlocal embed_db_info.sh cobol_runner.sh
	make -f $(srcdir)/Makefile -k $(MODULES_RUN)
	cat *.log
	[ -z "$(cat *.log | grep 'Failed tests:')" ]

$(LOCAL_HELPERS):
	if test -f $@; then \
		touch $@ ; \
	else \
		cp -p $(top_srcdir)/.github/workflows/ubuntu-test-settings/$@ . ; \
	fi

.SUFFIXES: .at

basic_run: $(srcdir)/basic
	$(srcdir)/basic
cobol_data_run: $(srcdir)/cobol_data
	$(srcdir)/cobol_data
sql_data_run: $(srcdir)/sql_data
	$(srcdir)/sql_data
sqlca_run: $(srcdir)/sqlca
	$(srcdir)/sqlca
misc_run: $(srcdir)/misc
	$(srcdir)/misc

#.at: package.m4 atlocal.in
.at: package.m4
	cd $(srcdir)
	autom4te --language=autotest -I $@.src -o $@ $@.at
	chmod +x $@

sql_data.src/sql_type.at: generate_testcase_sql_type.py
	python3 generate_testcase_sql_type.py
