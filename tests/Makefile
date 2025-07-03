top_srcdir ?= ..
srcdir = $(top_srcdir)/tests
top_builddir ?= ..
builddir = $(top_builddir)/tests

MODULES_RUN=basic_run cobol_data_run sql_data_run sqlca_run misc_run
MODULES=$(srcdir)/basic $(srcdir)/cobol_data $(srcdir)/sql_data $(srcdir)/sqlca $(srcdir)/misc
LOCAL_HELPERS=atlocal embed_db_info.sh cobol_runner.sh

TESTS = basic \
	cobol_data \
	sql_data \
	sqlca \
	misc

basic_DEPENDENCIES = \
	basic.src/commit-rollback.at \
	basic.src/connect-disconnect.at \
	basic.src/declare.at \
	basic.src/delete.at \
	basic.src/fetch.at \
	basic.src/include.at \
	basic.src/insert.at \
	basic.src/open-close.at \
	basic.src/sample.at \
	basic.src/select.at \
	basic.src/update.at

cobol_data_DEPENDENCIES = \
	cobol_data.src/alphanumeric.at \
    cobol_data.src/comp5.at \
	cobol_data.src/japanese.at \
	cobol_data.src/numeric_signed_trailing_v.at \
	cobol_data.src/numeric_signed_v.at \
	cobol_data.src/numeric_v.at \
	cobol_data.src/varying.at

sql_data_DEPENDENCIES = \
	sql_data.src/sql_type.at

sqlca_DEPENDENCIES = \
	sqlca.src/connect-disconnect.at \
	sqlca.src/insert-select-update-delete.at \
	sqlca.src/open-fetch-close.at \
	sqlca.src/prepare-execute.at

misc_DEPENDENCIES = \
	misc.src/compile.at \
	misc.src/empty-string.at \
	misc.src/quiet.at

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

sql_data.src/sql_type.at: generate_testcase_sql_type.py
	python3 generate_testcase_sql_type.py

clean-local:
	rm -rf $(TESTS)

test: $(TESTS)
	@for TEST_SCRIPT in $(TESTS); do \
		./$${TEST_SCRIPT}; \
	done

$(srcdir)/package.m4: $(top_srcdir)/configure.ac
	@echo "creating $(srcdir)/package.m4"
	@{								   \
	  echo '# Signature of the current package.';			   \
	  echo 'm4_define([AT_PACKAGE_NAME],	  [@PACKAGE_NAME@])';	   \
	  echo 'm4_define([AT_PACKAGE_TARNAME],	  [@PACKAGE_TARNAME@])';   \
	  echo 'm4_define([AT_PACKAGE_VERSION],	  [@PACKAGE_VERSION@])';   \
	  echo 'm4_define([AT_PACKAGE_STRING],	  [@PACKAGE_STRING@])';    \
	  echo 'm4_define([AT_PACKAGE_BUGREPORT], [@PACKAGE_BUGREPORT@])'; \
	} > $(srcdir)/package.m4

$(srcdir)/%: $(srcdir)/%.at $(srcdir)/package.m4
	autom4te --language=autotest -I $(srcdir) -I $@.src -o $@ $<
	chmod +x $@

$(srcdir)/basic: $(basic_DEPENDENCIES)
$(srcdir)/cobol_data: $(cobol_data_DEPENDENCIES)
$(srcdir)/sql_data: $(sql_data_DEPENDENCIES)
$(srcdir)/sqlca: $(sqlca_DEPENDENCIES)
$(srcdir)/misc: $(misc_DEPENDENCIES)
