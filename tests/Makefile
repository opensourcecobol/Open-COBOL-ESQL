MODULES=basic cobol_data sql_data sqlca

.PHONY: clean all help

all: $(MODULES)

clean:
	rm -f $(MODULES)

help:
	@cat README

.SUFFIXES: .at

#.at: package.m4 atlocal.in
.at: package.m4
	autom4te --language=autotest -I $@.src -o $@ $@.at
	chmod +x $@

sql_data.src/sql_type.at: generate_testcase_sql_type.py
	python3 generate_testcase_sql_type.py
