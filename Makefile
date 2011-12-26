EC = erlc
# EFLAGS = +native '+{hipe,o3}'
EFLAGS =

all: *.beam

*.beam: *.erl
	$(EC) $(EFLAGS) $^

clean:
	rm -rf *.beam

.PHONY: clean

